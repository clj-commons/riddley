(ns riddley.compiler)

(defmacro if-bb [then else]
  (if (System/getProperty "babashka.version") then else))

;; JVM-only imports
(if-bb nil
  (import '[clojure.lang Var Compiler Compiler$ObjMethod Compiler$ObjExpr]
          '[riddley Util]))

;; BB-only: dynamic var for tracking locals
(if-bb (def ^:dynamic *locals* nil) nil)

(if-bb nil
  (defn- stub-method []
    (proxy [Compiler$ObjMethod] [(Compiler$ObjExpr. nil) nil])))

(defn tag-of
  "Returns a symbol representing the tagged class of the symbol, or `nil` if none exists."
  [x]
  (when-let [tag (-> x meta :tag)]
    (let [sym (symbol
                (if (instance? Class tag)
                  (.getName ^Class tag)
                  (name tag)))]
      (when-not (= 'java.lang.Object sym)
        sym))))

(if-bb nil
  (let [n (atom 0)]
    (defn- local-id []
      (swap! n inc))))

(defn locals
  "Returns the local binding map, equivalent to the value of `&env`."
  []
  (if-bb
    *locals*
    (when (.isBound Compiler/LOCAL_ENV)
      @Compiler/LOCAL_ENV)))

(defmacro with-base-env [& body]
  (if-bb
    `(binding [*warn-on-reflection* false]
       (if *locals*
         (do ~@body)
         (binding [*locals* {}]
           ~@body)))
    `(binding [*warn-on-reflection* false]
       (with-bindings (if (locals)
                        {}
                        {Compiler/LOCAL_ENV {}})
         ~@body))))

(defmacro with-lexical-scoping
  "Defines a lexical scope where new locals may be registered."
  [& body]
  (if-bb
    `(binding [*locals* (or *locals* {})]
       ~@body)
    `(with-bindings {Compiler/LOCAL_ENV (locals)}
       ~@body)))

(if-bb nil
  (defmacro with-stub-vars [& body]
    `(with-bindings {Compiler/CLEAR_SITES nil
                     Compiler/METHOD (stub-method)}
       ~@body)))

;; if we don't do this in Java, the checkcasts emitted by Clojure cause an
;; IllegalAccessError on Compiler$Expr.  Whee.
(defn register-local
  "Registers a locally bound variable `v`, which is being set to form `x`."
  [v x]
  (if-bb
    (set! *locals* (-> *locals* (dissoc v) (assoc v x)))
    (with-stub-vars
      (.set ^clojure.lang.Var Compiler/LOCAL_ENV

        ;; we want to allow metadata on the symbols to persist, so remove old symbols first
        (-> (locals)
          (dissoc v)
          (assoc v (try
                     (riddley.Util/localBinding (local-id) v (tag-of v) x)
                     (catch Exception _
                       ::analyze-failure))))))))

(defn register-arg
  "Registers a function argument `x`."
  [x]
  (if-bb
    (set! *locals* (-> *locals* (dissoc x) (assoc x nil)))
    (with-stub-vars
      (.set ^clojure.lang.Var Compiler/LOCAL_ENV
        (-> (locals)
          (dissoc x)
          (assoc x (riddley.Util/localArgument (local-id) x (tag-of x))))))))
