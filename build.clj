(ns build
  (:require [clojure.tools.build.api :as b]))

(defn compile-java [_]
  (b/javac {:basis (b/create-basis)
            :src-dirs ["src"]
            :class-dir "target/classes"
            :javac-opts ["--release" "8"]}))
