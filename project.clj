(defproject riddley "0.2.0"
  :description "code-walking without caveats"
  :license {:name "MIT License"
            :url "http://opensource.org/licenses/MIT"}
  :dependencies []
  :plugins [[lein-codox "0.9.4"]]
  :codox {:src-dir-uri "https://github.com/ztellman/riddley/tree/master/"
          :src-linenum-anchor-prefix "L"
          :defaults {:doc/format :markdown}
          :include [riddley.walk riddley.compiler]
          :output-dir "doc"}
  :profiles {:provided {:dependencies [[org.clojure/clojure "1.8.0"]]}}
  :java-source-paths ["src"]
  :javac-options ["-target" "1.6" "-source" "1.6"])
