(defproject advent-of-code "0.1.0-SNAPSHOT"
  :description "advent of code in clojure"
  :dependencies [[org.clojure/clojure "1.10.1"]]
  :main ^:skip-aot advent-of-code.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
