(defproject core-async-tutorials "0.1.0-SNAPSHOT"
  :description "Examples from Clojure Tutorials - Core.Async"
  :url "https://github.com/jumarko/clojure-tutorials/tree/master/core-async-tutorials"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/core.async "0.3.442"]]
  :main ^:skip-aot core-async-tutorials.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
