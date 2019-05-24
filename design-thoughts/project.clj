(defproject design-thoughts "0.1.0-SNAPSHOT"
  :description "Examples for Tim Baldridge's Clojure Tutorials - Design Thoughts series"
  :url "https://github.com/jumarko/clojure-tutorials/design-thoughts"
  :dependencies [[org.clojure/clojure "1.10.1-RC1"]]
  :main ^:skip-aot design-thoughts.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
