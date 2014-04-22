(defproject mecha "0.1.0"
  :description "coordinate starting and stopping things in clojure"
  :url "https://github.com/justinvdm/mecha"
  :license {:name "MIT"}
  :dependencies [[org.clojure/clojure "1.5.1"]]
  :profiles {:dev {:dependencies [[speclj "3.0.2"]]}}
  :plugins [[speclj "3.0.2"]]
  :test-paths ["spec"])
