(ns mecha.core-spec
  (:require [speclj.core :refer :all]
            [mecha.core :refer :all]))


(defmecha mecha-foo
  (:start (println "a")
          (println "r"))
  (:stop (println "b")))


(defmecha mecha-bar [a b]
  (:start (println "c"))
  (:stop (println "d")))


(defmecha mecha-baz
  (:start [foo (mecha-foo)
           bar (mecha-bar)])
  (:stop (:name foo)))


(defmecha mecha-qux
  (:start [foo (mecha-foo)
           bar (mecha-bar)]))


(describe "mecha"
  (it "should be startable")
  (it "should be stoppable")
  (it "should start its own mecha when it is started")
  (it "should stop its own mecha when it is stopped")
  (it "should not mandate state")
  (it "should not mandate args")
  (it "should not mandate a start block")
  (it "should not mandate a stop block"))
