(ns mecha.core-spec
  (:require [speclj.core :refer :all]
            [mecha.core :refer :all]))


(defmecha mecha-foo
  (start ())
  (stop ()))


(defmecha mecha-bar []
  (start ())
  (stop ()))


(defmecha mecha-baz []
  []
  (start ())
  (stop ()))


(describe "mecha"
  (it "should be startable")
  (it "should be stoppable")
  (it "should start its own mecha when it is started")
  (it "should stop its own mecha when it is stopped")
  (it "should not mandate state")
  (it "should not mandate args"))
