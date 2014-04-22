(ns mecha.core-spec
  (:require [speclj.core :refer :all]
            [mecha.core :refer :all]))


(describe "mecha"
  (with-stubs)

  (it "should be startable"
    (let [starter (stub :starter)
          m (mecha (:start (starter)))]
      (m)
      (should-have-invoked :starter {:times 1})))

  (it "should be stoppable"
    (let [stopper (stub :stopper)
          m (mecha (:stop (stopper)))]
      (stop (m))
      (should-have-invoked :stopper {:times 1})))

  (it "should start its own mecha when it is started"
    (let [starter (stub :starter)
          m (mecha (:start [mecha (:start (starter))]))]
      (m)
      (should-have-invoked :starter {:times 1})))

  (it "should stop its own mecha when it is stopped"
    (let [stopper (stub :stopper)
          m (mecha (:start [foo 23
                            mecha (:stop (stopper))]))]
      (stop (m))
      (should-have-invoked :stopper {:times 1})))

  (it "should not support args"
    (let [starter (stub :starter)
          m (mecha [a b] (:start (starter a b)))]
      (m 1 2)
      (should-have-invoked :starter {:times 1
                                     :with [1 2]})))

  (it "should handle state as a `let` would"
    (let [starter (stub :starter)
          m (mecha (:start [foo 1
                            foo (inc foo)]
                           (starter foo)))]
      (m)
      (should-have-invoked :starter {:times 1
                                     :with [2]}))))
