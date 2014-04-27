(ns mecha.core-spec
  (:require [clojure.repl :refer [doc]]
            [speclj.core :refer :all]
            [mecha.core :refer :all]))


(defmecha mecha-foo)
(defmecha mecha-bar "is a bar")


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
          m (mecha (:start [sub-m ((mecha (:start (starter))))]))]
      (m)
      (should-have-invoked :starter {:times 1})))

  (it "should stop its own mecha when it is stopped"
    (let [stopper (stub :stopper)
          m (mecha (:start [sub-m
                            ((mecha (:stop (stopper))))]))]
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
          stopper (stub :stopper)
          mdef (mecha (:start [foo 1
                               foo (inc foo)

                               {bar :bar} {:bar 3}]
                              (starter foo bar))
                      (:stop (stopper foo bar)))
          m (mdef)]
      (should-have-invoked :starter {:times 1
                                     :with [2 3]})
      (stop m)
      (should-have-invoked :stopper {:times 1
                                     :with [2 3]})))

  (it "should support option arguments"
    (let [starter (stub :starter)
          stopper (stub :stopper)
          mdef (mecha [foo & [bar 5
                              baz 4]]
                      (:start (starter foo bar baz))
                      (:stop (stopper foo bar baz)))
          m (mdef 2 :bar 3)]
      (should-have-invoked :starter {:times 1
                                     :with [2 3 4]})
      (stop m)
      (should-have-invoked :stopper {:times 1
                                     :with [2 3 4]})))

  (it "should support variable args"
    (let [starter (stub :starter)
          stopper (stub :stopper)
          mdef (mecha [foo & args]
                      (:start (apply starter foo args))
                      (:stop (apply stopper foo args)))
          m (mdef 2 3 4)]
      (should-have-invoked :starter {:times 1
                                     :with [2 3 4]})
      (stop m)
      (should-have-invoked :stopper {:times 1
                                     :with [2 3 4]})))

  (it "should allow attributes to be returned by the start body"
    (let [mdef (mecha (:start [foo 2
                               bar 3]
                              {:foo 4
                               :bar 5}))
          m (mdef)]
      (should= 4 (:foo m))
      (should= 5 (:bar m)))))


(describe "defmecha"
  (it "should define a mecha"
    (should-be mecha? (mecha-foo)))

  (it "should be documentable"
    (should-contain "is a bar"
                    (with-out-str (doc mecha-bar)))))
