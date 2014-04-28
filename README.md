# mecha

coordinate starting and stopping things in clojure

```clojure
(ns thing.core
  (:require [mecha.core :refer [defmecha stop]]))


(defmecha foo [foo-name & [suffix "-foo"]]
  (:start [foo-name (str foo-name suffix)]
          (println foo-name "starting"))
  (:stop (println foo-name "stopping")))


(defmecha bar
  (:start [bar-foo (foo "bar")]
          (println "bar starting")))


(defmecha sys
  "systems are simply mecha made up of other mecha"
  (:start [sys-foo (foo "sys")
           sys-bar (bar)]))


(let [s (sys)]
  ; sys-foo starting
  ; bar-foo starting
  ; bar starting

  (stop s)
  ; bar-foo stopping
  ; sys-foo stopping
  ())
```


## install

add it to your project's `project.clj`

```clojure
{...
 {:dependencies [...
                 [mecha "0.5.0"]]}}
```
