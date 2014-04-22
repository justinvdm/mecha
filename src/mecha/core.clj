(ns mecha.core
  (:require [clojure.set :refer [map-invert]]
            [clojure.walk :refer [postwalk]]))


(defrecord Mecha [])


(defn symbols-from-bindings [bs]
  (let [result (take-nth 2 bs)
        result (postwalk #(if (map? %) (keys %) %) result)
        result (flatten result)]
    result))


(defn bindings-to-map [bs]
  (let [lhs (take-nth 2 bs)
        rhs (take-nth 2 (rest bs))
        result (map vector lhs rhs)
        result (into {} result)]
    result))


(defn symbols-to-map [symbols]
  (let [ks (map keyword symbols)
        result (map vector ks symbols)
        result (into {} result)]
    result))


(defmacro mecha
  "Make a mecha capable of being started and stopped"
  [& m-body]
  (let [[m-args m-body] (if (-> m-body first vector?)
                          [(first m-body) (rest m-body)]
                          [[] m-body])

        m-body (for [body m-body] [(first body) (rest body)])
        m-body (into {} m-body)

        m-start-body (or (:start m-body) ())
        [m-bindings m-start-body] (if (-> m-start-body first vector?)
                                    [(first m-start-body) (rest m-start-body)]
                                    [[] m-start-body])
        m-stop-body (or (:stop m-body) ())

        [m-args m-opts] (split-with #(not= % '&) m-args)
        m-opts (second m-opts)
        m-opts-symbols (-> m-opts symbols-from-bindings vec)
        m-opts (bindings-to-map m-opts)

        m-attrs (concat m-args
                        m-opts-symbols
                        (symbols-from-bindings m-bindings))
        m-attrs (symbols-to-map m-attrs)]
    `(mecha-def
       (fn [m# ~@m-args & {:or ~m-opts
                           :keys ~m-opts-symbols}]
         (let ~m-bindings
           ~@m-start-body
           (merge m# ~m-attrs)))
       (fn [m#]
         (let [~(map-invert m-attrs) m#]
           ~@m-stop-body
           m#)))))


(defmacro defmecha
  "Define a mecha capable of being started and stopped"
  [m-name & m-body]
  (let [[m-doc m-body]
        (if (-> m-body first string?)
          [(first m-body) (rest m-body)]
          [nil m-body])]
    (if (nil? m-doc)
      `(def ~m-name (mecha ~@m-body))
      `(def ~m-name ~m-doc (mecha ~@m-body)))))


(defn mecha?
  "Returns true if m is a mecha, false otherwise."
  [m]
  (= mecha.core.Mecha (type m)))


(defn start
  "Starts a mecha"
  [m & args]
  (apply (-> m ::start) m args))


(defn stop
  "Stops a mecha"
  [m]
  ((-> m ::stop) m)
  (doseq [[k v] m]
    (if (mecha? v)
      (stop v))))


(defn mecha-def [m-start m-stop]
  (fn [& args]
    (let [m (assoc (mecha.core.Mecha.)
                   ::start m-start
                   ::stop m-stop)
          m (apply mecha.core/start m args)]
      m)))
