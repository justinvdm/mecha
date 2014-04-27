(ns mecha.core
  (:require [clojure.set :refer [map-invert]]
            [clojure.walk :refer [postwalk]]))


(defrecord Mecha [])


(defn- symbols-from-bindings [bs]
  (let [result (take-nth 2 bs)
        result (postwalk #(if (map? %) (keys %) %) result)
        result (flatten result)]
    result))


(defn- bindings-to-map [bs]
  (let [lhs (take-nth 2 bs)
        rhs (take-nth 2 (rest bs))
        result (map vector lhs rhs)
        result (into {} result)]
    result))


(defn- symbols-to-map [symbols]
  (let [ks (map keyword symbols)
        result (map vector ks symbols)
        result (into {} result)]
    result))


(defn- parse-vargs [vargs]
  (cond
    (nil? vargs)
    {:symbols []
     :form `{:or []
             :keys []}}

    (symbol? vargs)
    {:symbols [vargs]
     :form vargs}

    (vector? vargs)
    (let [symbols (-> vargs symbols-from-bindings vec)
          opts (bindings-to-map vargs)]
      {:symbols symbols
       :form `{:or ~opts
               :keys ~symbols}})

    :else (throw (Error. "foo"))))


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

        [m-args m-vargs] (split-with #(not= % '&) m-args)
        m-vargs (second m-vargs)
        {m-vargs-symbols :symbols
         m-vargs-form  :form} (parse-vargs m-vargs)

        m-scope (concat m-args
                        m-vargs-symbols
                        (symbols-from-bindings m-bindings))
        m-scope (symbols-to-map m-scope)]
    `(let [m-start#
           (fn [m# ~@m-args & ~m-vargs-form]
             (let ~m-bindings
               (let [result# (do ~@m-start-body)
                     result# (if (map? result#)
                               result#
                               {})
                     data# (assoc (::data m#) :scope ~m-scope)
                     m# (assoc m# ::data data#)
                     m# (merge m# (dissoc result# ::data))]
                 m#)))

           m-stop#
           (fn [m#]
             (let [~(map-invert m-scope) (-> m# ::data :scope)]
               ~@m-stop-body
               m#))]
       (fn [& args#]
         (let [m# (assoc (mecha.core.Mecha.) ::data {:scope {}
                                                     :start m-start#
                                                     :stop m-stop#})
               m# (apply mecha.core/start m# args#)]
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
  (apply (-> m ::data :start) m args))


(defn stop
  "Stops a mecha"
  [m]
  ((-> m ::data :stop) m)
  (doseq [[k v] (-> m ::data :scope)]
    (if (mecha? v)
      (stop v))))
