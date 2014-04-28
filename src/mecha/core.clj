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


(defn mecha?
  "returns true if m is a mecha, false otherwise."
  [m]
  (= mecha.core.Mecha (type m)))


(defn mechadef?
  "returns true if mdef is a mecha def, false otherwise."
  [mdef]
  (= ::MechaDef (type mdef)))


(defn switch?
  "returns true if s is a mecha switch, false otherwise."
  [s]
  (= ::MechaSwitch (type s)))


(defmulti start
  "multipurpose function for starting things"
  (fn [t & _] (type t)))


(defmethod start :default [& _])


(defmethod start mecha.core.Mecha [m & args]
  "starts a mecha"
  (apply (-> m meta ::meta :start) m args))


(defmethod start ::MechaDef [mdef & args]
  "starts a new mecha using the given mecha def"
  (apply mdef args))


(defmethod start ::MechaSwitch [mswitch k & args]
  "invokes a mecha switch, possibly initialising it"
  (apply mswitch k args))


(defmulti stop
  "multipurpose function for stopping things"
  (fn [t & _] (type t)))


(defmethod stop :default [& _])


(defmethod stop mecha.core.Mecha [m]
  "stops a mecha"
  ((-> m meta ::meta :stop) m)
  (doseq [[k v] (-> m meta ::meta :scope)] (stop v)))


(defmethod stop ::MechaSwitch [mswitch]
  "stops a mecha"
  (mswitch nil))


(defmacro mecha
  "make a mecha capable of being started and stopped"
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
                     metadata# (-> m# meta ::meta)
                     metadata# (assoc metadata# :scope ~m-scope)
                     m# (merge m# result#)
                     m# (with-meta m# (assoc (meta m#) ::meta metadata#))]
                 m#)))

           m-stop#
           (fn [m#]
             (let [~(map-invert m-scope) (-> m# meta ::meta :scope)]
               ~@m-stop-body
               m#))

           mdef#
           (fn [& args#]
             (let [m# (with-meta (mecha.core.Mecha.) {::meta {:scope {}
                                                              :start m-start#
                                                              :stop m-stop#}})
                   m# (apply start m# args#)]
               m#))

           mdef# (with-meta mdef# {:type ::MechaDef})]
       mdef#)))


(defmacro defmecha
  "define a mecha capable of being started and stopped"
  [m-name & m-body]
  (let [[m-doc m-body]
        (if (-> m-body first string?)
          [(first m-body) (rest m-body)]
          [nil m-body])]
    (if (nil? m-doc)
      `(def ~m-name (mecha ~@m-body))
      `(def ~m-name ~m-doc (mecha ~@m-body)))))


(defn switch [mdefs]
  "make a mecha switch"
  (let [current (atom nil)
        current-k (atom nil)
        switcher
        (fn [k & args]
          (let [mdef (get mdefs k)]
            (when-not (= k @current-k)
              (if-not (nil? @current)
                (stop @current))
              (if-not (nil? mdef)
                (reset! current (apply start mdef args))
                (reset! current nil))
              (reset! current-k k))
            @current))
        switcher (with-meta switcher {:type ::MechaSwitch})]
    switcher))


(defmacro defswitch
  "define a mecha switch"
  ([switch-name mdefs]
   `(def ~switch-name (switch ~mdefs)))
  ([switch-name switch-doc mdefs]
   `(def ~switch-name ~switch-doc (switch ~mdefs))))
