(ns mecha.core)


(defrecord MechaDef [])
(defrecord Mecha [])


(defmacro defmecha
  "Define a thing capable of being started and stopped"
  [m-name & args]
  (let [[m-args m-body] (if (-> args first vector?)
                          [(first args) (rest args)]
                          [[] args])

        m-body (for [body m-body] [(first body) (rest body)])
        m-body (into {} m-body)

        m-start (or (:start m-body) ())
        [m-state m-start] (if (-> m-start first vector?)
                            [(first m-start) (rest m-start)]
                            [[] m-start])

        m-stop (or (:stop m-body) ())]))


(defn mecha?
  "Returns true if m is a mecha, false otherwise."
  [m]
  (= mecha.core.Mecha (type m)))


(defn start
  "Starts a mecha"
  [])


(defn stop
  "Stops a mecha"
  [])
