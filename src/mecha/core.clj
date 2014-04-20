(ns mecha.core)


(defmacro defmecha
  "Define a thing capable of being started and stopped"
  [m-name & args]
  (let [[m-args m-state m-body]
        (cond
          (and (-> args first vector?)
               (-> args second vector?))
          args

          (and (-> args first vector?)
               (-> args second vector? not))
          [(first args) [] (rest args)]

          (and (-> args first vector? not)
               (-> args second vector? not))
          (concat [[] []] args))]
    (println m-args m-state m-body)))


(defn stop [])
