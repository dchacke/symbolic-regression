(ns first-genetic-program.anon)

(defn one [a]
  (inc a))

(defn two [& args]
  (apply str args))
