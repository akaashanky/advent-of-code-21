(ns day-1)

(def test-input '(1 2 3 1 5 6 4))

(def real-input '())

(defn day-1-1 [input-list]
  (->> input-list
       (partition 2 1)
       (map (fn [pair] (< (first pair) (second pair))))
       (filter identity)
       count))

(defn day-1-2 [input-list]
  (->> input-list
       (partition 3 1)
       (map (fn [triplet] (reduce + triplet)))
       day-1-1))