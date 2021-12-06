(ns day-6
  (:require [clojure.string :as str]))

(def test-input (slurp "data/day-6.test.txt"))

(def input (slurp "data/day-6.txt"))

; part 1 using list comprehension

(defn parse-input [input]
  (->> (str/split input #",")
       (map #(Integer/parseInt %))))

(defn get-next-val [timer]
  (if (zero? timer)
    6 (dec timer)))

(defn get-school-after-today [current-school]
  (mapcat (fn [fish-timer]
            (if (= fish-timer 0)
              [(get-next-val fish-timer) 8]
              [(get-next-val fish-timer)]))
          current-school))

(defn get-school-after-n-days [starting-school n]
  (loop [current-school starting-school
         days-left n]
    (if (= days-left 0)
      current-school
      (recur (get-school-after-today current-school) (dec days-left)))))

(defn part-1 [input days]
  (-> (parse-input input)
      (get-school-after-n-days days)
      count))

; part 2 using maps (much more memory efficient)

(def empty-school-map {0 0, 1 0, 2 0, 3 0, 4 0, 5 0, 6 0, 7 0, 8 0})

(defn get-school-map-after-today [current-school-map]
  (let [new-fish-count (get current-school-map 0)]
    (-> (reduce-kv #(assoc %1 (get-next-val %2) %3) empty-school-map current-school-map)
        (assoc 8 new-fish-count)
        (update 6 + new-fish-count))))

(defn get-school-map-after-n-days [n current-school-map]
  (-> (iterate get-school-map-after-today current-school-map)
      (nth n)))

(defn part-2 [input days]
  (->> (parse-input input)
       frequencies
       (merge empty-school-map)
       (get-school-map-after-n-days days)
       vals
       (reduce +)))

