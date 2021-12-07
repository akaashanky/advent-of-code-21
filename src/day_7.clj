(ns day-7
  (:require [clojure.string :as str]
            [clojure.math.numeric-tower :as math]))

(def test-input (slurp "data/day-7.test.txt"))

(def input (slurp "data/day-7.txt"))

(defn parse-input [input]
  (->> (str/split input #",")
       (map #(Integer/parseInt %))))

(defn get-total-fuel-consumption-1 [crab-positions-map dest-position]
  (reduce-kv (fn [acc key val]
               (+ acc (* val (math/abs (- dest-position key)))))
             0 crab-positions-map))

(defn get-total-fuel-consumption-2 [crab-positions-map dest-position]
  (reduce-kv (fn [acc key val]
               (let [dist (math/abs (- dest-position key))]
                (+ acc (* val (/ (* dist (inc dist)) 2)))))
             0 crab-positions-map))

(defn part-1 [input]
  (let [crab-positions (parse-input input)
        max-position (apply max crab-positions)
        min-position (apply min crab-positions)
        crab-positions-map (frequencies crab-positions)]
    (apply min
      (map #(get-total-fuel-consumption-1 crab-positions-map %)
           (range min-position (inc max-position))))))

(defn part-2 [input]
  (let [crab-positions (parse-input input)
        max-position (apply max crab-positions)
        min-position (apply min crab-positions)
        crab-positions-map (frequencies crab-positions)]
    (apply min
      (map #(get-total-fuel-consumption-2 crab-positions-map %)
           (range min-position (inc max-position))))))
