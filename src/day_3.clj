(ns day-3
  (:require [clojure.string :as str]))

(def test-input "00100\n11110\n10110\n10111\n10101\n01111\n00111\n11100\n10000\n11001\n00010\n01010")

(defn parse-input [input-str]
  (str/split input-str #"\s+"))

(defn get-gamma-bit [input-ratings position]
  (if (>= (reduce
           (fn [acc val]
             (+ acc
                (Float/parseFloat
                  (subs val position (+ position 1)))))
           0 input-ratings)
         (/ (count input-ratings) 2))
    \1 \0))
(defn flip-bit [bit-char] (if (= bit-char \1) \0 \1))

(def get-epsilon-bit #(flip-bit (get-gamma-bit %1 %2)))

(defn get-gamma-rate [input-ratings]
  (str/join (map #(get-gamma-bit input-ratings %1) (range 0 (count (first input-ratings))))))

(defn get-epsilon-rate [input-ratings]
  (str/join (map #(get-epsilon-bit input-ratings %1) (range 0 (count (first input-ratings))))))

(defn get-power-consumption-ratings [input-str]
  (let [input-ratings (parse-input input-str)]
    [(Integer/parseInt (get-gamma-rate input-ratings) 2)
     (Integer/parseInt (get-epsilon-rate input-ratings) 2)]))

(defn part-1 [input-str]
  (reduce * (get-power-consumption-ratings input-str)))

(defn matches-o2-criteria? [input-ratings position input]
  (= (get-gamma-bit input-ratings position) (.charAt input position)))

(defn matches-co2-criteria? [input-ratings position input]
  (= (get-epsilon-bit input-ratings position) (.charAt input position)))

(defn get-rating [match-criteria input]
  (loop [input-ratings input
         position 0]
    (if (= 1 (count input-ratings))
      (first input-ratings)
      (recur (filter #(match-criteria input-ratings position %1) input-ratings) (inc position)))
    ))

(def get-o2-rating #(get-rating matches-o2-criteria? %))

(def get-co2-rating #(get-rating matches-co2-criteria? %))

(defn get-life-support-ratings [input-str]
  (let [input-ratings (parse-input input-str)]
    [(Integer/parseInt (get-o2-rating input-ratings) 2)
     (Integer/parseInt (get-co2-rating input-ratings) 2)]))

(defn part-2 [input-str]
  (reduce * (get-life-support-ratings input-str)))


