(ns day-5
  (:require [clojure.string :as str]
            [clojure.math.numeric-tower :as math]))

(def input (slurp "data/day-5.txt"))

(def test-input (slurp "data/day-5.test.txt"))

(defn parse-input [input]
  (->> (str/split input #"\s+")
       (filter #(not (= "->" %)))
       (mapcat #(str/split % #","))
       (map #(Integer/parseInt %))
       (partition 2)
       (map vec)
       (partition 2)
       (map vec)))

(defn get-floor-bounds [lines]
  (let [flattened-list (flatten lines)
        all-endpoints (partition 2 flattened-list)
        max-x-bound (reduce max (map first all-endpoints))
        max-y-bound (reduce max (map second all-endpoints))
        max-bound (max max-x-bound max-y-bound)]
   [max-bound max-bound]))

(defn square-matrix [dimension]
  (vec (for [x (range 0 dimension)]
         (vec (take dimension (repeat 0))))))

(defn generate-points [point-1 point-2]
  (let [[x1 y1] point-1
        [x2 y2] point-2
        x-increment (if (< x1 x2) 1 -1)
        y-increment (if (< y1 y2) 1 -1)]
    (for [x (range x1 (+ x2 x-increment) x-increment)
          y (range y1 (+ y2 y-increment) y-increment)]
      [x y])))

(defn generate-points-2 [start end]
  (let [x1 (first start) x2 (first end)
        y1 (last start)  y2 (last end)
        xdiff (- x2 x1)
        ydiff (- y2 y1)
        maxdiff (max (math/abs xdiff) (math/abs ydiff))
        dx (/ xdiff maxdiff)
        dy (/ ydiff maxdiff)]
    (for [i (range (inc maxdiff))]
      [(math/round (+ x1 (* i dx))) (math/round (+ y1 (* i dy)))])))

(defn mark-floor [line floor]
  (let [points (apply generate-points line)               ;use generate-points-2 for part-2
        floor-state (atom floor)]
    (doseq [point points
            :let [floor-value (get-in @floor-state point)
                  new-floor-state (assoc-in @floor-state point (inc floor-value))]]
      (reset! floor-state new-floor-state))
    @floor-state))


(defn get-final-marked-floor [input]
  (let [lines (parse-input input)
        filtered-lines (filter #(or
                                  (= (first (first %1)) (first (second %1)))
                                  (= (second (first %1)) (second (second %1))))
                               lines)
        floor-bounds (get-floor-bounds lines)
        floor (square-matrix (inc (first floor-bounds)))]
    (loop [remaining-lines filtered-lines                   ; use all lines for part-2
           floor-state floor]
      (if (= 0 (count remaining-lines))
        floor-state
        (recur (drop 1 remaining-lines) (mark-floor (first remaining-lines) floor-state))))))

(defn solution [input]
  (->> input
       get-final-marked-floor
       flatten
       (filter #(> % 1))
       count))