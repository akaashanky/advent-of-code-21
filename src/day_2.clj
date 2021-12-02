(ns day-2
  (:require [clojure.string :as str]))

(def test-input "forward 5\ndown 5\nforward 8\nup 3\ndown 8\nforward 2")

(defn operation-select [direction]
  (case direction
    "forward" +
    "down" +
    "up" -))

(def horizontal-directions ["forward"])
(def depth-directions ["down" "up"])

(defn traverse-1 [instructions-str]
  (let [instructions (str/split instructions-str #"\s+")
        inst-pairs (partition 2 instructions)
        horizontal-instructions (filter
                                  #(.contains horizontal-directions (first %1))
                                  inst-pairs)
        depth-instructions (filter
                             #(.contains depth-directions (first %1))
                             inst-pairs)
        start-position [0 0 0]
        horizontal-position (first start-position)
        depth (second start-position)
        reduce-fn #((operation-select (first %2)) %1 (Integer/parseInt (second %2)))]
    [(reduce reduce-fn horizontal-position horizontal-instructions)
     (reduce reduce-fn depth depth-instructions)]))

(defn traverse-2 [instructions-str]
  (let [instructions (str/split instructions-str #"\s+")
        inst-pairs (partition 2 instructions)
        aim (atom 0)
        reduce-fn (fn [result inst-pair]
                    (let [operation (operation-select (first inst-pair))
                          should-move? (.contains horizontal-directions (first inst-pair))]
                      (cond
                        should-move? [(operation (first result) (Integer/parseInt (second inst-pair)))
                                      (operation (second result) (* @aim (Integer/parseInt (second inst-pair))))
                                      @aim]
                        :else [(first result)
                               (second result)
                               (swap! aim operation (Integer/parseInt (second inst-pair)))])))]
    (reduce reduce-fn [0 0 @aim] inst-pairs)))


(defn day-2-1 [input]
  (reduce * (traverse-1 input)))

(defn day-2-2 [input]
  (reduce * (take 2 (traverse-2 input))))