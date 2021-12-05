(ns day-4
  (:require [clojure.string :as str]))

(def test-input "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1\n\n22 13 17 11  0\n 8  2 23  4 24\n21  9 14 16  7\n 6 10  3 18  5\n 1 12 20 15 19\n\n 3 15  0  2 22\n 9 18 13 17  5\n19  8  7 25 23\n20 11 10 24  4\n14 21 16 12  6\n\n14 21 17 24  4\n10 16 15  9 19\n18  8 23 26 20\n22 11 13  6  5\n 2  0 12  3  7")

(defn get-inputs [input-str]
  (let [parsed-input (str/split input-str #"\s+")]
    {:numbers (map #(Integer/parseInt %) (str/split (first parsed-input) #","))
     :boards  (->> parsed-input
                   (drop 1)
                   (map #(Integer/parseInt %))
                   (partition 5)
                   (map vec)
                   (partition 5)
                   (map vec)
                   vec)
     :results (->> (repeat 0)
                   (take (dec (count parsed-input)))
                   (partition 5)
                   (map vec)
                   (partition 5)
                   (map vec)
                   vec)}))

(defn get-new-results [current-number boards old-results]
  (let [old-results-state (atom old-results)]
    (doseq [board-index (range 0 (count boards))
            row-index (range 0 5)
            col-index (range 0 5)]
      (if (= current-number (get-in boards [board-index row-index col-index]))
        (swap! old-results-state
               assoc-in
               [board-index row-index col-index]
               1)
        @old-results-state))
    @old-results-state))

(defn row-complete? [row] (every? #(= 1 %) row))

(defn transpose [m] (apply mapv vector m))

(defn get-winner [boards results]
  (let [winner-index (atom nil)]
    (doseq [position (range 0 (count boards))
            :let [result (get results position)]
            :while (not @winner-index)]
      (if (or (some row-complete? result) (some row-complete? (transpose result)))
        (reset! winner-index position)))
    {:board (get boards @winner-index)
     :result (get results @winner-index)
     :position @winner-index}))

(defn get-winners [boards results]
  (let [winners (atom [])]
    (doseq [position (range 0 (count boards))
            :let [result (get results position)]]
      (if (or (some row-complete? result) (some row-complete? (transpose result)))
        (swap! winners conj {:board (get boards position)
                             :result result
                             :position position})))
    @winners))

(defn calculate-score [winner current-number]
  (let [{winner-result :result winner-board :board} winner
        score (atom 0)
        _ (prn current-number)]
    (doseq [row-index (range 0 5)
          col-index (range 0 5)
          :let [unmarked-num (if (= 0 (get-in winner-result [row-index col-index]))
                               (get-in winner-board [row-index col-index])
                               0)]]
      (swap! score + unmarked-num))
    (swap! score * current-number)))

(defn play-bingo [input-map]
  (let [{numbers :numbers boards :boards results :results} input-map]
    (loop [remaining-numbers numbers
           old-results results]
      (let [new-results (get-new-results (first remaining-numbers) boards old-results)
            winner (get-winner boards new-results)
            _ (prn winner)]
        (if (nil? (:board winner))
          (recur (drop 1 remaining-numbers) new-results)
          (calculate-score winner (first remaining-numbers)))))))

(defn filter-winners [remaining-boards winners]
  (filter (fn [board] (not (some #(= board (:board %1)) winners))) remaining-boards))

(defn play-bingo-2 [input-map]
  (let [{numbers :numbers boards :boards results :results} input-map
        remaining-boards (atom boards)]
    (loop [remaining-numbers numbers
           old-results results]
      (let [new-results (get-new-results (first remaining-numbers) boards old-results)
            winners (get-winners boards new-results)]
        (if (= (count winners) (count boards))
          (let [winner {:board  (first @remaining-boards)
                        :result (:result (first (filter #(= (first @remaining-boards) (:board %1)) winners)))}]
            (calculate-score winner (first remaining-numbers)))
          (do (swap! remaining-boards filter-winners winners)
            (recur (drop 1 remaining-numbers) new-results)))))))

(defn part-1 [input-map] (play-bingo input-map))

(defn part-2 [input-map] (play-bingo-2 input-map))


