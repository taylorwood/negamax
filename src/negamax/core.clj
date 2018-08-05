(ns negamax.core
  (:require [negamax.minimax :as minimax]))

(defn make-board [width & [height]]
  (let [empty-row (vec (repeat width nil))]
    (vec (repeat (or width height) empty-row))))

(defn print-board [board]
  (doseq [row board]
    (apply println (map #(or % "_") row))))

(defn open-coords [board]
  (for [y (range (count board))
        x (range (count (nth board y)))
        :when (nil? (get-in board [y x]))]
    [y x]))

(defn rotate [m] (apply map vector (reverse m)))

(defn win-paths [board]
  (let [rotated (rotate board)
        diagonals #(map-indexed (fn [i row] (row i)) %)]
    (concat (map set board)
            (map set rotated)
            [(set (diagonals board))]
            [(set (diagonals rotated))])))

(defn winner [board]
  (ffirst
   (for [path (win-paths board)
         :when (and (= 1 (count path))
                    (not= #{nil} path))]
     path)))

(defn score-board [board player]
  (if-let [winner (winner board)]
    (if (= 'X winner) 10 -10)
    0))

(defn next-board-states [board player]
  (map #(assoc-in board % player) (open-coords board)))

(defn simulate [get-player-move]
  (loop [players (cycle '[O X])
         board (make-board 3)]
    (let [new-board
          (if (= 'O (first players))
            (let [cell-num (dec (get-player-move))
                  cell-pos [(int (/ cell-num 3))
                            (mod cell-num 3)]]
              (if-not (get-in board cell-pos)
                (assoc-in board cell-pos 'O)
                (throw (IllegalArgumentException. "cell already occupied!"))))
            (let [next-states (seq (next-board-states board (first players)))
                  next-moves (map
                               (fn [node]
                                 [node (minimax/score node
                                                      50
                                                      score-board
                                                      next-board-states
                                                      true)])
                               next-states)
                  _ (do
                      (println "----")
                      (println "states:")
                      (doseq [[board score] next-moves]
                        (print-board board)
                        (println "score:" score))
                      (println "----"))
                  best-move (first (last (sort-by second next-moves)))]
              best-move))]
      (println (first players) "turn:")
      (print-board new-board)
      (Thread/sleep 250)
      (or (winner new-board)
          (recur (rest players) new-board)))))

(comment
  (simulate (fn []
              (println "Enter cell number (1-9):")
              (Integer/parseInt (read-line)))))
