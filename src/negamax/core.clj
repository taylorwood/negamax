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

(defn score-board [board]
  (case (winner board)
    X  10
    O -10
    0))

(defn next-board-states [board player]
  (map #(assoc-in board % player) (open-coords board)))

(defn simulate []
  (loop [players (cycle '[O X])
         board (first (next-board-states (make-board 3) (first players)))]
    (println)
    (print-board board)
    (if-let [next-states (seq (next-board-states board (first players)))]
      (let [next-moves (map
                         (fn [node]
                           [node (minimax/score node
                                                5
                                                (memoize score-board)
                                                (memoize next-board-states)
                                                true)])
                         next-states)
            best-move (first (last (sort-by second next-moves)))]
        (println "player move scores:" (first players)
                 (map second next-moves)
                 (count next-moves) "options")
        (if (winner best-move)
          (do
            (println)
            (print-board best-move)
            (first players))
          (recur (rest players) best-move)))
      (println 'tie))))

(simulate)