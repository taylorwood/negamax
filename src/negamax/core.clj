(ns negamax.core
  "Implements tic-tac-toe vs. a smarty pants simulator."
  (:require [negamax.negamax :refer :all]))

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

(defn win-paths
  "Returns sets of cell values along vertical/horizontal/diagonal paths."
  [board]
  (let [rotated (apply map vector (reverse board))
        diagonals #(map-indexed (fn [i row] (row i)) %)]
    (concat (map set board)
            (map set rotated)
            [(set (diagonals board))]
            [(set (diagonals rotated))])))

(defn winner
  "Returns the tic-tac-toe winner, if any."
  [board]
  (ffirst
   (for [path (win-paths board)
         :when (and (= 1 (count path))
                    (not= #{nil} path))]
     path)))

(defn game-over? [board]
  (or (empty? (open-coords board))
      (winner board)))

(defn score-board [board player]
  (if-let [winner (winner board)]
    (let [sign (if (= player winner) + -)]
      (sign 1))
    0))

(defn next-board-states [board player]
  (map #(assoc-in board % player) (open-coords board)))

(def human 'O)
(def computer 'X)
(def players [human computer])

(defn play [get-player-move]
  (loop [player-seq (cycle players)
         board (make-board 3)]
    (let [player (first player-seq)
          new-board
          (if (= human player)
            (let [cell-pos (get-player-move board)]
              (if-not (get-in board cell-pos)
                (assoc-in board cell-pos human)
                (throw (IllegalArgumentException. "cell already occupied!"))))
            (time (negamax board game-over? score-board next-board-states
                           {:max-depth Integer/MAX_VALUE :computer computer :human human})))]
      (println player "turn:")
      (print-board new-board)
      (cond
        (empty? (open-coords new-board)) nil
        (winner new-board) (winner new-board)
        :else (recur (rest player-seq) new-board)))))

(comment
  (play
    (fn [& _]
      (Thread/sleep 200)
      (println "Enter cell number [1-9] (left->right, top->down):")
      (let [n (dec (Integer/parseInt (read-line)))]
        [(int (/ n 3))
         (mod n 3)]))))

