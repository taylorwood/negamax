(ns negamax.core
  "Implements tic-tac-toe vs. a real smarty pants simulator."
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

(defn winner [board]
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

(defn play [board players]
  (loop [player-seq (cycle players)
         board board]
    (let [{:keys [move-fn marker]} (first player-seq)
          new-board (move-fn board)]
      (println marker "turn:")
      (print-board new-board)
      (cond
        (empty? (open-coords new-board)) nil
        (winner new-board) (winner new-board)
        :else (recur (rest player-seq) new-board)))))

(defn index->coord [n dim] [(int (/ n dim)) (mod n dim)])

(defn negamax-move
  "Returns the board having applied the 'best' move."
  [player opponent board]
  (->> (negamax board next-board-states game-over? score-board
                {:max-depth Integer/MAX_VALUE :player player :opponent opponent})
       (sort-by second)
       (ffirst)))

(def board-size 3)

(defn human-move
  "Returns the board with a user-supplied move applied."
  [board]
  (let [cell (index->coord
              (->> (repeatedly #(try
                                  (println "Enter cell number [1-9]:")
                                  (Integer/parseInt (read-line))
                                  (catch Exception _)))
                   (filter (set (range 1 10)))
                   (first)
                   (dec))
              board-size)]
    (if-not (get-in board cell)
      (assoc-in board cell 'O)
      (throw (IllegalArgumentException. "cell already occupied!")))))

(def players [{:marker 'O :move-fn human-move}
              {:marker 'X :move-fn (partial negamax-move 'X 'O)}])

(defn -main [& _args]
  (println "Let's play a game of Tic-tac-toe. You can go first!")
  (println "Enter your moves as if the grid were numbered like a phone pad.")
  (play (make-board board-size) players))
