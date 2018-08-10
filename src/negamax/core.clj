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

(def board-size 3)
(def human 'O)
(def computer 'X)
(def players [human computer])

(defn play [get-human-move]
  (loop [player-seq (cycle players)
         board (make-board board-size)]
    (let [player (first player-seq)
          new-board
          (if (= human player)
            (let [cell (get-human-move board)]
              (if-not (get-in board cell)
                (assoc-in board cell human)
                (throw (IllegalArgumentException. "cell already occupied!"))))
            (time (->> (negamax board next-board-states game-over? score-board
                                {:max-depth Integer/MAX_VALUE :computer computer :human human})
                       (sort-by second)
                       (ffirst))))]
      (println player "turn:")
      (print-board new-board)
      (cond
        (empty? (open-coords new-board)) nil
        (winner new-board) (winner new-board)
        :else (recur (rest player-seq) new-board)))))

(defn index->coord [n dim] [(int (/ n dim)) (mod n dim)])

(defn -main [& _args]
  (println "Let's play a game of Tic-tac-toe. You can go first!")
  (println "Enter your moves as if the grid were numbered like a phone pad.")
  (play
   (fn [& _]
     (index->coord
      (->> (repeatedly #(try
                          (println "Enter cell number [1-9]:")
                          (Integer/parseInt (read-line))
                          (catch Exception _)))
           (filter (set (range 1 10)))
           (first)
           (dec))
      board-size))))
