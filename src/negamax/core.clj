(ns negamax.core)

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

(defn negamax [board depth-limit]
  (letfn [(score [board depth maximize?]
            (let [player (if maximize? 'X 'O)]
              (if (or (< depth-limit depth)
                      (game-over? board))
                (/ (score-board board player) depth)
                (->> (next-board-states board player)
                     (map #(- (score % (inc depth) (not maximize?))))
                     (apply max Integer/MIN_VALUE)))))]
    (->> (next-board-states board computer)
         (map (fn [s] [s (score s 1 false)]))
         (sort-by second)
         (ffirst))))

(defn play [get-player-move]
  (loop [players (cycle [human computer])
         board (make-board 3)]
    (let [player (first players)
          new-board (if (= human player)
                      (let [cell-pos (get-player-move)]
                        (if-not (get-in board cell-pos)
                          (assoc-in board cell-pos human)
                          (throw (IllegalArgumentException. "cell already occupied!"))))
                      (negamax board 20))]
      (println player "turn:")
      (print-board new-board)
      (Thread/sleep 250)
      (cond
        (empty? (open-coords new-board)) nil
        (winner new-board) (winner new-board)
        :else (recur (rest players) new-board)))))

(comment
  (play (fn []
          (println "Enter cell number (1-9):")
          (let [n (dec (Integer/parseInt (read-line)))]
            [(int (/ n 3))
             (mod n 3)]))))
