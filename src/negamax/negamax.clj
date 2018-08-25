(ns negamax.negamax
  "Implements negamax algorithm with alpha beta pruning.")

(def ^:const −∞ Double/NEGATIVE_INFINITY)
(def ^:const +∞ Double/POSITIVE_INFINITY)

(defn negamax [node get-children terminate? heuristic
               {:keys [max-depth player opponent]}]
  (letfn [(score [node depth alpha beta maximize?]
            (let [player (if maximize? player opponent)]
              (if (or (< max-depth depth)
                      (terminate? node))
                (/ (heuristic node player) depth) ;; depth quotient bias for early win
                (let [best
                      (reduce
                       (fn [[alpha value] child]
                         (let [value' (max value
                                           (- (score child (inc depth) (- beta) (- alpha) (not maximize?))))
                               alpha' (max alpha value')]
                           (if (>= alpha' beta)
                             (reduced value')
                             [alpha' value'])))
                       [alpha −∞]
                       (get-children node player))]
                  (if (number? best) best (last best))))))]
    (->> (get-children node player)
         (map (fn [s] [s (score s 1 −∞ +∞ false)])))))
