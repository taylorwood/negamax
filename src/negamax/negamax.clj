(ns negamax.negamax)

(def −∞ Double/NEGATIVE_INFINITY)
(def +∞ Double/POSITIVE_INFINITY)

(defn negamax [node terminate? heuristic get-children
               {:keys [max-depth computer human]}]
  (letfn [(score [node depth alpha beta maximize?]
            (let [player (if maximize? computer human)]
              (if (or (< max-depth depth)
                      (terminate? node))
                (/ (heuristic node player) depth) ;; depth quotient bias for early win
                (let [best
                      (reduce
                       (fn [[alpha value] child]
                         (let [value' (max value (- (score child (inc depth) (- beta) (- alpha) (not maximize?))))
                               alpha' (max alpha value')]
                           (if (>= alpha' beta)
                             (reduced value')
                             [alpha' value'])))
                       [alpha −∞]
                       (get-children node player))]
                  (if (number? best) best (last best))))))]
    (->> (get-children node computer)
         (map (fn [s] [s (score s 1 −∞ +∞ false)]))
         (sort-by second)
         (ffirst))))
