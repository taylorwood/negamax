(ns negamax.negamax)

(defn negamax [node depth-limit terminate? heuristic get-children computer human]
  (letfn [(score [node depth maximize?]
            (let [player (if maximize? computer human)]
              (if (or (< depth-limit depth)
                      (terminate? node))
                (/ (heuristic node player) depth) ;; depth quotient bias for early win
                (->> (get-children node player)
                     (map #(- (score % (inc depth) (not maximize?))))
                     (apply max Double/NEGATIVE_INFINITY)))))]
    (->> (get-children node computer)
         (map (fn [s] [s (score s 1 false)]))
         (sort-by second)
         (ffirst))))
