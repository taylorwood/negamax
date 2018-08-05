(ns negamax.minimax)

(comment
  "function minimax(node, depth, maximizingPlayer) is
    if depth = 0 or node is a terminal node then
      return the heuristic value of node
    if maximizingPlayer then
      value := −∞
      for each child of node do
        value := max(value, minimax(child, depth − 1, FALSE))
      return value
    else (* minimizing player *)
      value := +∞
      for each child of node do
        value := min(value, minimax(child, depth − 1, TRUE))
      return value")

(defn score [node depth-limit get-score get-children maximizing?]
  (letfn [(iter [node depth maximizing?]
            (let [player (if maximizing? 'X 'O)]
              (if (or (<= depth-limit depth)
                      (empty? (get-children node player)))
                (/ (get-score node player) depth)
                (let [[minmax limit] (if maximizing?
                                       [max Double/NEGATIVE_INFINITY]
                                       [min Double/POSITIVE_INFINITY])]
                  (->> (get-children node player)
                       (map #(iter % (inc depth) (not maximizing?)))
                       (apply minmax limit))))))]
    (iter node 0 maximizing?)))
