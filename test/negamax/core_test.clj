(ns negamax.core-test
  (:require [clojure.test :refer :all]
            [negamax.core :refer :all]))

(deftest play-test
  (dotimes [_ 100]
    (println "playing a game")
    (let [get-player-move (fn [board]
                            (assoc-in board (rand-nth (open-coords board)) 'O))
          winner (play (make-board board-size)
                       [{:marker 'O :move-fn get-player-move}
                        {:marker 'X :move-fn (partial negamax-move 'X 'O)}])]
      (prn (or winner 'nobody) 'wins)
      (is (or (nil? winner) (= computer winner))
          "random moves should never win"))))

(deftest unstoppable-force-immovable-object-test
  (dotimes [_ 100]
    (println "playing a game")
    (let [board (as-> (make-board board-size) b
                  (assoc-in b (rand-nth (open-coords b)) 'O))
          players [{:marker 'X :move-fn (partial negamax-move 'X 'O)}
                   {:marker 'O :move-fn (partial negamax-move 'O 'X)}]]
      (print-board board)
      (is (nil? (play board players)) "AI should always draw against itself"))))
