(ns negamax.core-test
  (:require [clojure.test :refer :all]
            [negamax.core :refer :all]))

(deftest play-test
  (dotimes [_ 100]
    (println "playing a game")
    (let [get-player-move (fn [board] (rand-nth (open-coords board)))
          winner (play get-player-move)]
      (prn (or winner 'nobody) 'wins)
      (is (or (nil? winner) (= computer winner))
          "you should never win"))))
