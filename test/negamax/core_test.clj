(ns negamax.core-test
  (:require [clojure.test :refer :all]
            [clojure.spec.alpha :as s]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as props]
            [negamax.core :refer :all]))

(s/def ::cell-pos (s/tuple (s/int-in 0 3) (s/int-in 0 3)))

(def starting-board-gen
  "Generates boards with a random 'O' move applied."
  (gen/fmap (fn [pos] (-> (make-board board-size)
                          (assoc-in pos 'O)))
            (s/gen ::cell-pos)))

(defspec always-draw-against-self 1000
  (let [players [{:marker 'X :move-fn (partial negamax-move 'X 'O)}
                 {:marker 'O :move-fn (partial negamax-move 'O 'X)}]]
    (props/for-all [board starting-board-gen]
      (nil? (play board players)))))
