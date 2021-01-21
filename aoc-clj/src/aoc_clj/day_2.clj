(ns aoc-clj.day-2
  (:require [aoc-clj.intcode :as intcode]
            [aoc-clj.util :refer [read-program]]))

(def pairs (for [t (range)
                 i (range 0 (inc t))]
             [i (- t i)]))

(defn run-with-input [program [i j]]
  (let [program (assoc program 1 i, 2 j)
        end-state (intcode/run program nil nil)]
    (get-in end-state [:program 0])))

(defn run []
  (let [program (read-program "input2.txt")
        results (take-while (partial not= 19690720) (map (partial run-with-input program) pairs))
        [noun verb] (nth pairs (count results))]
    (+ (* 100 noun) verb)))
