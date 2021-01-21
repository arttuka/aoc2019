(ns aoc-clj.day-11
  (:require [clojure.core.async :as async :refer [<! <!! >! >!! chan go pipe]]
            [clojure.string :as str]
            [aoc-clj.intcode :as intcode]
            [aoc-clj.util :refer [async-reductions read-program split-chan zip]]))

(defn get-input [{:keys [pos white]}]
  (if (contains? white pos) 1 0))

(def left-turn {:up    :left
                :left  :down
                :down  :right
                :right :up})

(def right-turn {:up    :right
                 :right :down
                 :down  :left
                 :left  :up})

(defn turn-robot [dir left?]
  ((if left? left-turn right-turn) dir))

(defn move-robot [[x y] dir]
  (let [[dx dy] (case dir
                  :up [0 -1]
                  :right [1 0]
                  :down [0 1]
                  :left [-1 0])]
    [(+ x dx) (+ y dy)]))

(defn step [{:keys [pos dir white]} [paint turn]]
  (let [next-white ((if (zero? paint) disj conj) white pos)
        next-dir (turn-robot dir (zero? turn))
        next-pos (move-robot pos next-dir)]
    {:pos   next-pos
     :dir   next-dir
     :white next-white}))

(defn generate-map [white]
  (let [[xs ys] (apply zip (seq white))
        min-x (reduce min xs)
        min-y (reduce min ys)
        max-x (reduce max xs)
        max-y (reduce max ys)]
    (str/join "\n" (for [y (range (dec min-y) (+ 2 max-y))]
                     (str/join (for [x (range (dec min-x) (+ 2 max-x))]
                                 (if (contains? white [x y]) "#" ".")))))))

(defn run []
  (let [initial-state {:pos [0 0], :dir :up, :white #{[0 0]}}
        program (read-program "input11.txt")
        cin (chan 100)
        cout (intcode/run program cin)
        outputs (chan 100 (partition-all 2))
        [states-1 states-2] (split-chan (async-reductions step initial-state outputs))
        inputs (async/map get-input [states-1])]
    (pipe cout outputs)
    (pipe inputs cin)
    (let [final-state (last (<!! (async/into [] states-2)))]
      (println (generate-map (:white final-state))))))
