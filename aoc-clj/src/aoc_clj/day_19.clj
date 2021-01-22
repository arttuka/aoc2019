(ns aoc-clj.day-19
  (:require [clojure.core.async :as async :refer [<! <!! >! >!! chan close! go go-loop onto-chan! pipe pipeline-async]]
            [aoc-clj.intcode :as intcode]
            [aoc-clj.util :refer [binary-search indexed read-program]]))

(def size 100)

(defn make-check-positions [program]
  (fn [positions]
    (let [check (fn [pos out]
                  (let [cin (chan 2)]
                    (pipe (intcode/run program cin) out)
                    (onto-chan! cin pos)))
          pos-chan (chan 10)
          out-chan (chan 10)]
      (pipeline-async 1 out-chan check pos-chan)
      (onto-chan! pos-chan positions)
      out-chan)))

(defn get-slope [check-positions]
  (go (let [dy 100
            points-chan (check-positions (for [x (range dy)]
                                           [x (dec dy)]))
            part-chan (chan 2 (comp (partition-by identity) (map count)))]
        (pipe points-chan part-chan)
        (let [left (<! part-chan)
              width (<! part-chan)
              dx (+ left (/ width 2))]
          {:width (/ width dy)
           :slope (/ dx dy)}))))

(defn edge-compare
  ([check-positions y x]
   (edge-compare check-positions y x false))
  ([check-positions y x reverse?]
   (let [positions [[x y] [(inc x) y]]]
     (if reverse?
       (async/reduce + -1 (check-positions positions))
       (async/reduce - 1 (check-positions positions))))))

(defn find-max-x [check-positions {:keys [slope width]} y]
  (let [x (int (* slope y))
        x-width (* width y)]
    (binary-search #(edge-compare check-positions y %) x (int (+ x x-width)))))

(defn find-min-x [check-positions {:keys [slope width]} y]
  (go
    (let [x (int (* slope y))
          x-width (* width y)]
      (inc (<! (binary-search #(edge-compare check-positions y % true) (int (- x x-width)) x))))))

(defn find-bounds [check-positions slope min-y]
  (go
    (let [max-y (dec (+ min-y size))
          max-x (<! (find-max-x check-positions slope min-y))
          min-x (<! (find-min-x check-positions slope max-y))]
      [min-x min-y max-x max-y])))

(defn make-check-fit [check-positions slope]
  (fn [y]
    (go
      (let [[min-x _ max-x _] (<! (find-bounds check-positions slope y))]
        (and min-x max-x (<= (dec size) (- max-x min-x)))))))

(defn fit-compare [check-fit y]
  (go
    (let [fit (<! (check-fit y))
          prev-fit (<! (check-fit (dec y)))]
      (cond
        (not fit) -1
        (not prev-fit) 0
        :else 1))))

(defn run []
  (<!! (go
         (let [check-pos (make-check-positions (read-program "input19.txt"))
               slope (<! (get-slope check-pos))
               check-fit (make-check-fit check-pos slope)
               y (<! (binary-search #(fit-compare check-fit %) 1 (* 4 size (/ (:width slope)))))
               y' (reduce (fn [best curr]
                            (if (<!! (check-fit curr)) curr best))
                          y
                          (range (dec y) (- y 20) -1))
               [min-x min-y _ _] (<! (find-bounds check-pos slope y'))]
           [min-x min-y (+ (* 10000 min-x) min-y)]))))
