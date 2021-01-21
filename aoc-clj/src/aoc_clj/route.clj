(ns aoc-clj.route
  (:require [clojure.set :as set]
            [aoc-clj.util :refer [queue]]))

(defn adjacent-positions [[x y]]
  [[(inc x) y]
   [(dec x) y]
   [x (inc y)]
   [x (dec y)]])

(defn get-new-positions [discovered pos]
  (remove discovered (adjacent-positions pos)))

(defn generate-route [pos parents]
  (if-let [parent (get parents pos)]
    (conj (generate-route parent parents) pos)
    []))

(defn route [from passable? goal?]
  (loop [q (queue from)
         discovered #{from}
         parents {}]
    (if-let [pos (peek q)]
      (cond
        (goal? pos) (generate-route pos parents)
        (passable? pos) (let [new-positions (get-new-positions discovered pos)
                              new-discovered (set/union discovered (set new-positions))
                              new-queue (into (pop q) new-positions)
                              new-parents (into parents (for [p new-positions]
                                                          [p pos]))]
                          (recur new-queue new-discovered new-parents))
        :else (recur (pop q) discovered parents)))))

(defn flood-fill [from passable?]
  (loop [i -1
         next-positions [from]
         discovered #{from}]
    (if (seq next-positions)
      (let [new-positions (->> (mapcat (partial get-new-positions discovered) next-positions)
                               (filter passable?)
                               (distinct))
            new-discovered (into discovered new-positions)]
        (recur (inc i) new-positions new-discovered))
      i)))
