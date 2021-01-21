(ns aoc-clj.util
  (:require [clojure.core.async :refer [<! >! chan close! go mult tap]]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (clojure.lang PersistentQueue)))

(defn read-program [input]
  (-> (io/resource input)
      (slurp)
      (str/split #",")
      (->> (mapv #(Long/parseLong %)))))

(defn indexed [coll]
  (map-indexed vector coll))

(defn zip [& colls]
  (apply map vector colls))

(defn split-chan [c]
  (let [c1 (chan 100)
        c2 (chan 100)
        m (mult c)]
    (tap m c1)
    (tap m c2)
    [c1 c2]))

(defn async-reductions [f init c]
  (let [cout (chan 100)]
    (go
      (>! cout init)
      (loop [acc init
             x (<! c)]
        (if x
          (let [next-acc (f acc x)]
            (>! cout next-acc)
            (recur next-acc (<! c)))
          (close! cout))))
    cout))

(defn bbox [positions]
  (let [[xs ys] (apply zip positions)
        min-x (reduce min xs)
        min-y (reduce min ys)
        max-x (reduce max xs)
        max-y (reduce max ys)]
    [min-x min-y max-x max-y]))

(defn abs [x]
  (if (neg? x) (- x) x))

(defn queue [& xs]
  (into PersistentQueue/EMPTY xs))
