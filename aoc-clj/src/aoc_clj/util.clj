(ns aoc-clj.util
  (:require [clojure.core.async :refer [<! >! chan close! go go-loop mult tap]]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (clojure.lang PersistentQueue)
           (clojure.core.async.impl.channels ManyToManyChannel)))

(defn read-program [input]
  (-> (io/resource input)
      (slurp)
      (str/split #",")
      (->> (mapv #(Long/parseLong %)))))

(defn indexed [coll]
  (map-indexed vector coll))

(defn zip [& colls]
  (apply map vector colls))

(defn transpose [coll]
  (apply map vector coll))

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

(defn find-where [pred coll]
  (first (filter pred coll)))

(defn unfold [f init]
  (when-let [[x acc] (f init)]
    (cons x (lazy-seq (unfold f acc)))))

(defn prefix? [prefix coll]
  (every? (partial apply =) (zip prefix (concat coll (repeat (Object.))))))

(defn count-pattern [pattern coll]
  (let [length (count pattern)]
    (loop [n 0
           coll coll]
      (cond
        (empty? coll) n
        (prefix? pattern coll) (recur (inc n) (drop length coll))
        :else (recur n (rest coll))))))

(defn replace-pattern [pattern replacement coll]
  (cond
    (empty? coll) nil
    (prefix? pattern coll) (cons replacement (replace-pattern pattern replacement (drop (count pattern) coll)))
    :else (cons (first coll) (replace-pattern pattern replacement (rest coll)))))

(defn split-on [pred coll]
  (when (seq coll)
    (let [[start [_ & end]] (split-with (complement pred) coll)]
      (cons start (lazy-seq (split-on pred end))))))

(defn process-chan
  ([c f]
   (process-chan c f (fn [])))
  ([c f end-f]
   (go-loop []
     (if-some [x (<! c)]
       (let [ret (f x)]
         (when (instance? ManyToManyChannel ret)
           (<! ret))
         (recur))
       (end-f)))))

(defn binary-search [f left right]
  (go-loop [left left
            right right]
    (when (<= left right)
      (let [m (quot (+ left right) 2)
            r (<! (f m))]
        (cond
          (pos? r) (recur left (dec m))
          (neg? r) (recur (inc m) right)
          (zero? r) m)))))

(defn and'
  ([] true)
  ([x] x)
  ([x y] (and x y)))
