(ns aoc-clj.util
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn read-program [input]
  (-> (io/resource input)
      (slurp)
      (str/split #",")
      (->> (mapv #(Long/parseLong %)))))
