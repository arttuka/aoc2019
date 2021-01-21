(ns aoc-clj.day-7
  (:require [clojure.core.async :as async :refer [<! <!! >! >!! chan close! go mult pipe tap]]
            [clojure.math.combinatorics :as combo]
            [aoc-clj.intcode :as intcode]
            [aoc-clj.util :refer [read-program split-chan zip]]))

(defn run-with-settings [program settings]
  (let [cins (repeatedly 5 #(chan 100))
        couts (for [cin cins]
                (intcode/run program cin))
        first-cin (first cins)
        [last-cout-1 last-cout-2] (split-chan (last couts))]
    (doseq [[cin setting] (zip cins settings)]
      (>!! cin setting))
    (doseq [[cout cin] (zip couts (rest cins))]
      (pipe cout cin))
    (pipe last-cout-1 first-cin)
    (>!! first-cin 0)
    (last (<!! (async/into [] last-cout-2)))))

(defn run []
  (let [program (read-program "input7.txt")
        all-settings (combo/permutations (range 5 10))
        outputs (map (partial run-with-settings program) all-settings)]
    (reduce max outputs)))
