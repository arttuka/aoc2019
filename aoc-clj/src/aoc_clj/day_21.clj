(ns aoc-clj.day-21
  (:require [clojure.core.async :as async :refer [<! <!! >! >!! chan go onto-chan!]]
            [clojure.string :as str]
            [aoc-clj.intcode :as intcode]
            [aoc-clj.util :refer [read-program]]))

(def sc-program (str/join \newline
                          ["NOT A T"
                           "OR T J"
                           "NOT B T"
                           "OR T J"
                           "NOT C T"
                           "OR T J"
                           "AND D J"
                           "OR J T"
                           "AND I T"
                           "OR F T"
                           "AND E T"
                           "OR H T"
                           "AND T J"
                           "RUN\n"]))

(defn run []
  (let [program (read-program "input21.txt")
        cin (chan 100)
        cout (intcode/run program cin)]
    (onto-chan! cin (map int sc-program))
    (let [output (<!! (async/into [] cout))]
      (println (str/join (map char (filter #(< % 256) output))))
      (println (filter #(< 255 %) output)))))
