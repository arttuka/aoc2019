(ns aoc-clj.day-5
  (:require [clojure.core.async :as async :refer [<! <!! >! >!! chan go]]
            [aoc-clj.intcode :as intcode]
            [aoc-clj.util :refer [read-program]]))

(defn run []
  (let [program (read-program "input5.txt")
        cin (chan 1)
        cout (intcode/run program cin)]
    (>!! cin 5)
    (<!! (async/into [] cout))))
