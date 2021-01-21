(ns aoc-clj.day-9
  (:require [clojure.core.async :as async :refer [<! <!! >! >!! chan go]]
            [aoc-clj.intcode :as intcode]
            [aoc-clj.util :refer [read-program]]))

(defn run []
  (let [program (read-program "input9.txt")
        cin (chan 1)
        cout (intcode/run program cin)]
    (>!! cin 2)
    (<!! (async/into [] cout))))
