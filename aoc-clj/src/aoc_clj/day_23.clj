(ns aoc-clj.day-23
  (:require [clojure.core.async :as async :refer [<! <!! >! >!! alts! chan close! go go-loop pipe timeout]]
            [aoc-clj.intcode :as intcode]
            [aoc-clj.util :refer [indexed read-program]]))

(defn make-packet [[dest x y]]
  {:dest dest
   :x    x
   :y    y})

(defn make-packet-chan [c]
  (let [pc (chan 100 (comp (partition-all 3) (map make-packet)))]
    (pipe c pc)
    pc))

(defn route-packets [cins cout natc]
  (go-loop [nat-packet nil]
    (let [tc (timeout 500)
          [packet c] (alts! [cout tc])
          timeout? (= c tc)]
      (when (or packet timeout?)
        (let [{:keys [dest x y]} (if timeout?
                                   nat-packet
                                   packet)]
          (cond
            (and (not timeout?)
                 (not= 255 dest)) (doto (get cins dest)
                                    (>! x)
                                    (>! y))
            (and timeout?
                 nat-packet) (do
                               (doto (get cins 0)
                                 (>! x)
                                 (>! y))
                               (>! natc y)))
          (recur (if (= 255 dest)
                   (dissoc packet :dest)
                   nat-packet)))))))

(defn run []
  (let [program (read-program "input23.txt")
        cins (vec (repeatedly 50 #(chan 100)))
        couts (mapv #(make-packet-chan (intcode/run program % :nic)) cins)
        cout (async/merge couts 100)
        natc (chan 100)]
    (route-packets cins cout natc)
    (go
      (doseq [[i cin] (indexed cins)]
        (>! cin i)))
    (<!! (go-loop [prev nil]
           (let [v (<! natc)]
             (println v "sent by NAT")
             (if (= v prev)
               (do
                 (println v "sent twice by NAT")
                 (run! close! couts)
                 (close! natc))
               (recur v)))))))
