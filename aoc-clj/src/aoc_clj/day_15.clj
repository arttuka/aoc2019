(ns aoc-clj.day-15
  (:require [clojure.core.async :as async :refer [<! <!! >! >!! chan close! go go-loop]]
            [clojure.string :as str]
            [aoc-clj.intcode :as intcode]
            [aoc-clj.route :as route]
            [aoc-clj.util :refer [abs bbox read-program]]))

(defn adjacent-positions [[x y]]
  [[(inc x) y]
   [(dec x) y]
   [x (inc y)]
   [x (dec y)]])

(defn make-passable? [tiles]
  (fn [pos]
    (not= :wall (get tiles pos))))

(def draw-tile {:wall   "#"
                :empty  "."
                :system "o"})

(defn adjacent-unknown [tiles pos]
  (for [p (adjacent-positions pos)
        :when (not (contains? tiles p))]
    p))

(defn get-move [{:keys [pos route]}]
  (let [[x1 y1] pos
        [x2 y2] (first route)]
    (condp = [(- x2 x1) (- y2 y1)]
      [-1 0] 1
      [0 1] 4
      [1 0] 2
      [0 -1] 3)))

(defn draw-map [{:keys [tiles pos]} route]
  (let [[min-x min-y max-x max-y] (bbox (keys tiles))]
    (str/join "\n" (for [y (range min-y (inc max-y))]
                     (str/join (for [x (range min-x (inc max-x))]
                                 (cond
                                   (= pos [x y]) "D"
                                   (contains? route [x y]) "x"
                                   :else (get draw-tile (get tiles [x y]) " "))))))))

(defn step [{:keys [pos route targets tiles system]} output]
  (let [[new-pos & rest-route] route
        next-tiles (assoc tiles new-pos (case output
                                          0 :wall
                                          1 :empty
                                          2 :system))
        next-targets (cond-> (disj targets new-pos)
                             (pos? output) (into (adjacent-unknown next-tiles new-pos)))
        current-pos (if (zero? output) pos new-pos)]
    (when-let [next-route (if (and (pos? output) (seq rest-route))
                            rest-route
                            (route/route current-pos (make-passable? next-tiles) next-targets))]
      {:pos     current-pos
       :route   next-route
       :targets next-targets
       :tiles   next-tiles
       :system  (or system (when (= 2 output) current-pos))})))

(defn search-map [cin cout]
  (let [states-c (chan 10000)
        initial-targets (set (adjacent-positions [0 0]))
        initial-route [(first initial-targets)]
        initial-state {:pos     [0 0]
                       :route   initial-route
                       :targets initial-targets
                       :tiles   {[0 0] :empty}}]
    (go
      (>! states-c initial-state)
      (>! cin (get-move initial-state))
      (loop [state initial-state
             output (<! cout)]
        (if-let [next-state (step state output)]
          (do
            (>! states-c next-state)
            (>! cin (get-move next-state))
            (recur next-state (<! cout)))
          (close! states-c))))
    states-c))

(defn run []
  (let [program (read-program "input15.txt")
        cin (chan 100)
        cout (intcode/run program cin)
        states (search-map cin cout)
        final-state (last (<!! (async/into [] states)))
        route (set (route/route [0 0] (make-passable? (:tiles final-state)) #{(:system final-state)}))]
    (println (draw-map (assoc final-state :pos [0 0]) route))
    (println (:system final-state))
    (count (route/route [0 0] (make-passable? (:tiles final-state)) #{(:system final-state)}))
    (println (route/flood-fill (:system final-state) (make-passable? (:tiles final-state))))))
