(ns aoc-clj.day-17
  (:require [clojure.core.async :as async :refer [<! <!! >! >!! chan close! go go-loop onto-chan!]]
            [clojure.string :as str]
            [aoc-clj.intcode :as intcode]
            [aoc-clj.util :refer [count-pattern find-where indexed read-program
                                  replace-pattern split-on unfold zip]]))

(defn end [{:keys [start length]}]
  (dec (+ start length)))

(defn scaffold? [c]
  (not= \. c))

(defn intersection [s1 s2]
  (let [x (:line s2)
        y (:line s1)]
    (when (and (< (:start s1) x (end s1))
               (< (:start s2) y (end s2)))
      [x y])))

(defn horizontal-dir? [dir]
  (or (= :right dir) (= :left dir)))

(defn turn [d1 d2]
  (condp = [d1 d2]
    [:left :up] :right
    [:up :right] :right
    [:right :down] :right
    [:down :left] :right
    :left))

(defn get-scaffolds [view horizontal?]
  (for [[n line] (indexed view)
        :let [groups (partition-by scaffold? line)
              lengths (map count groups)
              starts (reductions + 0 lengths)]
        [g l s] (zip groups lengths starts)
        :when (and (scaffold? (first g)) (< 1 l))]
    {:line        n
     :start       s
     :length      l
     :horizontal? horizontal?}))

(defn get-start [view]
  (let [start-direction {\< :left
                         \^ :up
                         \v :down
                         \> :right}
        start? (set (keys start-direction))]
    (first (for [[y line] (indexed view)
                 [x c] (indexed line)
                 :when (start? c)]
             {:x   x
              :y   y
              :dir (start-direction c)}))))

(defn get-next-scaffold [prev-line prev-pos scaffolds]
  (let [next? (fn [s]
                (and (= prev-pos (:line s))
                     (or (= prev-line (:start s))
                         (= prev-line (end s)))))
        direction (fn [s]
                    (condp = [(:horizontal? s) (= prev-line (:start s))]
                      [true true] :right
                      [true false] :left
                      [false true] :down
                      [false false] :up))]
    (some-> (find-where next? scaffolds) ((juxt identity direction)))))

(defn get-movements [h-scaffolds v-scaffolds start]
  (let [get-next-movement (fn [{:keys [dir x y]}]
                            (when-let [[s next-dir] (if (horizontal-dir? dir)
                                                      (get-next-scaffold y x v-scaffolds)
                                                      (get-next-scaffold x y h-scaffolds))]
                              (let [[x' y'] (case next-dir
                                              :right [(end s) (:line s)]
                                              :left [(:start s) (:line s)]
                                              :up [(:line s) (:start s)]
                                              :down [(:line s) (end s)])]
                                [{:turn   (turn dir next-dir)
                                  :length (dec (:length s))}
                                 {:dir next-dir
                                  :x   x'
                                  :y   y'}])))]
    (unfold get-next-movement start)))

(defn find-functions [movements]
  (let [find-pattern-step (fn [n prev-cnt coll]
                            (let [pattern (take n coll)
                                  curr-cnt (* n (count-pattern pattern coll))]
                              (cond
                                (= n (count coll)) nil
                                (< curr-cnt prev-cnt) (split-at (dec n) coll)
                                :else (recur (inc n) curr-cnt coll))))
        find-next-pattern (fn [coll]
                            (when (seq coll)
                              (find-pattern-step 1 0 coll)))]
    (distinct (unfold find-next-pattern movements))))

(defn apply-functions [functions movements]
  (reduce (fn [movements [c f]]
            (replace-pattern f (char c) movements))
          movements
          (zip (range (int \A) (int \D)) functions)))

(defn draw-map [view intersections]
  (str/join \newline
            (for [[y line] (indexed view)]
              (str/join (for [[x c] (indexed line)]
                          (if (contains? intersections [x y])
                            \O
                            c))))))

(defn print-movement [m]
  (if (char? m)
    (str m)
    (str (case (:turn m)
           :left "L"
           :right "R")
         \,
         (:length m))))

(defn ->input [code]
  (map int (str (str/join \, (map print-movement code)) \newline)))

(defn read-map [cout]
  (go-loop [prev nil
            x (<! cout)
            acc []]
    (if (and (= prev 10) (= x 10))
      (split-on #{\newline} (map char acc))
      (recur x (<! cout) (if prev (conj acc prev) acc)))))

(defn print-channel [c]
  (go-loop [x (<! c)]
    (when x
      (print (if (< x 256)
               (char x)
               (str x)))
      (recur (<! c)))))

(defn run []
  (let [program (assoc (read-program "input17.txt") 0 2)
        cin (chan 100)
        cout (intcode/run program cin)]
    (<!! (go
           (let [view (<! (read-map cout))
                 h-scaffolds (get-scaffolds view true)
                 v-scaffolds (get-scaffolds (apply map vector view) false)
                 start (get-start view)
                 movements (get-movements h-scaffolds v-scaffolds start)
                 functions (find-functions movements)
                 code (apply-functions functions movements)]
             (println (draw-map view nil))
             (doseq [x (->input code)]
               (>! cin x))
             (doseq [f functions
                     x (->input f)]
               (>! cin x))
             (>! cin 110)
             (>! cin 10)
             (last (<! (async/into [] cout))))))))