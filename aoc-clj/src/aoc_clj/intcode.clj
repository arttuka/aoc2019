(ns aoc-clj.intcode
  (:require [clojure.core.async :refer [<! <!! >! alts! chan close! go go-loop]]
            [aoc-clj.util :refer [indexed]]))

(defn get-modes [v n]
  (if (zero? n)
    []
    (let [mode (case (rem v 10)
                 0 ::position
                 1 ::immediate
                 2 ::relative)]
      (cons mode (get-modes (quot v 10) (dec n))))))

(defn get-params [{:keys [program pointer base]} modes]
  (for [[i mode] (indexed modes)
        :let [v (get program (+ pointer 1 i) 0)
              pos (case mode
                    ::position v
                    ::immediate nil
                    ::relative (+ v base))]]
    {:val (if pos (get program pos 0) v)
     :pos pos}))

(defn get-instruction [{:keys [program pointer] :as state}]
  (let [opcode (get program pointer 0)
        [op n] (case (rem opcode 100)
                 1 [::add 3]
                 2 [::mult 3]
                 3 [::input 1]
                 4 [::output 1]
                 5 [::jnz 2]
                 6 [::jz 2]
                 7 [::lt 3]
                 8 [::eq 3]
                 9 [::base 1]
                 99 [::exit 0])
        modes (get-modes (quot opcode 100) n)]
    {:op     op
     :params (get-params state modes)}))

(defn update-program [program pos v]
  (let [new-program (if (<= (count program) pos)
                      (vec (concat program (repeat (- pos (count program)) 0)))
                      program)]
    (assoc new-program pos v)))

(defn step [{:keys [program pointer cin cout base mode] :as state}]
  (go
    (let [{:keys [op params]} (get-instruction state)
          new-pointer (case op
                        ::jnz (let [[v pos] params]
                                (if-not (zero? (:val v))
                                  (:val pos)
                                  (+ pointer 3)))
                        ::jz (let [[v pos] params]
                               (if (zero? (:val v))
                                 (:val pos)
                                 (+ pointer 3)))
                        (+ pointer 1 (count params)))
          new-program (if-let [[pos v] (case op
                                         ::add (let [[p1 p2 out] params]
                                                 [(:pos out) (+ (:val p1) (:val p2))])
                                         ::mult (let [[p1 p2 out] params]
                                                  [(:pos out) (* (:val p1) (:val p2))])
                                         ::input [(:pos (first params)) (if (= :nic mode)
                                                                          (first (alts! [cin] :default -1))
                                                                          (<! cin))]
                                         ::lt (let [[p1 p2 out] params]
                                                [(:pos out) (if (< (:val p1) (:val p2)) 1 0)])
                                         ::eq (let [[p1 p2 out] params]
                                                [(:pos out) (if (= (:val p1) (:val p2)) 1 0)])
                                         nil)]
                        (update-program program pos v)
                        program)
          new-base (+ base (if (= ::base op)
                             (:val (first params))
                             0))]
      (when (= ::output op)
        (>! cout (:val (first params))))
      (if (= ::exit op)
        ::exit
        (assoc state
          :pointer new-pointer
          :program new-program
          :base new-base)))))

(defn ^:private run-program [state]
  (go-loop [state state]
    (let [new-state (<! (step state))]
      (condp = new-state
        nil (do
              (println "Error while running")
              state)
        ::exit state
        (recur new-state)))))

(defn run [program cin & [mode]]
  (let [cout (chan 100)]
    (go
      (<! (run-program {:program program
                        :pointer 0
                        :base    0
                        :cin     cin
                        :cout    cout
                        :mode    mode}))
      (close! cout))
    cout))
