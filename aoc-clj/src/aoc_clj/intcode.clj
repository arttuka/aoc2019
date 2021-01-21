(ns aoc-clj.intcode
  (:require [clojure.core.async :refer [<! <!! >! chan close! go go-loop]]
            [aoc-clj.util :refer [indexed]]))

(defn get-modes [v n]
  (if (zero? n)
    []
    (let [mode (case (rem v 10)
                 0 ::position
                 1 ::immediate)]
      (cons mode (get-modes (quot v 10) (dec n))))))

(defn get-params [program pointer modes]
  (for [[i mode] (indexed modes)
        :let [v (get program (+ pointer 1 i))]]
    (case mode
      ::position {:val (get program v)
                  :pos v}
      ::immediate {:val v})))

(defn get-instruction [program pointer]
  (let [opcode (get program pointer)
        [op n] (case (rem opcode 100)
                 1 [::add 3]
                 2 [::mult 3]
                 3 [::input 1]
                 4 [::output 1]
                 5 [::jnz 2]
                 6 [::jz 2]
                 7 [::lt 3]
                 8 [::eq 3]
                 99 [::exit 0])
        modes (get-modes (quot opcode 100) n)]
    {:op     op
     :params (get-params program pointer modes)}))

(defn step [{:keys [program pointer cin cout] :as state}]
  (go
    (let [{:keys [op params]} (get-instruction program pointer)
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
                                         ::input [(:pos (first params)) (<! cin)]
                                         ::lt (let [[p1 p2 out] params]
                                                [(:pos out) (if (< (:val p1) (:val p2)) 1 0)])
                                         ::eq (let [[p1 p2 out] params]
                                                [(:pos out) (if (= (:val p1) (:val p2)) 1 0)])
                                         nil)]
                        (assoc program pos v)
                        program)]
      (when (= ::output op)
        (>! cout (:val (first params))))
      (if (= ::exit op)
        ::exit
        (assoc state :pointer new-pointer :program new-program)))))

(defn ^:private run-program [state]
  (go-loop [state state]
    (let [new-state (<! (step state))]
      (condp = new-state
        nil (do
              (println "Error while running")
              state)
        ::exit state
        (recur new-state)))))

(defn run [program cin]
  (let [cout (chan 100)]
    (go
      (<! (run-program {:program program
                        :pointer 0
                        :cin     cin
                        :cout    cout}))
      (close! cout))
    cout))
