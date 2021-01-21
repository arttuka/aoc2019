(ns aoc-clj.intcode)

(defn get-params [program pointer n]
  (subvec program (inc pointer) (+ pointer 1 n)))

(defmulti run-instruction (fn [{:keys [program pointer]}]
                              (get program pointer)))

(defmethod run-instruction 1
  [{:keys [program pointer]}]
  (let [[in1 in2 out] (get-params program pointer 3)]
    {:program (assoc program out (+ (get program in1) (get program in2)))
     :pointer (+ pointer 4)}))

(defmethod run-instruction 2
  [{:keys [program pointer]}]
  (let [[in1 in2 out] (get-params program pointer 3)]
    {:program (assoc program out (* (get program in1) (get program in2)))
     :pointer (+ pointer 4)}))

(defmethod run-instruction 99
  [_]
  ::exit)

(defn ^:private run-program [state]
  (let [next (run-instruction state)]
    (if (= ::exit next)
      state
      (recur (merge state next)))))

(defn run [program]
  (run-program {:program program
                :pointer 0}))
