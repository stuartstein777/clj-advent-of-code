(ns stuartstein777.2017.day6
  (:require [clojure.string :as str]
            [stuartstein777.utils :as u :refer [fsec]]))

(defn parse-input []
  (as-> (slurp "puzzle-inputs/2017/day6") o
    (str/trim-newline o)
    (str/split o #"\t")
    (mapv #(Integer/parseInt %) o)))

(defn get-largest-block-idx [xs]
  (loop [biggest-idx 0
         biggest (first xs)
         idx 1]
    (if (= idx (count xs))
      biggest-idx
      (if (> (nth xs idx) biggest)
        (recur idx (nth xs idx) (inc idx))
        (recur biggest-idx biggest (inc idx))))))

(defn increment-xs-at-idx [xs idx]
  (update xs idx inc))

(defn zero-xs-at-idx [xs idx]
  (assoc xs idx 0))

(defn redistribute [xs idx-biggest]
  (let [biggest (nth xs idx-biggest)
        cnt (count xs)]
    (loop [cur-idx (mod (inc idx-biggest) cnt)
           biggest biggest
           current-state (zero-xs-at-idx xs idx-biggest)]
      (if (= 0 biggest)
        current-state
        (if (zero? biggest)
          current-state
          (recur (mod (inc cur-idx) cnt)
                 (dec biggest)
                 (increment-xs-at-idx current-state cur-idx)))))))


;; part 1 & part 2 (soln for part 1 is first item in vector, part 2 is second)
(let [puzzle-input (parse-input)]
  (loop [current-state puzzle-input
         seen-states {current-state 0}
         cycles 1]
    (let [highest-idx (get-largest-block-idx current-state)
          new-state (redistribute current-state highest-idx)]
      (if (seen-states new-state)
        [cycles (- cycles (seen-states new-state))]
        (recur new-state (assoc seen-states new-state cycles) (inc cycles))))))
