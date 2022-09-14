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
    #_(prn biggest)
    (loop [cur-idx (mod (inc idx-biggest) cnt)
           biggest biggest
           current-state (zero-xs-at-idx xs idx-biggest)]
      #_(prn cur-idx current-state)
      (if (= 0 biggest)
        current-state
        (if (zero? biggest)
          current-state
          (recur (mod (inc cur-idx) cnt)
                 (dec biggest)
                 (increment-xs-at-idx current-state cur-idx)))))))


;; part 1
(let [puzzle-input (parse-input)]
  (loop [current-state puzzle-input
         seen-states #{}
         cycles 1]
    (let [highest-idx (get-largest-block-idx current-state)
          new-state (redistribute current-state highest-idx)]
      (if (seen-states new-state)
        cycles
        (recur new-state (conj seen-states new-state) (inc cycles))))))

;; part 2
