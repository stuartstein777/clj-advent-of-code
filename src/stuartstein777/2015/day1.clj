(ns stuartstein777.2015.day1
  (:require [clojure.string :as str]))

;; part 1
(let [c (->> (slurp "puzzle-inputs/2015/day1")
             (str/trim)
             (sort)
             (partition-by identity))]
  (- (count (first c)) (count (second c))))

;; part 2
(let [c (->> (slurp "puzzle-inputs/2015/day1")
             (str/trim)
             (map (fn [p] (if (= p \() 1 -1)))
             (reductions +))]
  (->> (take-while #(not= -1 %) c)
       (count)
       (inc)))