(ns stuartstein777.2021.day1
  (:require [stuartstein777.file :as f]))

(def puzzle-input "puzzle-inputs/2021/day1")

(defn calculate-increasing [xs]
  (->> (map < xs (rest xs))
       (filter true?)
       (count)))

(defn parser [n]
  (Integer/parseInt n))

;; Part 1
(->> (f/read-all-lines-and-parse puzzle-input parser)
     calculate-increasing)

;; Part 2
(->> (f/read-all-lines-and-parse puzzle-input parser)
     (partition 3 1)
     (map #(reduce + %))
     calculate-increasing)