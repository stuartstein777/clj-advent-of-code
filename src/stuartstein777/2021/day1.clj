(ns stuartstein777.2021.day1
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [stuartstein777.file :as f]))

(defn calculate-increasing [xs]
  (->> (map < xs (rest xs))
       (filter true?)
       (count)))

;; Part 1
(->> (f/read-all-lines-and-parse "puzzle-inputs/2021/day1" (fn [n] (Integer/parseInt n)))
     calculate-increasing)

;; Part 2
(->> (f/read-all-lines-and-parse "puzzle-inputs/2021/day1" (fn [n] (Integer/parseInt n)))
     (partition 3 1)
     (map #(reduce + %))
     calculate-increasing)
