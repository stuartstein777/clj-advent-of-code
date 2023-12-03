(ns stuartstein777.2022.day3 
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def test-input
  "vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw")

(defn score-character [c]
  (if (Character/isUpperCase c)
    (- (int c) 38)
    (- (int c) 96)))

(defn split-compartments [s]
  (let [compartment-size (/ (count s) 2)]
    [(subs s 0 compartment-size)
     (subs s compartment-size)]))

(defn score [xs]
  (reduce (fn [acc i] (+ acc (score-character i))) 0 xs))

(defn find-common [xs]
  (->> (map (partial into #{}) xs)
       (apply set/intersection)))

;; part 1
(->> (slurp "puzzle-inputs/2022/day3")
     str/split-lines
     (map split-compartments)
     (mapcat find-common)
     score)

;; part 2
(->> (slurp "puzzle-inputs/2022/day3")
     str/split-lines
     (partition 3)
     (mapcat find-common)
     score)
