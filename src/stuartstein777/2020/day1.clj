(ns stuartstein777.2020.day1
  (:require [clojure.string :as str]))

;; part 1
(time
  (let [puzzle-input (->> (slurp "puzzle-inputs/2020-day1-1")
                          (str/split-lines)
                          (map #(Integer/parseInt %))
                          (sort <))]
    (for [xs puzzle-input
          ys puzzle-input
          :while (< xs 2020)
          :let [s (+ xs ys)]
          :when (= s 2020)]
      (reduce * [xs ys]))))

;; part 2
(time
  (let [puzzle-input (->> (slurp "puzzle-inputs/2020/day1")
                          (str/split-lines)
                          (map #(Integer/parseInt %))
                          (sort <))]
    (for [x puzzle-input
          y puzzle-input
          z puzzle-input
          :while (< (+ x y) 2020)
          :let [s (+ x y z)]
          :while (<= s 2020)
          :when (= s 2020)]
      (reduce * [x y z]))))