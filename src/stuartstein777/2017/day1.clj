(ns stuarts.2017day1
  (:require [clojure.string :as str]))

(defn parse-input []
  (as-> (slurp "puzzle-inputs/2017/day1") o
        (str/split o #"")
        (map #(Integer/parseInt %) o)))

;; part 1
(let [input (parse-input)
      cycled (conj input (last input))]
  (->> (map (fn [a b] (if (= a b) a 0)) cycled (rest cycled))
       (reduce +)))

;; part 2
(let [input (parse-input)
    halfway (/ (count input) 2)
    split (concat (drop halfway input) (take halfway input))]
  (->> (map (fn [a b] (if (= a b) a 0)) input split)
       (reduce +)))
