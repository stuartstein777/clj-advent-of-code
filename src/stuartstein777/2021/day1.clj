(ns stuartstein777.2021.day1
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [stuartstein777.file :as file]))

(defn calculate-increasing [xs]
  (->> (map < xs (rest xs))
       (filter true?)
       (count)))

(defn get-and-parse-input []
  (->> (slurp "puzzle-inputs/2021/day1")
       (str/split-lines)
       (map #(Integer/parseInt %))))

;; Part 1
(let [inputs (get-and-parse-input)]
  (calculate-increasing inputs))


;; Part 2
(let [inputs (->> (get-and-parse-input)
                  (partition 3 1)
                  (map #(reduce + %)))]
  (calculate-increasing inputs))

