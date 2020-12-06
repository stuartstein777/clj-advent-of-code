(ns stuartstein777.2020.day6
  (:require [clojure.string :as str]))

;; part 1
(defn count-answers [s]
  (->> (mapcat #(str/split % #"") s)
       (into #{})
       (count)))

;; part 2
(defn count-answers-2 [s]
  (let [num-people (count s)
        combined (apply str s)
        answer-frequencies (frequencies combined)]
    (->> (filter (fn [[_ v]] (= num-people v)) answer-frequencies)
         (count))))

;; shared
(as-> (slurp "puzzle-inputs/2020/day6") o
      (str/split o #"\R\R")
      (map #(str/split % #"\n") o)
      (map count-answers-2 o)
      #_(map count-answers o)
      (reduce + o))