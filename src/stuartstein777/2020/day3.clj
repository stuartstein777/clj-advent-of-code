(ns stuartstein777.2020.day3
  (:require [clojure.string :as str]))

; part 1
(defn is-tree? [x i]
  (= \# (nth i (mod x (count i)))))

(defn count-trees [[x-step y-step]]
  (let [input (->> (slurp "puzzle-inputs/2020/day3")
                   (str/split-lines))]
    (->> (take-nth y-step input)
         (map is-tree? (iterate #(+ x-step %) 0))
         (filter true?)
         (count))))

(count-trees [3 1])

; part 2
(->> (map count-trees [[1 1] [3 1] [5 1] [7 1] [1 2]])
     (reduce *))