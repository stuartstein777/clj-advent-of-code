(ns stuartstein777.2022.day4 
  (:require [clojure.string :as str]))

(def test-input
  "2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8")

(defn parse-block [[a b]]
  [(->> (str/split a #"-")
        (map parse-long))
   (->> (str/split b #"-")
        (map parse-long))])

(defn fully-contains? [[[a1 a2] [b1 b2]]]
  (or
   (and (>= a1 b1) (<= a2 b2))
   (and (>= b1 a1) (<= b2 a2))))

(defn overlaps? [ab]
  (let [[[a1 a2] [b1 b2]] (sort-by first ab)]
    (or
     (and (<= a1 b1) (>= a2 b2))
     (and (>= a1 b1) (>= a2 b2))
     (and (<= a1 b1) (>= a2 b1)))))

(defn parse [s]
  (->> s
       str/split-lines
       (map #(str/split % #","))
       (map parse-block)))

;; part 1
(->> #_test-input
     (slurp "puzzle-inputs/2022/day4")
     (parse)
     (filter fully-contains?)
     (count))

;; part 2
(->> #_test-input
     (slurp "puzzle-inputs/2022/day4")
     (parse)
     (filter overlaps?)
     (count))