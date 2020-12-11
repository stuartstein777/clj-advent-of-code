(ns stuartstein777.2020.day10
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combo]))

(def test-input "16\n10\n15\n5\n1\n11\n7\n19\n6\n12\n4")
(def test-input2 "28\n33\n18\n42\n31\n14\n46\n20\n48\n47\n24\n23\n49\n45\n19\n38\n39\n11\n1\n32\n25\n35\n8\n17\n7\n9\n4\n2\n34\n10\n3")

(defn parse-input [input]
  (as-> (str/split-lines input) o
       (map #(Integer/parseInt %) o)
       (concat [0] o [162])
        (sort o)))

;; part 1
(let [input (as-> (slurp "puzzle-inputs/2020/day10") o
                  (parse-input o)
                  (sort o))
      diffs (->> (map - (rest input) input)
                 (frequencies))]
  (* (diffs 3)
     (diffs 1)))

;; part 2
;; all the ways we can arrange the adapters with gaps of 1 or 3
(time
  (as-> (slurp "puzzle-inputs/2020/day10") o
        (str/split-lines o)
        (map #(Integer/parseInt %) o)
        (sort o)
        (concat [0] o)
        (map - (rest o) o)
        (partition-by identity o)
        (filter #(some #{1} %) o)
        (map #(case (count %) 1 1 2 2 3 4 4 7) o)
        (reduce * 1 o)))