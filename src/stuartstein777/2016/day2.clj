(ns stuartstein777.2016.day2
  (:require [clojure.string :as str]))

;; part 1
(def keypad
  {[0 0] 5
   [-1 1] 1
   [0 1] 2
   [1 1] 3
   [-1 0] 4
   [1 0] 6
   [-1 -1] 7
   [0 -1] 8
   [1 -1] 9})

(def invalid-cells
  #{[-1 2] [0 2] [1 2] [-2 1] [-2 0] [-2 -1] [-1 -2] [0 -2] [1 -2] [2 -1] [2 0] [2 1]})

;; part 2
(def pt2-keypad
  {[2 2] 1
   [1 1] 2 [2 1] 3 [3 1] 4
   [0 0] 5 [1 0] 6 [2 0] 7 [3 0] 8 [4 0] 9
   [1 -1] \A [2 -1] \B [3 -1] \C
   [2 -2] \D})

(def pt2-invalid-cells
  #{[2 3] [1 2] [0 1] [-1 0] [0 -1] [1 -2] [2 -3] [3 2] [4 1] [5 0] [4 -1] [3 -2]})

;; shared
(defn get-next-button [acc row]
  (reduce (fn [[x y] i]
            (let [[nx ny] (cond (= i "U") [x (inc y)]
                                (= i "D") [x (dec y)]
                                (= i "R") [(inc x) y]
                                (= i "L") [(dec x) y])]
              (if (invalid-cells [nx ny])
                [x y]
                [nx ny]))) acc row))

(let [input (->> (slurp "puzzle-inputs/2016/day2")
                 (str/split-lines)
                 (map #(str/split % #"")))]
  (->> (reductions get-next-button [0 0] input)
       (rest)
       (map keypad)
       (apply str)))