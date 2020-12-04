(ns stuartstein777.2019.day8
  (:require [clojure.string :as str]))

;; part 1
(defn count-zeroes [partition]
  (count (filter #(= \0 %) partition)))

(defn solve []
  (let [input               (partition 150 (slurp "puzzle-inputs/2019/day8"))
        zero-counts         (map count-zeroes input)
        smallest-zero-count (apply min zero-counts)
        index               (.indexOf zero-counts smallest-zero-count)]
    (* (count (filter #(= \2 %) (nth input index)))
       (count (filter #(= \1 %) (nth input index))))))

(solve)

;; part 2
(defn get-pixel [coll loc]
  (let [pixel (nth (first coll) loc)]
    (if (= pixel \2)
      (recur (rest coll) loc)
      (if (= pixel \1) \▓ \░))))

(as-> (slurp "puzzle-inputs/2019/day8") o
      (partition 150 o)
      (map (partial get-pixel o) (range 150))
      (partition 25 o)
      (map (partial apply str) o)
      (str/join "\n" o))