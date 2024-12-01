(ns stuartstein777.2024.day1
  (:require [clojure.string :as str]))

(defn parse-input [lines]
  (let [xs (->>
            (str/split-lines lines)
            (map #(str/split % #"   "))
            (mapv (fn [[x y]] [(Integer/parseInt x) (Integer/parseInt y)])))]
    
    [(sort (mapv first xs))
     (sort (mapv second xs))]))

(defn solve-part1 [[xs ys]]
  (->> (map (fn [x y] (Math/abs (- x y))) xs ys)
       (reduce +)))

(defn reducer [freqs acc i]
  (+ acc (* (freqs i 0) i)))

(defn solve-part2 [[xs ys]]
  (let [ys-freqs (frequencies ys)]
    (reduce (partial reducer ys-freqs) 0 xs)))

(->> "puzzle-inputs/2024/day1"
     (slurp)
     (parse-input)
     #_(solve-part1)
     (solve-part2))
