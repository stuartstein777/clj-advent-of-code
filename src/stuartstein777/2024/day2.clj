(ns stuartstein777.2024.day2
  (:require [clojure.string :as str]
            [stuartstein777.utils :as utils]))

(defn parse [line]
  (->> line
       (slurp)
       (str/split-lines)
       (mapv (fn [l] (->>  (str/split l #" ")
                           (mapv (fn [x] (Integer/parseInt x))))))))

(defn is-safe? [record]
  (and (or (apply > record) (apply < record))
       (->> (map #(Math/abs (- %1 %2)) record (rest record))
            (every? (fn [x] (<= 1 x 3))))))

(defn part-one [record]
  (is-safe? record))

(defn part-two [record]
  (let [permutations (map (partial utils/remove-value-at record) (range (count record)))]
    (->> permutations
         (map is-safe?)
         (filter true?)
         (count)
         (not= 0))))

(->>  "puzzle-inputs/2024/day2"
      (parse)
      (mapv part-one)
      (filter true?)
      (count))

(->>  "puzzle-inputs/2024/day2"
      (parse)
      (mapv part-two)
      (filter true?)
      (count))
