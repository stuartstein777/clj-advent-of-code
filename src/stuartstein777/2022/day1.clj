(ns stuartstein777.2022.day1
  (:require [clojure.string :as str]))

(defn parse-input []
  (->>
   (str/split (slurp "puzzle-inputs/2022/day1") #"\n\n")
   (map str/split-lines)
   (map #(map parse-long %))))

;; part 1
(->> (parse-input)
     (map #(reduce + %))
     (apply max))

;; part 2
(->> (parse-input)
     (map #(reduce + %))
     (sort >)
     (take 3)
     (reduce +))
