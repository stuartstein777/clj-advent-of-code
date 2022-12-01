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


;; Cookie: session=53616c7465645f5f2b7c538f828989bb141a30abdcc3adb75292fe131dbb75050371fd5081da44291b1b200d1e16a8e7895c94a716b5b53972dc23c8dcc3c667
