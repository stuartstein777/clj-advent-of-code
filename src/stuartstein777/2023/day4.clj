(ns stuartstein777.2023.day4
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-line [line]
  (->> (str/split line #":|\|")
       (rest)
       (map (fn [s] (-> s (str/replace #"  " " ")
                       (str/trim))))
       (map #(str/split % #" "))
       (map #(map parse-long %))
       (mapv set)))

(parse-line "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53")

(defn score-line [[winning-numbers numbers]]
  (let [winners (set/intersection  winning-numbers numbers)]
    (if (> (count winners) 0)
      (Math/pow 2 (dec (count winners)))
      0)))

(score-line [#{41 48 83 86 17} #{83 86 6 31 17 9 48 53}])

(defn parse-input [input]
  (->> input
       slurp
       str/split-lines
       (mapv parse-line)
       (map score-line)
       (reduce +)))

(parse-input "puzzle-inputs/2023/day4")

;; part 2

