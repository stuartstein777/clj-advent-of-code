(ns stuartstein777.2022.day2 
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def comparer {"rock"     "paper"
             "scissors" "rock"
             "paper"    "scissors"})

(def scores
  {["rock"     "rock"]     4
   ["rock"     "paper"]    8
   ["rock"     "scissors"] 3
   ["paper"    "rock"]     1
   ["paper"    "paper"]    5
   ["paper"    "scissors"] 9
   ["scissors" "rock"]     7
   ["scissors" "paper"]    2
   ["scissors" "scissors"] 6})

(defn parse-input [input]
 (-> input
     (str/replace #"A|B|C|X|Y|Z"
                  {"A" "rock"
                   "B" "paper"
                   "C" "scissors"
                   "X" "lose" #_"rock"
                   "Y" "draw" #_"paper"
                   "Z" "win" #_"scissors"})
     (str/split-lines)
     (->> (map #(str/split % #" ")))))


;; part 1
(defn play-part1 [rounds]
  (reduce (fn [score round] (+ score (scores round))) 0 rounds))

(->> (parse-input (slurp "puzzle-inputs/2022/day2"))
     play-part1)

;; part 2
(def wld {"lose" #(scores [% ((set/map-invert comparer) %)])
        "draw" #(scores [% %])
        "win"  #(scores [% (comparer %)])})

(defn play-part2 [rounds]
  (reduce (fn [score [p1 res]] 
            (+ score ((wld res) p1))) 0 rounds))

(->> (parse-input (slurp "puzzle-inputs/2022/day2"))
     play-part2)
