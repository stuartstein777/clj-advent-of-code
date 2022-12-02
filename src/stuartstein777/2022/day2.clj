(ns stuartstein777.2022.day2 
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def test-input "A Y\nB X\nC Z\n")

(def scorer {"rock" 1, "paper" 2, "scissors", 3})

(def comparer {"rock"     "paper"
             "scissors" "rock"
             "paper"    "scissors"})

(defn score-round [[p1 p2]]
  (+ (scorer p2)
     (cond
       (= p1 p2) 3
       (= p2 (comparer p1)) 6
       :else 0)))

(defn play-part1 [rounds]
  (reduce (fn [score round] (+ score (score-round round))) 0 rounds))


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
(->> (parse-input (slurp "puzzle-inputs/2022/day2"))
     play-part1)

;; part 2
(defn play-part2 [rounds]
  (reduce (fn [score [p1 res]] 
            (+ score (condp = res
                     "lose"
                     (score-round [p1 ((set/map-invert comparer) p1)])
                     
                     "draw"
                     (score-round [p1 p1])
                     
                     (score-round [p1 (comparer p1)])))) 0 rounds))

(->> (parse-input (slurp "puzzle-inputs/2022/day2"))
     play-part2)