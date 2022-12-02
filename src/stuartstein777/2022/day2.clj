(ns stuartstein777.2022.day2 
  (:require [clojure.string :as str]
            [clojure.set :as set]))

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

(defn play [rounds]
  (reduce (fn [acc i] (+ acc (score-round i))) 0 rounds))


(defn parse-input [input]
 (-> input
     (str/replace #"A|B|C|X|Y|Z"
                  {"A" "rock"
                   "B" "paper"
                   "C" "scissors"
                   "X" "lose"
                   "Y" "draw"
                   "Z" "win"})
     (str/split-lines)
     (->> (map #(str/split % #" ")))))


;; part 1
(->> (parse-input (slurp "puzzle-inputs/2022/day2"))
     play)

;; part 2
(def test-input
  "A Y\nB X\nC Z\n")


(defn play [rounds]
  (reduce (fn [acc [p1 res]] 
            (+ acc (cond 
                     (= res "lose")
                     (score-round [p1 ((set/map-invert comparer) p1)])
                     
                     (= res "draw")
                     (score-round [p1 p1])
                     
                     :else
                     (score-round [p1 (comparer p1)])))) 0 rounds))

(->> (parse-input (slurp "puzzle-inputs/2022/day2"))
     play)

;; 20437 is too high...