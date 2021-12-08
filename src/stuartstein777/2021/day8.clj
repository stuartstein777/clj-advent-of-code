(ns stuartstein777.2021.day8
  (:require [stuartstein777.file :as f]
            [stuartstein777.utils :as u]
            [clojure.string :as str]
            [clojure.math.combinatorics :as combo]))

(defn parser [line]
  (let [[xs ys] (str/split line #" \| ")]
    [(str/split xs #" ") (str/split ys #" ")]))

;; part 1
(let [input (f/read-all-lines-and-parse "puzzle-inputs/2021/day8" parser)]
  (->> input
       (map second)
       (apply concat)
       (map count)
       (filter #(some #{2 3 4 7} #{%}))
       (count))) ; 247

;; part 2
(defn parser [line]
  (str/split line #" \| "))

(def numbers {#{\c \f}                1
            #{\a \c \d \e \g}       2
            #{\a \c \d \f \g}       3
            #{\b \c \d \f}          4
            #{\a \b \d \f \g}       5
            #{\a \b \d \e \f \g}    6
            #{\a \c \f}             7
            #{\a \b \c \d \e \f \g} 8
            #{\a \b \c \d \f \g}    9
            #{\a \b \c \e \f \g}    0})

(defn all-combos [xs]
  (combo/permutations xs))

(defn get-replacer [[a b c d e f g]]
  {"a" a, "b" b, "c" c, "d" d, "e" e, "f" f, "g" g})

(defn decoded->int [decoded]
  (Integer/parseInt (str/join "" decoded)))

(defn to-display-num [num]
  (map set (str/split num #"\s")))

(defn solve-line [[input to-solve]]
  (loop [[perm & rest] (all-combos ["a" "b" "c" "d" "e" "f" "g"])]
    (if (nil? rest)
      :not-found
      (let [replacements (get-replacer perm)
            input'       (str/replace input #"a|b|c|d|e|f|g" replacements)
            nums         (to-display-num input')]
        (if (every? numbers nums)
          (->> (str/replace to-solve #"a|b|c|d|e|f|g" replacements)
               (to-display-num)
               (map numbers)
               (decoded->int))
          (recur rest))))))

(let [input (f/read-all-lines-and-parse "puzzle-inputs/2021/day8" parser)]
  (->> input 
       (map solve-line)
       (reduce +)))
