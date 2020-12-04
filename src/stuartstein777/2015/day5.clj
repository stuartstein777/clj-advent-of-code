(ns stuartstein777.2015.day5
  (:require [clojure.string :as str]))

(defn is-vowel [c]
  (boolean (#{\a \e \i \o \u} c)))

(defn has-three-vowels? [input]
  (>= (count (filter is-vowel (map identity input))) 3))

(defn has-double-char? [input]
  (let [ol (count input)
      dl (count (dedupe input))]
    (> ol dl)))

(defn has-no-forbidden-text? [input]
  (nil? (re-seq #"ab|cd|pq|xy" input)))

(defn is-nice [input]
  (let [has-three-vowels (has-three-vowels? input)
        has-double-char (has-double-char? input)
        has-no-forbidden-text (has-no-forbidden-text? input)]
    (and has-three-vowels has-double-char has-no-forbidden-text)))

(let [input (str/split-lines (slurp "puzzle-inputs/2015/day5"))]
  (count (remove false? (map is-nice input))))

;; part 2

(defn has-pair [input]
  (->> (partition-all 2 1 (dedupe input))
       (group-by identity)
       (filter #(>= (count (second %)) 2))
       (count)
       (zero?)
       (not)))

(defn rule-two [input]
  (->> (partition 3 1 input)
       (filter (fn [x] (and (= (first x) (nth x 2)))))
       (count)
       (zero?)
       (not)))

(defn is-nice [s]
  (and (has-pair s) (rule-two s)))

(let [input (str/split-lines (slurp "puzzle-inputs/2015/day5"))]
  (->> (map is-nice input)
       (filter true?)
       (count)))

