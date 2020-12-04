(ns stuartstein777.2015.day3
  (:require [clojure.string :as str]))

;; part 1
(defn move [[x y] dir]
  (cond (= \^ dir) [x (inc y)]
        (= \v dir) [x (dec y)]
        (= \> dir) [(inc x) y]
        (= \< dir) [(dec x) y]))

(defn reducer [acc i]
  (let [curr (last acc)]
    (conj acc (move curr i))))

(let [input (str/trim (slurp "puzzle-inputs/2015/day3"))]
  (->> (map identity input)
       (reduce reducer [[0 0]])
       (group-by identity)
       (keys)
       (count)))

;; part 2
(defn deliver [input]
  (->> (map identity input)
       (reduce reducer [[0 0]])))

(let [input (->> (slurp "puzzle-inputs/2015/day3")
                 (str/trim)
                 (map identity))]
  (let [santa      (deliver (take-nth 2 input))
        robo-santa (deliver (take-nth 2 (rest input)))]
    (->> (concat santa robo-santa)
         (group-by identity)
         (keys)
         (count))))