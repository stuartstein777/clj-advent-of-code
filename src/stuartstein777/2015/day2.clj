(ns stuartstein777.2015.day2
  (:require [clojure.string :as str]))

;; part 1
(defn split-area [s]
  (->> (str/split s #"x")
       (map #(Integer/parseInt %))))

(defn calc-area [[l w h]]
  (let [lw (* 2 l w)
        wh (* 2 w h)
        hl (* 2 h l)
        smallest-area (reduce * (take 2 (sort [l w h])))]
    (+ lw wh hl smallest-area)))

(->> (str/split-lines (slurp "puzzle-inputs/2015/day2"))
     (map split-area)
     (map calc-area)
     (reduce +))

;; part 2
;; 2 * 3 * 4 requires 2 + 2 + 3 + 3 = 10 + (2 * 3 * 4) = 24 = 34 feet

(defn calc-ribbon [d]
  (+ (->> (sort d)
          (take 2)
          (reduce +)
          (* 2)
          (+ (reduce * d)))))

(->> (str/split-lines (slurp "puzzle-inputs/2015/day2"))
     (map split-area)
     (map calc-ribbon)
     (reduce +))