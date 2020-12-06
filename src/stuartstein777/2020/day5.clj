(ns stuartstein777.2020.day5
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

;; shared
(defn split-rows-and-cols [s]
  [(subs s 0 (- (count s) 3)) (subs s (- (count s) 3))])

(defn eval-pass [r p]
  (if (seq p)
    (let [inst (first p)]
      (if (or (= inst \F) (= inst \L))
        (recur (take (/ (count r) 2) r) (rest p))
        (recur (drop (/ (count r) 2) r) (rest p))
        ))
    (first r)))

(defn find-seat-id [[row col]]
  (let [row (eval-pass (range 128) row)
        col (eval-pass (range 8) col)]
    (+ (* row 8) col)))

;; part 1
(->> (slurp "puzzle-inputs/2020/day5")
     (str/split-lines)
     (map split-rows-and-cols)
     (map find-seat-id)
     (apply max))

;; part 2
(let [seat-ids (->> (slurp "puzzle-inputs/2020/day5")
                    (str/split-lines)
                    (map split-rows-and-cols)
                    (map find-seat-id)
                    (sort))]
  (->> (partition 2 1 seat-ids)
       (filter (fn [[x y]] (> (- y x) 1)))
       (ffirst)
       (inc)))