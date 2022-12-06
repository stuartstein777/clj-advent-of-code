(ns stuartstein777.2022.day6)

(defn marker? [s]
  (= (count (set s)) (count s)))

(defn find-start-of-marker [s]
  (->> s
       (partition 4 1)
       (take-while (complement marker?))
       count
       (+ 4)))

;; part 1
(-> (slurp "puzzle-inputs/2022/day6")
     (find-start-of-marker))

(defn find-start-of-message [s]
  (->> s
       (partition 14 1)
       (take-while (complement marker?))
       count
       (+ 14)))

;; part 2
(-> (slurp "puzzle-inputs/2022/day6")
    (find-start-of-message))

