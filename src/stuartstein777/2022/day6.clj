(ns stuartstein777.2022.day6)

(defn marker? [s]
  (= (count (set s)) (count s)))

(defn find-marker [n s]
  (->> s
       (partition n 1)
       (take-while (complement marker?))
       count
       (+ n)))

;; part 1
(->> (slurp "puzzle-inputs/2022/day6")
     (find-marker 4))

;; part 2
(->> (slurp "puzzle-inputs/2022/day6")
     (find-marker 14))

