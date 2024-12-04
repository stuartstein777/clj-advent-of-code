(ns stuartstein777.2024.day4
  (:require [clojure.string :as str]))

(defn parse-input []
  (as-> "puzzle-inputs/2024/day4" o
    (slurp o)
    (str/split-lines o)
    (mapv (fn [xs] (str/split xs #"")) o)))

(defn xmas-horizontal [grid x y]
  (let [cells [(get-in grid [y x])
               (get-in grid [y (+ 1 x)])
               (get-in grid [y (+ 2 x)])
               (get-in grid [y (+ 3 x)])]]
    (if (or (= cells ["X" "M" "A" "S"])
            (= cells ["S" "A" "M" "X"]))
      1 0)))

(defn xmas-vertical [grid x y]
  (let [cells [(get-in grid [y x])
               (get-in grid [(+ 1 y) x])
               (get-in grid [(+ 2 y) x])
               (get-in grid [(+ 3 y) x])]]
    (if (or (= cells ["X" "M" "A" "S"])
            (= cells ["S" "A" "M" "X"]))
      1 0)))

(defn xmas-diagonal [grid x y]
  (let [diag1 [(get-in grid [y x])
               (get-in grid [(+ 1 y) (+ 1 x)])
               (get-in grid [(+ 2 y) (+ 2 x)])
               (get-in grid [(+ 3 y) (+ 3 x)])]
        diag2 [(get-in grid [y x])
               (get-in grid [(+ 1 y) (- x 1)])
               (get-in grid [(+ 2 y) (- x 2)])
               (get-in grid [(+ 3 y) (- x 3)])]]
    (+ (if (or (= diag1 ["X" "M" "A" "S"])
               (= diag1 ["S" "A" "M" "X"]))
         1 0)
       (if (or (= diag2 ["X" "M" "A" "S"])
               (= diag2 ["S" "A" "M" "X"]))
         1 0))))

(defn xmas? [grid x y]
  (+ (xmas-horizontal grid x y)
     (xmas-vertical grid x y)
     (xmas-diagonal grid x y)))

(defn part-1 []
  (let [grid (parse-input)
        width (count grid)
        height (count (first grid))]
    (loop [x 0
           y 0
           total 0]
      (cond
        (and (= height y) (= width x)) total
        (= width x) (recur 0 (inc y) total)
        :else (recur (inc x) y (+ total (xmas? grid x y)))))))

(part-1) ;; 2514

;; part 2

(defn x-mas [grid y x]
  (if (= "A" (get-in grid [y x]))
    (let [corners [(get-in grid [(- y 1) (- x 1)])
                   (get-in grid [(- y 1) (+ x 1)])
                   (get-in grid [(+ y 1) (- x 1)])
                   (get-in grid [(+ y 1) (+ x 1)])]]
      (if (or (= corners ["M" "M" "S" "S"])
              (= corners ["S" "S" "M" "M"])
              (= corners ["S" "M" "S" "M"])
              (= corners ["M" "S" "M" "S"]))
        1
        0))
    0))

(defn part-2 []
  (let [grid (parse-input)
        width (count grid)
        height (count (first grid))]
    (loop [x 0
           y 0
           total 0]
      (cond
        (and (= height y) (= width x)) total
        (= width x) (recur 0 (inc y) total)
        :else (recur (inc x) y (+ total (x-mas grid x y))))))
  )

(part-2) ;; 1888
