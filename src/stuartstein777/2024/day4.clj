(ns stuartstein777.2024.day4
  (:require [clojure.string :as str]))

(defn parse-input []
  (as-> "puzzle-inputs/2024/day4" o
    (slurp o)
    (str/split-lines o)
    (mapv #(mapv identity %) o)))

(defn xmas-horizontal [grid x y xy limit]
  (if (= x limit)
    0
    (let [cells [xy
                 (get-in grid [y (+ 1 x)])
                 (get-in grid [y (+ 2 x)])
                 (get-in grid [y (+ 3 x)])]]
      (if (or (= cells [\X \M \A \S])
              (= cells [\S \A \M \X]))
        1 0))))

(defn xmas-vertical [grid x y xy limit]
  (if (= y limit)
    0
    (let [cells [xy
                 (get-in grid [(+ 1 y) x])
                 (get-in grid [(+ 2 y) x])
                 (get-in grid [(+ 3 y) x])]]
      (if (or (= cells [\X \M \A \S])
              (= cells [\S \A \M \X]))
        1 0))))

(defn xmas-diagonal [grid x y xy limit]
  (if (= y limit)
    0
    (let [diag1 [xy
                 (get-in grid [(+ 1 y) (+ 1 x)])
                 (get-in grid [(+ 2 y) (+ 2 x)])
                 (get-in grid [(+ 3 y) (+ 3 x)])]
          diag2 [xy
                 (get-in grid [(+ 1 y) (- x 1)])
                 (get-in grid [(+ 2 y) (- x 2)])
                 (get-in grid [(+ 3 y) (- x 3)])]]
      (+ (if (or (= diag1 [\X \M \A \S])
                 (= diag1 [\S \A \M \X]))
           1 0)
         (if (or (= diag2 [\X \M \A \S])
                 (= diag2 [\S \A \M \X]))
           1 0)))))

(defn xmas? [grid x y limit]
  (let [xy (get-in grid [y x])]
    (if (or (= xy \S)
            (= xy \X))
      (+ (xmas-horizontal grid x y xy limit)
         (xmas-vertical grid x y xy limit)
         (xmas-diagonal grid x y xy limit))
      0)))

(defn part-1 [grid grid-size]
  (let [limit (- grid-size 3)]
    (loop [x 0
           y 0
           total 0]
      (cond
        (and (= grid-size y) (= grid-size x)) total
        (= grid-size x) (recur 0 (inc y) total)
        :else (recur (inc x) y (+ total (xmas? grid x y limit)))))))

(defn x-mas [grid y x]
  (if (= \A (get-in grid [y x]))
    (let [corners [(get-in grid [(- y 1) (- x 1)])
                   (get-in grid [(- y 1) (+ x 1)])
                   (get-in grid [(+ y 1) (- x 1)])
                   (get-in grid [(+ y 1) (+ x 1)])]]
      (if (or (= corners [\M \M \S \S])
              (= corners [\M \S \M \S])
              (= corners [\S \S \M \M])
              (= corners [\S \M \S \M]))
        1
        0))
    0))

(defn part-2 [grid grid-size] 
  (loop [x 0
         y 0
         total 0]
    (cond
      (and (= grid-size y) (= grid-size x)) total
      (= grid-size x) (recur 0 (+ 1 y) total)
      :else (recur (+ 1 x) y (+ total (x-mas grid x y))))))

(time
 (let [grid (parse-input)
       grid-size (count grid)]
   (part-1 grid grid-size)
   (part-2 grid grid-size)))

;; part 1 - 2514
;; part 2 - 1888


