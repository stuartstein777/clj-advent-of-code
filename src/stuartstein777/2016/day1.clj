(ns stuartstein777.2016.day1
  (:require [clojure.string :as str]))

;; part 1
(def directions {[:north "R"] :east
               [:north "L"] :west
               [:south "R"] :west
               [:south "L"] :east
               [:east "R"] :south
               [:east "L"] :north
               [:west "R"] :north
               [:west "L"] :south})

(defn parse-line [line]
  (let [[dir blocks] (rest (first (re-seq #"(R|L)(\d+)" line)))]
    [dir (Integer/parseInt blocks)]))


(defn walk-route [route [x y] facing visited]
  (if (seq route)
    (let [[turn blocks] (first route)
         facing (directions [facing turn])]
      (cond (= facing :north) (recur (rest route) [x (+ y blocks)] facing visited)
            (= facing :south) (recur (rest route) [x (- y blocks)] facing visited)
            (= facing :east) (recur (rest route) [(+ x blocks) y] facing visited)
            (= facing :west) (recur (rest route) [(- x blocks) y] facing visited)))
    [x y]))

;; part 2
(defn get-steps [[x1 y1] [x2 y2]]
  (let [xs (range (inc x1) (inc x2))
      ys (range (inc y1) (inc y2))]
    (if (= x1 x2)
      (map (fn [x y] [x y]) (repeat x1) ys)
      (map (fn [x y] [x y]) xs (repeat y1)))))

(defn walk-route [route [x y] facing visited]
  (if (seq route)
    (let [[turn blocks] (first route)
          facing (directions [facing turn])
          steps (rest (reductions (fn [[x y] _]
                                    (cond (= facing :north) [x (inc y)]
                                          (= facing :south) [x (dec y)]
                                          (= facing :east) [(inc x) y]
                                          (= facing :west) [(dec x) y])) [x y] (range blocks)))
          [nx ny] (last steps)
          revisited (first (drop-while (fn [step] (not (visited step))) steps))]
      (if revisited
        revisited
        (recur (rest route) [nx ny] facing (apply conj visited steps))))))

;; shared
(let [fixed-input (as-> (slurp "puzzle-inputs/2016/day1") o
                        (str/split o #",")
                        (map str/trim o)
                        (map parse-line o))
      [x y] (walk-route fixed-input [0 0] :north #{})]
  (+ (Math/abs ^int x) (Math/abs ^int y)))