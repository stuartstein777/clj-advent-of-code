(ns stuartstein777.2018.day10
  (:require [clojure.string :as str]))

(defn parse-input [line]
  (let [[x y vx vy] (->> (re-seq #"^position=<(-?\s?\d+), (-?\s?\d+)> velocity=<(-?\s?\d+), (-?\s?\d+)>" line)
                         (first)
                         (rest)
                         (map #(Integer/parseInt (str/trim %))))]
    {:x x, :y y, :vx vx, :vy vy}))

(defn move [{:keys [x y vx vy]}]
  {:x (+ x vx)
   :y (+ y vy)
   :vx vx
   :vy vy})

(defn get-bounding-box [nodes]
  (let [xs (map :x nodes)
        ys (map :y nodes)
        min-x (apply min xs)
        min-y (apply min ys)
        max-x (apply max xs)
        max-y (apply max ys)]
    [(- max-x min-x) (- max-y min-y)]))

(defn get-row-to-print [min-x max-x node-group]
  (let [xs (->> node-group second (map :x) set)
        rg (range min-x (inc max-x))]
    (str/join "" (map #(if (xs %) "⭐" "⬛") rg))))

(defn display-message [nodes]
  (let [min-x (apply min (map :x nodes))
        max-x (apply max (map :x nodes))
        grouped-nodes (sort-by first (group-by :y nodes))]
    (->> grouped-nodes
         (map (partial get-row-to-print min-x max-x))
         (str/join "\n")
         (println))))

(defn solve [width height nodes]
  (let [next (map move nodes)
        [n-width n-height] (get-bounding-box next)]
    (if (> (* width height) (* n-width n-height))
      (recur n-width n-height next)
      (display-message nodes))))

(->> (slurp "puzzle-inputs/2018/day10")
     (str/split-lines)
     (map parse-input)
     (solve Integer/MAX_VALUE Integer/MAX_VALUE))
