(ns stuartstein777.2021.day9
  (:require [stuartstein777.file :as f]
            [stuartstein777.utils :as u]
            [clojure.string :as str]
            [clojure.math.combinatorics :as combo]
            [clojure.set :as set]))

(defn parser [s]
  (->> (str/split s #"")
       (mapv #(Integer/parseInt %))))

(defn find-low-points [input]
  (let [height (count input)
        width (count (first input))]
    (for [w (range 0 width)
          h (range 0 height)
          :let [n (get-in input [h w])
                u (get-in input [(dec h) w] 10)
                d (get-in input [(inc h) w] 10)
                l (get-in input [h (dec w)] 10)
                r (get-in input [h (inc w)] 10)]
          :when (and (< n u) (< n d) (< n l) (< n r))]
      {[h w] n})))

;; part 1
(let [input (->> (f/read-all-lines-and-parse "puzzle-inputs/2021/day9" parser))]
  (->> input
       (find-low-points)
       (mapcat vals)
       (map inc)
       (reduce +))) ; 436

#_(str \a \u0333)

;; part 2
;; do this with a stack ? Store locations and recur till stack is empty, as we recur keep count of locations.
;; take the top 3 and find their product.
;; need to keep track of already visited.
(defn get-neighbours [input [y x]]
  (let [yx' [[y (dec x)] [y (inc x)] [(dec y) x] [(inc y) x]]]
    (remove (fn [yx] (= 9 (get-in input yx 9))) yx')))

(defn walk-basins [input visited size to-visit]
  (if (empty? to-visit)
    size
    (let [yx         (first to-visit)
          neighbours (->> (get-neighbours input yx)
                          (remove (fn [yx] (visited yx))))]
      (recur input (conj visited yx) (inc size) (distinct (into (rest to-visit) neighbours))))))

(let [input (f/read-all-lines-and-parse "puzzle-inputs/2021/day9" parser)
      walk-basin-fn (partial walk-basins input #{} 0)]
  (->> input
       (find-low-points)
       (mapcat keys)
       (map vector)
       (map walk-basin-fn)
       (sort >)
       (take 3)
       (reduce * 1)))
