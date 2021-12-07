(ns stuartstein777.2021.day5
  (:require [stuartstein777.file :as f]
            [stuartstein777.utils :as u]
            [clojure2d.core :as c2d]
            [clojure2d.extra.utils :as utils]))

(defn parser [line]
  (let [[x1 y1 x2 y2] (->> line
                           (re-seq #"(\d+),(\d+) -> (\d+),(\d+)")
                           u/frest
                           u/parse-list-of-ints)]
    [[x1 y1] [x2 y2]]))

(defn horizontal-or-vertical? [[[x1 y1] [x2 y2]]]
  (or (= x1 x2) (= y1 y2)))

(defn get-horizontal-or-vertical-points [[[x1 y1] [x2 y2]]]
  (for [xs (u/range-inclusive (min x1 x2) (max x1 x2))
        ys (u/range-inclusive (min y1 y2) (max y1 y2))]
    [xs ys]))

(defn get-diagonal-points [[[x1 y1] [x2 y2]]]
  (cond (and (>= x2 x1) (>= y2 y1)) (map vector (u/range-inclusive x1 x2) (u/range-inclusive y1 y2))
        (and (<= x2 x1) (<= y2 y1)) (map vector (u/range-inclusive x2 x1) (u/range-inclusive y2 y1))
        (and (<= x1 x2) (>= y1 y2)) (map vector (u/range-inclusive x1 x2) (reverse (u/range-inclusive y2 y1)))
        (and (>= x1 x2) (<= y1 y2)) (map vector (reverse (u/range-inclusive x2 x1)) (u/range-inclusive y1 y2))))

(defn get-points-on-line [coords]
  (if (horizontal-or-vertical? coords)
    (get-horizontal-or-vertical-points coords)
    (get-diagonal-points coords)))

(->> (f/read-all-lines-and-parse "puzzle-inputs/2021/day5" parser)
     #_(filter horizontal-or-vertical?) ; uncomment to solve part 1
     (mapcat get-points-on-line)
     (frequencies)
     (filter (fn [[_ v]] (> v 1)))
     (count))
