(ns stuartstein777.2022.day8
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [clojure.core.match :refer [match]]))

(defn parse-line [l]
  (->> (str/split l #"")
       (mapv (fn [n] (parse-long n)))))

(defn get-coords [dir w idx y]
  (condp = dir
    :l->r [idx y]
    :r->l [(dec (- w idx)) y]
    [idx y])
  )

(defn get-visible-trees-l->r [row]
  #_(prn row)
  (let [last-idx (dec (count row))]
    (loop [idx 0
           biggest -1
           visible []]
      (if (<= idx last-idx) 
        (let [[cur xy] (row idx)]
          #_(prn cur xy)
          (if (> cur biggest)
            (recur (inc idx) cur (conj visible xy))
            (recur (inc idx) biggest visible)))
        visible))))


;                                0 1 2 3 4
;                                2 5 5 1 2
;                        :l->r   _ _            [0 1] [1 1]
;                        :r->l       _   _      [2 1] [4 1]

(defn reverse-rows [grid]
  (mapv (comp vec reverse) grid))

(defn get-visible-trees [grid]
  (set/union 
   (into #{} (mapcat get-visible-trees-l->r grid))
   (into #{} (mapcat get-visible-trees-l->r (reverse-rows grid)))
   (into #{} (mapcat get-visible-trees-l->r (apply map vector grid)))
   (into #{} (mapcat get-visible-trees-l->r (reverse-rows (apply map vector grid))))
   ))

(->> (slurp "puzzle-inputs/2022/day8")
     (str/split-lines)
     (mapv (fn [l] (->> (str/split l #"")
                       (mapv parse-long))))
     (mapv (fn [x r]
             (mapv (fn [y n] [n [x y]])
                  (range (count r)) r)) (range))
     #_(mapv parse-line)
     (get-visible-trees)
     (count))

[[[3 [0 0]] [0 [0 1]] [3 [0 2]] [7 [0 3]] [3 [0 4]]]
 [[2 [1 0]] [5 [1 1]] [5 [1 2]] [1 [1 3]] [2 [1 4]]]
 [[6 [2 0]] [5 [2 1]] [3 [2 2]] [3 [2 3]] [2 [2 4]]]
 [[3 [3 0]] [3 [3 1]] [5 [3 2]] [4 [3 3]] [9 [3 4]]]
 [[3 [4 0]] [5 [4 1]] [3 [4 2]] [9 [4 3]] [0 [4 4]]]]


(comment
  (reverse (range 5))
  )

[;0 1 2 3 4
 [3 0 3 7 3] ; 0
 [2 5 5 1 2] ; 1
 [6 5 3 3 2] ; 2
 [3 3 5 4 9] ; 3
 [3 5 3 9 0] ; 4
 ]

[0 0]
[4 0]
[3 0]