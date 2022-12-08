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

(defn get-visible-trees-l->r [dir w row y]
  #_(prn y row)
  (let [last-idx (dec (count row))]
    (loop [idx 0
           biggest -1
           visible []]
      (if (<= idx last-idx) 
        (let [cur (row idx)]
          (prn "biggest: " biggest ", cur: " cur)
          (if (> cur biggest)
            (do
              (prn "adding" cur "@" (get-coords dir w idx y) "can be seen" dir)
              (recur (inc idx) cur (conj visible (get-coords dir w idx y))))
            (recur (inc idx) biggest visible)))
        visible))))


;                                0 1 2 3 4
;                                2 5 5 1 2
;                        :l->r   _ _            [0 1] [1 1]
;                        :r->l       _   _      [2 1] [4 1]
(get-visible-trees-l->r :l->r 5 [2 5 5 1 2] 1)
(get-visible-trees-l->r :r->l 5 [2 1 5 5 2] 1)

(defn reverse-rows [grid]
  (mapv (comp vec reverse) grid))

(defn get-visible-trees [grid]
  (let [rg (range (count (first grid)))
        rgc (count (first grid))]
    (set/union 
     (into #{} (mapcat (partial get-visible-trees-l->r :l->r rgc) grid rg))
     (into #{} (mapcat (partial get-visible-trees-l->r :r->l rgc) (reverse-rows grid) (reverse rg)))
     (into #{} (mapcat (partial get-visible-trees-l->r :t->b rgc) (apply map vector grid) rg))
     (into #{} (mapcat (partial get-visible-trees-l->r :b->t rgc) (reverse-rows (apply map vector grid)) (reverse rg)))
     )))

(->> (slurp "puzzle-inputs/2022/day8-test")
     (str/split-lines)
     (mapv (fn [l] (->> (str/split l #"")
                       (mapv parse-long))))
     (mapv (fn [x r]
             (mapv (fn [y n] [n [x y]])
                  (range (count r)) r)) (range))
     #_(mapv parse-line)
     #_(get-visible-trees)
     #_(count))

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