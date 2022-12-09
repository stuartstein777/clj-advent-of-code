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
    [idx y]))

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

;; pass in current set ?
;; if current co-ord is in set, don't do the checks. 

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
   (into #{} (mapcat get-visible-trees-l->r (reverse-rows (apply map vector grid))))))

(->> (slurp "puzzle-inputs/2022/day8")
     (str/split-lines)
     (mapv (fn [l] (->> (str/split l #"")
                        (mapv parse-long))))
     (mapv (fn [x r]
             (mapv (fn [y n] [n [x y]])
                   (range (count r)) r)) (range))
     (get-visible-trees)
     (count))

;; 1763

;; Part 2

(defn update-previous
  [summary height xys dir]
  (reduce
   (fn [summary [x y]]
     (let [prev-height (get-in summary [:biggest [x y]] -1)]
       (prn (str "comparing " [x y] " with " prev-height))
       (if (> height prev-height)
         (-> summary
             (assoc-in [:biggest [x y]] height)
             (update-in [:visible [x y] dir] (fnil inc 0))) ;; -1 so edges are 0
         summary)))
   summary
   xys))

;; this works per row.
(defn get-scenic-score-per-tree [summary grid dir]
  (prn dir)
  (->> grid
       (reduce
        (fn [summary row]
          (prn row)
          (reduce (fn [summary [height [cx cy]]]
                    (let [previous-cells (take-while (fn [[_ [x y]]](not= [x y] [cx cy])) row)]
                      (prn "cur-and-previous" [cx cy] [previous-cells])
                      (update-previous summary height previous-cells dir)))
                  summary row))
        summary)))

(defn solve [grid]
  (let [summary {:visible {}
                 :biggest {}}]
  ;; map over each row. Deal with each row l->r
    (-> summary
        (get-scenic-score-per-tree grid :lr)
        (assoc :biggest {})
        (get-scenic-score-per-tree (reverse-rows grid) :rl)
        (assoc :biggest {})
        (get-scenic-score-per-tree (apply map vector grid) :ud)
        (assoc :biggest {})
        (get-scenic-score-per-tree (reverse-rows (apply map vector grid)) :du))))


(->> (slurp "puzzle-inputs/2022/day8-test")
     (str/split-lines)
     (mapv (fn [l] (->> (str/split l #"")
                        (mapv parse-long))))
     (mapv (fn [x r]
             (mapv (fn [y n] [n [x y]])
                   (range (count r)) r)) (range))
     (solve)
     :visible
     #_(vals)
     #_(map vals)
     #_(map #(reduce * 1 %))
     )



;; on each run, reset the biggest.
;; need to stop on first tree same height or taller than
;; the tree under consideration.

30373
25512
65332
33549
35390

[[[3 [0 0]] [0 [0 1]] [3 [0 2]] [7 [0 3]] [3 [0 4]]]
 [[2 [1 0]] [5 [1 1]] [5 [1 2]] [1 [1 3]] [2 [1 4]]]
 [[6 [2 0]] [5 [2 1]] [3 [2 2]] [3 [2 3]] [2 [2 4]]]
 [[3 [3 0]] [3 [3 1]] [5 [3 2]] [4 [3 3]] [9 [3 4]]]
 [[3 [4 0]] [5 [4 1]] [3 [4 2]] [9 [4 3]] [0 [4 4]]]]

{[3 [2 3]] {:du 2}
 [5 [1 1]] {:du 1}
 [3 [2 2]] {:du 1}
 [2 [2 4]] {:du 2}
 [3 [4 2]] {:du 1}
 [3 [4 0]] {:du 2}
 [5 [1 2]] {:du 1}
 [4 [3 3]] {:du 2}
 [5 [3 2]] {:du 2}
 [2 [1 4]] {:du 1}
 [5 [2 1]] {:du 1}
 [9 [4 3]] {:du 2}
 [1 [1 3]] {:du 1}
 [2 [1 0]] {:du 1}
 [5 [4 1]] {:du 2}
 [0 [4 4]] {:du 1}
 [3 [3 0]] {:du 1}
 [6 [2 0]] {:du 2}
 [3 [3 1]] {:du 1}
 [9 [3 4]] {:du 2}}


;; l->r all correct
;; r->l all correct
;; b->t  













;;  0         1         2         3         4
;;[[3 [0 0]] [0 [0 1]] [3 [0 2]] [7 [0 3]] [3 [0 4]]]

;;   0         1         2         3         4          5        6          7

(comment
  (let [summary {:biggest {} :visible {}}
        row [[3 [0 0]] [7 [0 1]] [1 [0 2]] [2 [0 3]] [9 [0 4]] [3 [0 5]] [7 [0 6]] [5 [0 7]]]]
    (reduce 
     (fn [summary [n [x y]]] (update-previous summary n x y))
     summary row))
  

  [[3 [0 0]] [7 [0 1]] [1 [0 2]] [2 [0 3]] [9 [0 4]] [3 [0 5]] [7 [0 6]] [5 [0 7]]]
  
;;   ----------1----------------------------2
;;             ---------1----------2--------3
;;                       ----------1--------2
;;                                 ---------1
;;                                          -----------1----------2
;;                                                     -----------1
;;                                                                ---------1
  
  {:biggest {[0 0] 9, [0 1] 9, [0 2] 9, [0 3] 9, [0 4] 7, [0 5] 7, [0 6] 5},
   :visible {[0 0] 2, [0 1] 3, [0 2] 2, [0 3] 1, [0 4] 2, [0 5] 1, [0 6] 1}}
  


;;                               summary = {}
;; idx = 1    n = 7, xy = [0 1]
;; if n > (get-in summary [:biggest xy] -1)  :: 7 > -1
;;     update summary->biggest [0 0] 7
;;     update summary->visible [0 0] inc = 1
;;
;; idx = 2    n = 1, xy = [0 2]      |- go from 0 to idx
;; if n > (get-in summary [:biggest [0, 0]] -1)  :: 1 > 7
;; if n > (get-in summary [:biggest [0, 1]] -1)  :: 1 > -1
;;     update summary->biggest [0 1] 1
;;     update summary->visible [0 1] inc = 1
;; 
;; idx = 3    n = 2, xy = [0 3]      |- go from 0 to idx
;; if n > (get-in summary [:biggest [0, 0]] -1)  :: 2 > 7
;; if n > (get-in summary [:biggest [0, 1]] -1)  :: 2 > 1
;;     update summary->biggest [0 1] 2
;;     update summary->visible [0 1] inc = 2
;; if n > (get-in summary [:biggest [0, 2]] -1)  :: 2 > -1
;;     update summary->biggest [0 2] 2
;;     update summary->visible [0 2] inc = 1 
  
;; these steps are a reduce of (range 0 y)
;; with summary as the reduction accumulator
;; how to do the sub steps ?
;; another loop ?
  












  (get-in {} [:biggest [0 0]] -1)
  )