(ns stuartstein777.2022.day8
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [medley.core :as med]))

(defn get-visible-trees-l->r [row]
  (let [last-idx (dec (count row))]
    (loop [idx 0
           biggest -1
           visible []]
      (if (<= idx last-idx)
        (let [[cur xy] (row idx)]
          (if (> cur biggest)
            (recur (inc idx) cur (conj visible xy))
            (recur (inc idx) biggest visible)))
        visible))))

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

(defn get-scenic-score-per-tree [summary grid dir]
  (reduce
   (fn [summary row]
     (reduce 
      (fn [summary idx]
               (let [[height [cx cy]] (nth row idx)
                     to-check (drop (inc idx) row)
                     visible  (count (med/take-upto (fn [[h [_ _]]] 
                                                  (<= height h)) to-check))]
                 (update-in summary [:visible [cx cy] dir] (fnil #(+ % visible) 0)))
               ) summary (range (count row)))) summary grid))

(defn solve-pt2 [grid]
  (let [summary {:visible {}}]
    (-> summary
        (get-scenic-score-per-tree grid :lr)
        (get-scenic-score-per-tree (reverse-rows grid) :rl)
        (get-scenic-score-per-tree (apply map vector grid) :ud)
        (get-scenic-score-per-tree (reverse-rows (apply map vector grid)) :du))))

(->> (slurp "puzzle-inputs/2022/day8")
     (str/split-lines)
     (mapv (fn [l] (->> (str/split l #"")
                        (mapv parse-long))))
     (mapv (fn [x r]
             (mapv (fn [y n] [n [x y]])
                   (range (count r)) r)) (range))
     (solve-pt2)
     :visible
     (vals)
     (map vals)
     (map #(reduce * 1 %))
     (apply max))


