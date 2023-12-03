(ns stuartstein777.2022.day9
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [medley.core :as med]))

(defn neighbours [[x y]]
  (set 
   (for [Δx [-1 0 1]
         Δy [-1 0 1]
         :let [neighbour [(+ x Δx) (+ y Δy)]]
         :when (not= neighbour [x y])]
     neighbour)))

(defn tail-needs-to-move? [hxy txy]
  (not ((neighbours hxy) txy)))

(defn move-tail [[hx hy] [tx ty]]
  (cond
    
    ;; tail and head are same position
    (and (= hx tx) (= hy ty)) 
    [tx ty]
    
    ;; if tail is on same x as head
    (= hx tx) 
    [hx (if (> hy ty) (dec hy) (inc hy))]
    
    ;; if tail is on same y as head
    (= hy ty) 
    [(if (> hx tx) (dec hx) (inc hx)) hy]

    ;; diagonals
    (or (and (< hx tx) (= 2 (- ty hy)))
        (and (> hx tx) (= 2 (- ty hy))))
    [hx (inc hy)]

    (or (and (< hx tx) (= 1 (- ty hy)))
        (and (< hx tx) (= 1 (- hy ty))))
    [(inc hx) hy]

    (or (and (> hx tx) (= 1 (- ty hy)))
        (and (> hx tx) (= 1 (- hy ty))))
    [(dec hx) hy]

    (or (and (> hx tx) (= 2 (- hy ty)))
        (and (< hx tx) (= 2 (- hy ty))))
    [hx (dec hy)]))

(defn parse-input []
  (->> (slurp "puzzle-inputs/2022/day9")
       (str/split-lines)
       (map (fn [l] (let [[a b] (str/split l #"\s")]
                      [a (parse-long b)])))))

(defn expand-input [input]
  (mapcat (fn [[d i]] (repeat i d)) input))

(defn move-head [[hx hy] d]
  (condp = d
    "R" [hx (inc hy)]
    "L" [hx (dec hy)]
    "U" [(dec hx) hy]
    "D" [(inc hx) hy]))

(defn play-step [{:keys [head tail tail-visited]} dv]
  (let [new-head-pos (move-head head dv)
        new-tail-pos (if (tail-needs-to-move? new-head-pos tail)
                       (move-tail new-head-pos tail)
                       tail)
        new-visited (conj tail-visited new-tail-pos)]
    {:head new-head-pos
     :tail new-tail-pos
     :tail-visited new-visited}))

 (->> (parse-input)
      (expand-input)
      (reduce play-step {:head [0 0] :tail [0 0] :tail-visited #{[0 0]}})
      :tail-visited
      count)

;; 6503