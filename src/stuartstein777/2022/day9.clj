(ns stuartstein777.2022.day9
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [medley.core :as med]))

(defn get-neighbours [[x y]]
  (set 
   (for [Δx [-1 0 1]
         Δy [-1 0 1]
         :let [neighbour [(+ x Δx) (+ y Δy)]]
         :when (not= neighbour [x y])]
     neighbour)))

(defn tail-needs-to-move? [hxy txy]
  (not ((get-neighbours hxy) txy)))

(defn move-tail [[hx hy] [tx ty]]
  (cond
    (= hx tx) ;; if tail is on same x as head
    [hx (if (> hy ty) (dec hy) (inc hy))]
    (= hy ty) ;; if tail is on same y as head
    [(if (> hx tx) (dec hx) (inc hx)) hy]
    
    ;; diagonals
    (or (and (< hx tx) (< hy ty))
        (and (< hx tx) (> hy ty)))
    [(inc hx) hy]

    (or (and (> hx tx) (< hy ty))
        (and (> hx tx) (> hy ty)))
    [(dec hx) hy]))

(defn parse-input []
  (->> (slurp "puzzle-inputs/2022/day9-test")
       (str/split-lines)
       (map (fn [l] (let [[a b] (str/split l #"\s")]
                      [a (parse-long b)])))))

(defn expand-input [input]
  (mapcat (fn [[d i]] (repeat i [d 1])) input))

(defn move-head [[hx hy] [d v]]
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
    (prn new-tail-pos)
    {:head new-head-pos
     :tail new-tail-pos
     :tail-visited new-visited}))

(->> (parse-input)
     (expand-input)
     (reduce play-step {:head [0 0] :tail [0 0] :tail-visited #{[0 0]}})
     :tail-visited
     count)

;; test answer should be 13

(comment
  (move-tail [1 1] [3 1]) ; [2 1]
  (move-tail [3 1] [1 1]) ; [2 1]
  (move-tail [2 3] [2 1]) ; [2 2]
  (move-tail [2 1] [2 3]) ; [2 2]
  (move-tail [1 1] [3 2]) ; [2 1]
  (move-tail [1 3] [3 2]) ; [2 3]
  (move-tail [3 1] [1 2]) ; [2 1]
  (move-tail [3 3] [1 2]) ; [2 3]
  
  [[1 1] [1 2] [1 3]
   [2 1] [2 2] [2 3]
   [3 1] [3 2] [3 3]]

;; hy < ty
;; hx < tx
;; [hx (dec hy)]
  '[[H] [ ] [ ] 
    [.] [ ] [ ]
    [ ] [T] [ ]]
  

;; hy < ty
;; hx > tx
;; [hx (dec hy)]
  '[[ ] [ ] [H]
    [ ] [ ] [.]
    [ ] [T] [ ]]
  

;; hy > ty
;; hx > tx
;; [hx (inc hy)]
  '[[ ] [T] [ ]
    [ ] [ ] [.]
    [ ] [ ] [H]]
  

;; hy > ty
;; hx > tx
;; [hx (inc hy)]
  '[[ ] [T] [ ]
    [.] [ ] [ ]
    [H] [ ] [ ]]
  )
