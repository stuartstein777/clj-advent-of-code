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
    (and (= hx tx) (= hy ty))
    [tx ty]
    
    (= hx tx) ;; if tail is on same x as head
    [hx (if (> hy ty) (dec hy) (inc hy))]
    (= hy ty) ;; if tail is on same y as head
    [(if (> hx tx) (dec hx) (inc hx)) hy]

    ;; diagonals
    (and (< hx tx) (= 2 (- ty hy)))
    [hx (inc hy)]

    (and (< hx tx) (= 1 (- ty hy)))
    [(inc hx) hy]

    (and (> hx tx) (= 2 (- ty hy)))
    [hx (inc hy)]

    (and (> hx tx) (= 1 (- ty hy)))
    [(dec hx) hy]

    (and (> hx tx) (= 2 (- hy ty)))
    [hx (dec hy)]

    (and (> hx tx) (= 1 (- hy ty)))
    [(dec hx) hy]

    (and (< hx tx) (= 1 (- hy ty)))
    [(inc hx) hy]
    
    (and (< hx tx) (= 2 (- hy ty)))
    [hx (dec hy)]
    
    :else
    (do (prn "argh!" [hx hy] [tx ty])
        (throw "wtf"))
    ))
;; combine some of these based on returned value.

(= [-4 3] (move-tail [-4 2] [-3 4]))
(= [-3 2] (move-tail [-4 2] [-2 3]))
(= [-2 3] (move-tail [-2 2] [-3 4])) 
(= [-3 2] (move-tail [-2 2] [-4 3]))
(= [-2 3] (move-tail [-2 4] [-3 2]))
(= [-3 4] (move-tail [-2 4] [-4 3])) 
(= [-3 4] (move-tail [-4 4] [-2 3]))
(= [-4 3] (move-tail [-4 4] [-3 2]))

;; > hy ty
;; < hx tx
;; [hx (inc hx)]
;;          hx hy  tx ty
(move-tail [-4 2] [-3 4]) ; [-4 3]

(defn parse-input []
  (->> (slurp "puzzle-inputs/2022/day9-test")
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
    (prn new-head-pos "::" new-tail-pos)
    {:head new-head-pos
     :tail new-tail-pos
     :tail-visited new-visited}))


 (->> (parse-input)
      (expand-input)
      (reduce play-step {:head [0 0] :tail [0 0] :tail-visited #{[0 0]}})
      :tail-visited
      count)

;; 6808 too high

;; test answer should be 13

(comment
  
  
     *
  [[-4 2] [-4 3] [-4 4]
   [-3 2] [-3 3] [-3 4] *
   [-2 2] [-2 3] [-2 4]]

;; hy > ty
;; hx < tx
;; [hx (inc hy)]
  '[[H] [ ] [ ]
    [ ] [ ] [T]
    [ ] [ ] [ ]]

  
;; hy < ty
;; hx < tx
;; [hx (dec hy)]
  '[[H] [ ] [ ] 
    [.] [ ] []
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
