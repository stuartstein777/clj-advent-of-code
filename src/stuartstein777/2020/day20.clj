(ns stuartstein777.2020.day20
  (:require [clojure.string :as str]))

;; ----- rotations ------
(defn flip-hori [tile]
  (map reverse tile))

;; not needed.
(defn flip-vert [tile]
  (reverse tile))

(defn rot-90-cw [tile]
  (->> (apply map vector tile)
       (flip-hori)))

;; ----- borders ------
(defn top-edge [tile]
  (first tile))

(defn bottom-edge [tile]
  (last tile))

(defn left-edge [tile]
  (map first tile))

(defn right-edge [tile]
  (map last tile))

;; ----- parsing ------
(defn parse-tile-id [tile-line]
  (-> (subs tile-line 0 (dec (count tile-line)))
      (str/split #"\s")
      (last)
      (->> (apply str)
           (Integer/parseInt))))

(defn parse-tile-image [tile-lines]
  (->> (map #(str/split % #"") tile-lines)
       (vec)))

(defn parse-tile [tile]
  (let [tile-lines (str/split-lines tile)]
    {:tile-id (parse-tile-id (first tile-lines))
     :tile    (parse-tile-image (rest tile-lines))}))

(defn parse-input []
  (-> (slurp "puzzle-inputs/2020/day20")
      (str/split #"\n\n")
      (->> (map parse-tile))))

;; ----- solving
(defn borders-match [tile1 tile2]
  
  )
;; how to return the rotation that matches.
;; don't want to do the rotations a second time.
;; how to check all rotations of tile1 with all rotations of tile2.
;; how to short circuit if we find a match?
;; how to avoid duplicates. e.g. right-border of A matches left border of B.
;; left-border of B matches right border of A
;; if a tile is matched, dont flip it again.

(comment

  (let [matched-left-border   #{}
        matched-right-border  #{}
        matched-top-border    #{}
        matched-bottom-border #{}
        dont-flip             #{}]
    
    )

  (let [tiles (->> (parse-input)
                   (take 5))]
    (for [tile1 tiles
          tile2 tiles
          :when (not= (:tile-id tile1) (:tile-id tile2))]
      [(:tile-id tile1) (:tile-id tile2)]))


  '([1249 1693] [1249 1481] [1249 3169] [1249 1229]
    [1693 1249] [1693 1481] [1693 3169] [1693 1229]
    [1481 1249] [1481 1693] [1481 3169] [1481 1229]
    [3169 1249] [3169 1693] [3169 1481] [3169 1229]
    [1229 1249] [1229 1693] [1229 1481] [1229 3169])

  {:tile-id 1249
   :tile [["." "." "." "#" "." "." "." "." "." "."]
          ["#" "." "." "#" "." "." "#" "." "#" "#"]
          ["#" "#" "." "." "." "." "." "." "." "."]
          ["#" "." "#" "." "." "." "." "." "." "."]
          ["." "." "." "." "." "." "." "." "." "."]
          ["#" "." "." "." "#" "#" "#" "." "." "."]
          ["#" "." "." "#" "." "." "." "." "." "."]
          ["#" "." "." "." "#" "#" "." "." "." "."]
          ["." "." "." "." "." "." "." "." "." "."]
          ["." "." "." "." "." "#" "." "." "." "#"]]}






  (defn foo [a b c]
    (+ a b c))

;; apply
  (apply foo [1 2 3])

  (defn add [x y]
    (+ x y))

;; map
  (map add [1 2 3] [4 5 6])

;; vector
  (vector 1 2 3)

  (map vector [1 2 3] [4 5 6] [7 8 9])

  (apply map vector [[1 2 3] [4 5 6] [7 8 9]]))


