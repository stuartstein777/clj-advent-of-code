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

(->> (parse-input)
     (map :tile-id)
     (sort <)
     (count))


;; ----- solving
(defn borders-match [tile1 tile2])


;; how to return the rotation that matches.
;; don't want to do the rotations a second time.
;; how to check all rotations of tile1 with all rotations of tile2.
;; how to short circuit if we find a match?
;; how to avoid duplicates. e.g. right-border of A matches left border of B.
;; left-border of B matches right border of A
;; if a tile is matched, dont flip it again.

;; grid is 12x12

;; for each tile, match it to every other tile, rotated all directions, flipped and rotated.
;; create a sparse map of the matches.
;;

(loop [tiles (->> (parse-input))
       corners []]
  (if (empty? tiles)
    :done
    (let [tile (first tiles)]
      
      (recur (rest tiles) []))))

;; can we pick a tile, then find all the tiles that match the sides.
;; match top, right, bottom, left
;; how to decide which tile to move onto next.
;; after each, get the first tile that is placed that doesnt have all 4 directions
;; either set or nil.
;; then look for its borders.
;; if no border matches, then find bordering pieces
;; when matching a piece, set that matched pieces borders as well with matched piece.
;; e.g. 1249 top matches to 1385 bottom.
;; {1249 {:top 1385}
;;  1385 {:bottom 1249}}
;; once a tile has all borders accounted for, remove it from available pieces
;; can we walk the tiles, to set borders without looking: e.g.
;;      A X
;;      B C
;; We are looking at right of C and find X, after placing, if we look, bottom, then right of A
;; we can see that the bottom of X must be C.
;; then go up from A and right.

