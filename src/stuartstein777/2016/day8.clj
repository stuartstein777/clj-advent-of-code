(ns stuartstein777.2016.day8
  (:require [clojure.string :as str]))

;; part 1
(def screen-width 50)
(def screen-height 6)

;; ============================================================================================================
;; turning on a 'width' by 'height' rectangle in the top left corner means taking h rows then
;; repeating '\o' width times for each row.
;; Then concatenate the repeated \o with the rest of the row.
;; e.g. on a 6 x 5 grid that is initially all off (x means off, o means on):
;;        [[x x x x x x]
;;         [x x x x x x]
;;         [x x x x x x]
;;         [x x x x x x]
;;         [x x x x x x]]
;;
;; (rect 3 2 screen)
;;  so, w = 3, h = 2
;;  we take 2 rows:
;;        [[x x x x x x]
;;         [x x x x x x]]
;;
;; Then we map the rec-turn-on function over these 2 rows.
;; Turning on the 3 items mean repeating o 3 times
;;         [o o o]
;; Then concatenate this with the original row, but drop 3 columns (Since we turned on 3).
;;        [o o o x x x]
;;  Since we mapped over 2 rows, we will end up with:
;;        [[o o o x x x]
;;         [o o o x x x]]
;; Now drop 2 rows from the original screen and concatenate these 2 new rows with the rest of the screen
;; (after dropping 2 rows).
;; ============================================================================================================
(defn turn-on-rectangle [w row]
  (concat (repeat w \o) (drop w row)))

(defn rect [a b screen]
  (as-> (take b screen) o
        (map (partial turn-on-rectangle a) o)
        (concat o (drop b screen))))

(defn rotate [r n row screen]
  (let [rotated (concat (drop (- (count r) n) r) (take (- (count r) n) r))]
    (concat (take row screen) [rotated] (drop (inc row) screen))))

;; ============================================================================================================
;; Rotating a row is just splitting the row at the rotation point, and swapping the halves and rejoining.
;; e.g.
;;  1 2 3 4 5 6 7                   4 5 6 7 1 2 3
;; [o x o x o x x] rotated by 3 => [x o x x o x o]
;;
;; rotating more than the width of the row is irrelevant, as each multiple of the row length just returns
;; you back to the starting layout. So mod the rotation figure by 50.
;; ============================================================================================================
(defn rotate-row [row n screen]
  (let [r (nth screen row)]
    (rotate r n row screen)))

;; ============================================================================================================
;; Rotating a col is exactly the same as rotating a row, except I first rotate the whole thing so the rows
;; are columns and the columns are rows. Transform the row as above, then rotate the whole thing back to
;; how it was originally. This has the effect of rotating the column.
;; ============================================================================================================
(defn rotate-col [col n screen]
  (let [screen (apply map vector screen)
        r (nth screen col)]
    (->> (rotate r n col screen)
         (apply map vector))))

;; =============================================================================================================
;; The starting screen. 50 x 6 all in the off (\x) position.
;; =============================================================================================================
(defn start-screen []
  (repeat screen-height (repeat screen-width \x)))

;5x6
;; Called for each line the input file, returns a partially applied function for each transform.
;; Function return only requires the current screen.
(defn parse-line [line]
  (cond (str/starts-with? line "rect")
        (let [[x y] (rest (re-matches #"rect (\d+)x(\d+)" line))]
          (partial rect (Integer/parseInt x) (Integer/parseInt y)))

        (str/starts-with? line "rotate column")
        (let [[x y] (rest (re-matches #"rotate column x=(\d+) by (\d+)" line))]
          (partial rotate-col (Integer/parseInt x) (mod (Integer/parseInt y) screen-width)))

        (str/starts-with? line "rotate row")
        (let [[x y] (rest (re-matches #"rotate row y=(\d+) by (\d+)" line))]
          (partial rotate-row (Integer/parseInt x) (mod (Integer/parseInt y) screen-width)))))

(let [screen (start-screen)
    inputs (->> (slurp "puzzle-inputs/2016/day8")           ; read the input
                (str/split-lines)                           ; split it by line to get collection of instructions.
                (map parse-line))]                          ; get a collection of functions, 1 for each instruction.
  (->> (reduce (fn [acc i] (i acc)) screen inputs)           ; reduce the fns, each on gets the result of the prev
       (flatten)                                            ; flatten and filter to only on pixels
       (filter #(= \o %))
       (count)))                                            ; and count them.

;; part 2
(let [screen (start-screen)
      inputs (->> (slurp "puzzle-inputs/2016/day8")
                  (str/split-lines)
                  (map parse-line))]
  (->> (reduce (fn [acc i] (i acc)) screen inputs)
       (map (partial apply str))
       (map (fn [s] (str/replace (str/replace s "o" "▓") "x" " ")))))