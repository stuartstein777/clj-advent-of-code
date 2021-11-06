(ns stuarts.ctest
  (:gen-class)
  (:require [clojure.math.combinatorics :as combo]
            [clojure.string :as str]
            [clojure.set :as set]))

(def grid-width 90)                                         ; 90
(def grid-height 93)                                        ; 93

(defn parse []
  (->> (slurp "resources/2020/day11")
       (str/split-lines)
       (map (fn [line] (map identity line)))
       (mapv (fn [row] (mapv (fn [i] (cond (= i \L) \L
                                           :else \.)) row)))))

(defn test-grid [state]
  (case state
    ;        0  1  2  3  4  5  6  7  8  9
    :start [[\L \. \L \L \. \L \L \. \L \L]                 ; 0
            [\L \L \L \L \L \L \L \. \L \L]                 ; 1
            [\L \. \L \. \L \. \. \L \. \.]                 ; 2
            [\L \L \L \L \. \L \L \. \L \L]                 ; 3
            [\L \. \L \L \. \L \L \. \L \L]                 ; 4
            [\L \. \L \L \L \L \L \. \L \L]                 ; 5
            [\. \. \L \. \L \. \. \. \. \.]                 ; 6
            [\L \L \L \L \L \L \L \L \L \L]                 ; 7
            [\L \. \L \L \L \L \L \L \. \L]                 ; 8
            [\L \. \L \L \L \L \L \. \L \L]]                ; 9

    :occup [[\# \. \# \# \. \# \# \. \# \#]
            [\# \# \# \# \# \# \# \. \# \#]
            [\# \. \# \. \# \. \. \# \. \.]
            [\# \# \# \# \. \# \# \. \# \#]
            [\# \. \# \# \. \# \# \. \# \#]
            [\# \. \# \# \# \# \# \. \# \#]
            [\. \. \# \. \# \. \. \. \. \.]
            [\# \# \# \# \# \# \# \# \# \#]
            [\# \. \# \# \# \# \# \# \. \#]
            [\# \. \# \# \# \# \# \. \# \#]]))

;; Gets the element at x y in the grid.
(defn get-element-at-location [grid [x y]]
  (nth (nth grid y) x))

;; Returns true if the cell is within the grid.
(defn valid-neighbour? [[x y]]
  (and (<= 0 x (dec grid-width)) (<= 0 y (dec grid-height))))

;; Get the neighbouring cell co-ordinates of the cell [x y]
;;   [x-1, y-1] [x, y-1] [x+1, y-1]
;;   [x-1, y]            [x+1, y]
;;   [x-1, y+1] [x, y+1] [x+1, y+1]
(defn get-neighbour-cells [[x y]]
  (let [possible-neighbours [[(dec x) (dec y)] [x, (dec y)] [(inc x) (dec y)]
                             [(dec x) y] [(inc x) y]
                             [(dec x) (inc y)] [x (inc y)] [(inc x) (inc y)]]
        valid-neighbours (filter valid-neighbour? possible-neighbours)]
    valid-neighbours))

;; Counts the number of occupied neighbour cells in the grid at location [x y], not including [x y]
(defn number-of-occupied-neighbours [grid [x y]]
  (let [neighbour-cells (get-neighbour-cells [x y])
      neighbours (map (partial get-element-at-location grid) neighbour-cells)]
    (->> (filter (fn [c] (#{\#} c)) neighbours)
         (count))))

;; Get the total number of occupied seats in the grid, an occupied seat is a \#
(defn count-total-occupied-seats [grid]
  (->> (apply concat grid)
       (filter #(= \# %))
       (count)))

;; apply the rules on occupying and emptying seats in the grid and return the new grid.
(defn round [grid]
  (->> (for [y (range grid-height)
             x (range grid-width)
             :let [occupied-neighbours (number-of-occupied-neighbours grid [x y])
                   is-floor? (= \. (get-element-at-location grid [x y]))]]
         (cond is-floor? \.
               (zero? occupied-neighbours) \#
               (>= occupied-neighbours 4) \L
               :else (get-element-at-location grid [x y])))
       (partition grid-width)))

(comment (count-total-occupied-seats (round (round (round (round (round (test-grid :start))))))))

(time (loop [grid (parse)]
        (let [new-grid (round grid)]
          (if (= new-grid grid)
            (count-total-occupied-seats new-grid)
            (recur new-grid)))))

;; part 2
(defn chunk [x y]
  (->> (range (* x y))
       (partition x)
       (map (fn [xs] [(first xs) (last xs)]))))

(first (chunk grid-height grid-width))
(first (chunk grid-width grid-height))

(defn in-same-chunk? [chunks a b]
  (boolean (seq (filter (fn [chunk] (and (>= (min a b) (first chunk))
                                         (<= (max a b) (last chunk)))) chunks))))


(defn get-chunk [chunks n]
  (first (filter (fn [chunk] (and (>= n (first chunk))
                                  (<= n (last chunk)))) chunks)))

;;(= \L (nth grid nxt))
;            (recur nxt (inc cnt))

;; takes the flattened grid.
;; the chunks for the rows
;; current location i
;;
(defn count-seats-diagonal-down-left [chunks grid i]
  #_(prn "i::" i ", chunks::" chunks)
  (loop [i i]
    (let [nxt (+ i (dec grid-width))]
      #_(prn "DDL:: " i)
      (cond (or (in-same-chunk? chunks i nxt)
                (>= nxt (last (last chunks)))
                (= \L (nth grid nxt)))
            0

            (= \# (nth grid nxt))
            1
            ; else its floor, keep looking
            :else
            (recur nxt)))))

(defn count-seats-diagonal-down-right [chunks grid i]
  (loop [i i]
    (let [nxt (+ i (inc grid-width))
        cur-chunk (get-chunk chunks i)
        nxt-chunk (get-chunk chunks nxt)]
      (cond (or (>= nxt (count grid))
                (= (first cur-chunk) (first nxt-chunk))     ;in the same chunk
                (> (first nxt-chunk) (+ (first cur-chunk) grid-width))
                (= \L (nth grid nxt)))
            0

            (= \# (nth grid nxt))
            1

            :else
            (recur nxt)))))

(defn count-seats-diagonal-up-right [chunks grid i]
  (loop [i i]
    (let [nxt (- i (dec grid-width))
        cur-chunk (get-chunk chunks i)
        nxt-chunk (get-chunk chunks nxt)]
      (cond (or (< nxt 0)
                (= (first cur-chunk) (first nxt-chunk))
                (> (first nxt-chunk) (- (first cur-chunk) grid-width))
                (= \L (nth grid nxt)))
            0

            (= \# (nth grid nxt))
            1

            :else
            (recur nxt)))))

(defn count-seats-diagonal-up-left [chunks grid i]
  (loop [i i]
    (let [nxt (- i (inc grid-width))
        cur-chunk (get-chunk chunks i)
        nxt-chunk (get-chunk chunks nxt)]
      (cond (or (< nxt 0)
                (= (first cur-chunk) (first nxt-chunk))
                (> (- (first cur-chunk) (first nxt-chunk)) grid-width)
                (= \L (nth grid nxt)))
            0

            (= \# (nth grid nxt))
            1

            :else
            (recur nxt)))))

(defn count-seats-left [chunks grid i]
  (let [[min _] (get-chunk chunks i)]
    (loop [i (dec i)]
      (cond (or (< i min)
              (= \L (nth grid i)))
        0

        (= \# (nth grid i))
        1

        :else
        (recur (dec i))))))

(defn count-seats-right [chunks grid i]
  (let [[_ max] (get-chunk chunks i)]
    (loop [i (inc i)]
      (cond (or (> i max)
              (= \L (nth grid i)))
        0
        (= \# (nth grid i))
        1

        :else
        (recur (inc i))))))

(defn count-seats-up [grid i]
  (loop [i (- i grid-width)]
    (cond (or (< i 0)
            (= \L (nth grid i)))
          0
          (= \# (nth grid i))
          1
          :else
          (recur (- i grid-width)))))

(defn count-seats-down [grid i]
  (loop [i (+ i grid-width)]
    (cond (or (>= i (count grid))
            (= \L (nth grid i)))
          0
          (= \# (nth grid i))
          1
          :else
          (recur (+ i grid-width)))))

;; takes a flattened grid.
(defn visible-seats [chunks grid i]
  (let [ddl (count-seats-diagonal-down-left chunks grid i)
      ddr (count-seats-diagonal-down-right chunks grid i)
      dul (count-seats-diagonal-up-left chunks grid i)
      dur (count-seats-diagonal-up-right chunks grid i)
      up (count-seats-up grid i)
      dn (count-seats-down grid i)
      l (count-seats-left chunks grid i)
      r (count-seats-right chunks grid i)]
    #_(println "diagonal-down-left ::" ddl
             "\ndiagonal-down-right::" ddr
             "\ndiagonal-up-left   ::" dul
             "\ndiagonal-up-right  ::" dur
             "\nup                 ::" up
             "\ndown               ::" dn
             "\nleft               ::" l
             "\nright              ::" r "\n")
    (+ ddl ddr dul dur up dn l r)))

(comment (let [grid '((\# \. \# \# \. \# \# \. :a \#)
                      (\# \# \# \# \# \# \# \. \# \#)
                      (\# \. \# \. \# \. \. \# \. \.)
                      (\# \# \# \# \. \# \# \. \# \#)
                      (\# \. \# \# \. \# \# \. \# \#)
                      (\# \. \# \# \# \# \# \. \# \#)
                      (\# \. \# \. \# \. \. \. \. \.)
                      (\# \# \# \# \# \# \# \# \# \#)
                      (\# \. \# \# \# \# \# \# \. \#)
                      (\# \. \# \# \# \# \# \. \# \#))
               chunks (chunk 10 10)
               flattened (apply concat grid)]
           (prn chunks)
           (let [i (.indexOf flattened :a)]
             (visible-seats chunks flattened i)
             #_(count-seats-diagonal-down-left chunks flattened i))))

;; round takes a flatted grid and an index i. and the chunked grid boundaries
(defn round [chunks grid]
  (map-indexed (fn [ix seat]
                 (let [visible-seats (visible-seats chunks grid ix)]
                   (cond (= seat \.) \.
                         (>= visible-seats 5) \L
                         (zero? visible-seats) \#
                         :else seat))) grid))

(defn count-total-occupied-seats [grid]
  (->> (filter (fn [f] (= \# f)) grid)
       (count)))

(let [grid (parse)
      flattened (apply concat grid)
      chunks (chunk 90 93)]
  (loop [grid flattened]
    #_(clojure.pprint/pprint (partition 10 grid))
    (let [new-grid (round chunks grid)]
      (if (= new-grid grid)
        (count-total-occupied-seats new-grid)
        (recur new-grid)))))

;; not 5871
;; not 2877

(let [grid (test-grid :start)
      flattened (apply concat grid)
      chunks (chunk  90 93)]
  #_(clojure.pprint/pprint (partition 10 (round chunks flattened)))
  (clojure.pprint/pprint (partition-all 10 (round chunks
                                                  (round chunks
                                                         (round chunks flattened))))))
