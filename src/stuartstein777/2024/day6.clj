(ns stuartstein777.2024.day6 
  (:require
    [clojure.string :as str]))


(defn parse-input []
  (-> "puzzle-inputs/2024/day6-test"
      (slurp)
      (str/split-lines)
      (->> (mapv (fn [r] (mapv identity r))))
      (vec)))

(defn is-guard? [cell]
  (or (= cell \^) (= cell \v) (= cell \<) (= cell \>)))

(defn find-guard-start [grid]
  (let [width (range (count (first grid)))
        height (range (count grid))]
    (first (for [col-idx width
                 row-idx height
                 :let [cell (get-in grid [row-idx col-idx])] 
                 :when (is-guard? cell)]
             [[row-idx col-idx] cell]))))

(defn facing [c]
  (condp = c
    \^ :north
    \v :south
    \> :east
    \< :west))

(defn rotate [{:keys [yx dir]}]
  (let [new-dir (condp = dir
                  :north :east
                  :east :south
                  :south :west
                  :west :north)]
    ;(print (print-str "turn " new-dir))
    {:yx yx
     :dir new-dir}))

(defn advance [{:keys [yx dir]}]
  (let [[y x] yx
        new-yx (condp = dir
                :north [(dec y) x]
                :east [y (inc x)]
                :west [y (dec x)]
                :south [(inc y) x])]
    {:yx new-yx :dir dir}))

(defn can-advance? [grid {:keys [yx dir]}]
  (let [[y x] yx
        obstacle \#
        north-cell (get-in grid [(dec y) x])
        south-cell (get-in grid [(inc y) x])
        west-cell (get-in grid [y (dec x)])
        east-cell (get-in grid [y (inc x)])]
    (or
     (and (= :north dir) (not= north-cell obstacle))
     (and (= :south dir) (not= south-cell obstacle))
     (and (= :west dir) (not= west-cell obstacle))
     (and (= :east dir) (not= east-cell obstacle)))))

(defn walk-guard [grid {:keys [yx] :as guard} visited]
  ;(prn yx visited)
  (let [limit-y (count grid)
        limit-x (count (first grid))
        [guard-y guard-x] yx
        visited (if (and (pos? guard-y) (not= guard-y limit-y) (not= guard-x limit-x) (pos? guard-x))
                  (conj visited [guard-y guard-x])
                  visited)]
    ;(print (print-str "[" guard-y guard-x "] "))
    (if (or (= limit-x guard-x) (= limit-y guard-y) (neg? guard-y) (neg? guard-x))
      visited
      (if (can-advance? grid guard)
        (recur grid (advance guard) visited)
        (recur grid (advance (rotate guard)) visited)))))

(defn part-1 []
  (let [grid (parse-input)
        [guard-location icon] (find-guard-start grid)
        current-guard {:yx guard-location :dir (facing icon)}]
    (->> (walk-guard grid current-guard [])
         (distinct)
         count)))

(part-1)

;; check each square they hit, see if you can put an obstacle there. Plus north, south, east, west of guard start
;; if you end up with a loop.
;; how to detect a loop? cant just use visited squares, as they could revisit a square multiple times
;; without looping.

;    0 1 2 3 4 5 6 7 8 9
; 0 [. . . . # . . . . .]
; 1 [. . . . . . . . . #]
; 2 [. . . . . . . . . .]
; 3 [. . # . . . . . . .]
; 4 [. . . . . . v # . .]
; 5 [. . . . . . . . . .]
; 6 [. # . 0 ^ . . . . .]
; 7 [. . . . . . . . # .]
; 8 [# . . . . . . . . .]
; 9 [. . . . . . # . . .]

;; [7 6] [6 3] [7 8] [8 1] [8 3] [9 7]

;; [6 4] exists multiple times in the path,
;; first time at [6 4] we are travelling north.
;; second time at [6 4] we are travelling west.
;; so an obstacle at *[6 3]* would turn us north and create a loop.
;;
;; [4 4] exists multiple times in the path,
;; first time at [4 4] we are travelling north.
;; second time at [4 4] we are travelling east, 
;; so an obstacle at [4 3] would turn us south, no loop.

;; check north, south, east, west of every square that is visited.
;; put an obstacle there, see if it makes a loop. apart from starting square.

(let [xs [[6 4] [5 4] [4 4] [3 4] [2 4] [1 4] [1 5] [1 6] [1 7] [1 8]
          [2 8] [3 8] [4 8] [5 8] [6 8] [6 7] [6 6] [6 5] [6 4] [6 3] [6 2]
          [5 2] [4 2] [4 3] [4 4] [4 5] [4 6] [5 6] [6 6] [7 6] [8 6]
          [8 5] [8 4] [8 3] [8 2] [8 1]
          [7 1]
          [7 2] [7 3] [7 4] [7 5] [7 6] [7 7]
          [8 7] [9 7]]]
  
  (frequencies xs))


(frequencies [[1 1] [1 1] [2 2] [3 3] [4 4] [4 4]])

;; 
;; [6 4] [5 4] [4 4] [3 4] [2 4] [1 4] turn east
;; [1 5] [1 6] [1 7] [1 8] turn south
;; [2 8] [3 8] [4 8] [5 8] [6 8] turn west
;; [6 7] [6 6] [6 5] [6 4] [6 3] [6 2] turn north
;; [5 2] [4 2] turn east
;; [4 3] [4 4] [4 5] [4 6] turn south
;; [5 6] [6 6] [7 6] [8 6] turn west
;; [8 5] [8 4] [8 3] [8 2] [8 1] turn north
;; [7 1] turn east
;; [7 2] [7 3] [7 4] [7 5] [7 6] [7 7] turn south
;; [8 7] [9 7] exit


;; [6 4] [5 4] [4 4] [3 4] [2 4] [1 4] turn :east
;; [1 5] [1 6] [1 7] [1 8] turn :south
;; [2 8] [3 8] [4 8] [5 8] [6 8] turn :west
;; [6 7] [6 6] [6 5] [6 4] [6 3] [6 2] turn :north
;; [5 2] [4 2] turn :east
;; [4 3] [4 4] [4 5] [4 6] turn :south
;; [5 6] [6 6] [7 6] [8 6] turn :west
;; [8 5] [8 4] [8 3] [8 2] [8 1] turn :north
;; [7 1] turn :east
;; [7 2] [7 3] [7 4] [7 5] [7 6] [7 7] turn :south
;; [8 7] [9 7] [10 7] 

