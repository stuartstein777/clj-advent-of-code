(ns stuartstein777.2024.day6 
  (:require
    [clojure.string :as str]
    [clojure.set :as set]))


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

(defn walk-guard [grid {:keys [yx dir] :as guard} visited]
  ;(prn yx visited)
  (let [limit-y (count grid)
        limit-x (count (first grid))
        [guard-y guard-x] yx
        visited (if (and (pos? guard-y) (not= guard-y limit-y) (not= guard-x limit-x) (pos? guard-x))
                  (conj visited [guard-y guard-x dir])
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

;; check each square they hit, see if you can put an obstacle there in the direction of travel.
;; Plus north, south, east, west of guard start
;; if you end up with a loop.
;; how to detect a loop? cant just use visited squares, as they could revisit a square multiple times
;; without looping.

;    0 1 2 3 4 5 6 7 8 9
; 0 [. . . . # . . . . .]
; 1 [. . . . ┌───────┐ #]
; 2 [. . . . │ . . . │ .]
; 3 [. . # . │ . . . │ .]
; 4 [. . ┌───┼───┐ # │ .]
; 5 [. . │ . | . │ . │ .]
; 6 [. # └───^───┼───┘ .]
; 7 [. ┌─────────┼──┐# .]
; 8 [# └─────────┘  │. .]
; 9 [. . . . . . # .│. .]
;                   ╽


;; expected: [7 6] [6 3] [7 8] [8 1] [8 3] [9 7]
;;
;; found  [7 6] [6 3] [7 8]
;; missed [8 1] [8 3] [9 7]
;;

;; [[6 4 :north] [5 4 :north] [4 4 :north] [3 4 :north] [2 4 :north] [1 4 :north] 
;;  [1 5 :east]  [1 6 :east]  [1 7 :east]  [1 8 :east]  [2 8 :south] [3 8 :south] 
;;  [4 8 :south] [5 8 :south] [6 8 :south] [6 7 :west]  [6 6 :west]  [6 5 :west] 
;;  [6 4 :west]  [6 3 :west]  [6 2 :west]  [5 2 :north] [4 2 :north] [4 3 :east]
;;  [4 4 :east]  [4 5 :east]  [4 6 :east]  [5 6 :south] [6 6 :south] [7 6 :south]
;;  [8 6 :south] [8 5 :west]  [8 4 :west]  [8 3 :west]  [8 2 :west]  [8 1 :west] 
;;  [7 1 :north] [7 2 :east]  [7 3 :east]  [7 4 :east]  [7 5 :east]  [7 6 :east]
;;  [7 7 :east]  [8 7 :south] [9 7 :south]]
;;
;; consider [6 4 :north], do we have any cells for:
;;          [7 4 :west] AFTER [6 4 :north] with no cells between these at
;;                    [7 2 :east] 
;;                    [6 3 :south]
;;                    [8 3 :north]
;;           - if so obstacle can go at [7 3] to create loop.
;;
;;          [5 4 :north], do we have any cells for:
;;          [6 4 :west] AFTER [5 4 :north] - if so obstacle can go at [6 5]
;;
;; no. Could hit it before from another direction, which invalidates later path...

;; part 2 

;; given a sequence and a target, get the cells from the start of the sequence and the
;; target.
(defn get-cells-between-points [visited target]
  (take-while #(not= target %) visited))

;; given a potential obstacle cell, find potential visited blockers that would stop us placing
;; an obstacle there.
;; e.g. we want to end up on [6 4 :north], so we are looking at a path from 
;; [7 4 :west] via an obstacle on [7 3].
;;
;;         [5 3] [5 4] [5 5]
;;         [6 3] [6 4] [6 5]
;;   [7 2] [7#3] [7 4] [7 5]
;;         [8 3]
;; 
;; We couldn't have visited cells between [6 4 :north] and [7 4 :west] of:
;;     [7 2 :east] -> Would hit the obstacle on [7 3]
;;     [8 3 :north] -> Would hit the obstacle on [7 3]
;;     [6 3 :south] -> Would hit the obstacle on [7 3]
;; causing a redirect and our obstacle might no longer be valid.
;; TODO: need to handle limits
(defn get-potential-blockers [[y x]]
  [[y (dec x) :east]
   [(inc y) x :north]
   [(dec y) x :south]])

;; given a cell and direction of travel
;; e.g. [6 4 :north], return an obstacle cell and a cell that if on with specified direction of travel
;; would redirect onto the target cell and direction
;;
;; e.g. we want to end up [6 4 :north], 
;; 
;;   [5 3] [5 4] [5 5]
;;   [6 3] [6 4] [6 5]
;;   [7 3] [7 4] [7 5]
;;
;; We would want an obstacle on **[7 3]** then when it was hit by [7 4 :west]
;; we would be redirected to [6 4 :north]
;; could also be redirected at [6 4 :west], [8 4 :west] [9 4 :west].. upto limit...
;; TODO: need to handle limits
(defn get-potential-obstactle-cell [[y x dir]]
  (condp = dir
    :north [[(inc y) (dec x)] [(inc y) x :west]]
    :south [[(dec y) (inc x)] [(dec y) x :east]]
    :west  [[(inc y) (inc x)] [y (inc x) :south]]
    :east  [[(dec y) (dec x)] [y (dec y) :north]]))

(defn xs-contains-cell? [xs cell]
  (-> (filter (fn [v] (= v cell)) xs)
      count
      (> 0)))

(defn part-2 []
  (let [grid (parse-input)
        [guard-location icon] (find-guard-start grid)
        current-guard {:yx guard-location :dir (facing icon)}
        visited (->> (walk-guard grid current-guard []))]
    (prn visited)
    (loop [to-check visited
           obstacles []]
      (prn) (prn)
      (let [cur (first to-check)]
        (prn cur)
        (if (nil? cur)
          obstacles
          (let [[potential-obstacle target-cell] (get-potential-obstactle-cell cur)]
            (prn "potential obstacle" potential-obstacle " | target-cell " target-cell)
            (if (xs-contains-cell? to-check target-cell)
              (let [cells-upto-target-cell (set (get-cells-between-points to-check target-cell))
                    potential-blockers     (set (get-potential-blockers potential-obstacle))]
                (prn "cells-upto-target-cell" cells-upto-target-cell)
                (prn "potential-blockers" potential-blockers)
                (if (empty? (set/intersection potential-blockers cells-upto-target-cell))
                  (do (prn "found obstacle at " potential-obstacle)
                      (recur (rest to-check) (conj obstacles potential-obstacle)))
                  (do
                    (prn "path contains a potential blocker")
                    (recur (rest to-check) obstacles))))
              (do 
                (prn "never hit target cell" target-cell)
                (recur (rest to-check) obstacles)))))))))


(part-2)

(comment
  
  ; test res [[6 3] [7 6] [7 7]]
  ; test target res [[6 3] [7 6] [7 7] [8 1] [8 3] [9 7]]

  )

