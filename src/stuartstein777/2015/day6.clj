(ns stuartstein777.2015.day6
  (:require [clojure.string :as str]
            [stuartstein777.file :as file]))

(defn parse-line [line]
  (let [[action x1 y1 x2 y2] (->> line
                                  (re-seq #"(turn on|turn off|toggle) (\d+),(\d+) through (\d+),(\d+)")
                                  (first)
                                  (rest))]
    {:action (condp = action
               "turn on" :turn-on
               "turn off" :turn-off
               "toggle" :toggle)
     :x1 (Integer/parseInt x1)
     :y1 (Integer/parseInt y1)
     :x2 (Integer/parseInt x2)
     :y2 (Integer/parseInt y2)}))

(defn get-affected-coords [{:keys [x1 y1 x2 y2]}]
  (for [xs (range x1 (inc x2))
        ys (range y1 (inc y2))]
    [xs ys]))

(defn update-cell [grid cell action]
  (condp = action
    :turn-on (if (grid cell) (update grid cell inc) (assoc grid cell 1))
    :turn-off (if (grid cell) (update grid cell (fn [n] (if (> n 0) (dec n) 0))) grid)
    :toggle (if (grid cell) (update grid cell (fn [n] (+ 2 n))) (assoc grid cell 2))))

(defn reducer [acc i]
  (let [to-update (get-affected-coords i)]
    (reduce (fn [grid cell] (update-cell grid cell (:action i))) acc to-update)))

(time
 (let [parsed-input (file/read-all-lines-and-parse "puzzle-inputs/2015/day6" parse-line)
       initial-grid {}]
   (->> (reduce reducer initial-grid parsed-input)
        (vals)
        (reduce +))))

;; The above solution is very slow, even using a sparse map, at ~25 seconds.

;; Tried to avoid using map below, but it's even slower. 41 seconds!

(defn dec-to-zero [n]
  (if (> n 0) (dec n) 0))

(defn toggle [n]
  (+ 2 n))

(defn get-affected-coords [{:keys [x1 y1 x2 y2 action]}]
  (for [xs (range x1 (inc x2))
        ys (range y1 (inc y2))]
    [[xs ys] (condp = action
                     :turn-on inc
                     :turn-off dec-to-zero
                     :toggle toggle)]))

(defn reducer [part]
  (reduce (fn [acc [_ f]] (f acc)) 0 part))

(time
 (let [parsed-input (file/read-all-lines-and-parse "puzzle-inputs/2015/day6" parse-line)]
   (->> (mapcat get-affected-coords parsed-input)
        (group-by first)
        (partition-by first)
        (map reducer)
        (reduce +))))


;; Attempt 3 - Conj the action to cell in the map.
;; Then reduce each vals, still slow at 32 seconds, nice concise code though.

(defn dec-to-zero [n]
  (if (> n 0) (dec n) 0))

(defn toggle [n]
  (+ 2 n))

(defn get-affected-coords [{:keys [x1 y1 x2 y2 action]}]
  (for [xs (range x1 (inc x2))
        ys (range y1 (inc y2))]
    [[xs ys] (condp = action
               :turn-on inc
               :turn-off dec-to-zero
               :toggle toggle)]))

(defn count-cell [cell]
  (reduce (fn [acc i]  (i acc)) 0 cell))

(time
 (let [parsed-input (->> (file/read-all-lines-and-parse "puzzle-inputs/2015/day6" parse-line)
                         (mapcat get-affected-coords))
       grid  {}]
   (->> parsed-input
        (reduce (fn [acc [cell f]] (update acc cell (fnil conj []) f)) grid)
        (vals)
        (map count-cell)
        (reduce +))))

;; next attempt ???
;; use a vector 1000*1000 wide, update the value in the vector for the co-ord and the action.
