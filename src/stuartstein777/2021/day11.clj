(ns stuartstein777.2021.day11
  (:require [stuartstein777.file :as f]
            [stuartstein777.utils :as u]
            [clojure.string :as str]))

(defn xy->idx
  [width [x y]]
  (+ y (* x width)))

(defn parser [line]
  (->> line
       (u/str-split #"")
       (map identity)
       (mapv #(Integer/parseInt %))))

(defn build-coords [w h]
  (for [x (range 0 w)
        y (range 0 h)]
    [x y]))

(defn get-adjacent-cells [[x y] w h]
  (for [Δx [-1 0 1]
        Δy [-1 0 1]
        :let [x' (+ x Δx)
              y' (+ y Δy)]
        :when (and (not= [x y] [x' y'])
                   (<= 0 x' (dec w))
                   (<= 0 y' (dec h)))]
    [x' y']))

(defn increment-cell [inc-zeros? cell]
  (update cell :n (fn [n]
                    (case n
                      0 (if inc-zeros? 1 0)
                      (-> n inc (mod 10))))))

(defn flash-cell [{:keys [flashed] :as cell}]
  (if flashed
    cell
    (-> cell
        (assoc :flashed true)
        (update :flashes inc))))

(defn print-grid [w grid]
  (let [grid (->> (map :n grid)
                  (partition w))]
    (println (str/join "\n" (map (partial str/join " ") grid)))
    (println)))

(defn flash [w h grid]
  (let [{:keys [xy i] :as z} (first (filter (fn [{:keys [n flashed]}] (and (= 0 n) (not flashed))) grid))]
    (if z
      (let [adjacent-cells (map (partial xy->idx w) (get-adjacent-cells xy w h))]
        (recur w h
               (-> (reduce (fn [acc i] (update acc i (partial increment-cell false))) grid adjacent-cells)
                   (update i flash-cell))))
      grid)))

(defn increment-all [grid]
  (mapv (partial increment-cell true) grid))

(defn build [xs width height]
  (map (fn [n xy i] {:n n, :flashes 0, :flashed false :xy xy :i i})
       xs
       (build-coords width height)
       (range (* width height))))

(defn reset-flashed [grid]
  (map (fn [g] (assoc g :flashed false)) grid))

(defn round [width height grid]
  (-> grid
      (increment-all)
      ((partial flash width height))
      reset-flashed))

(defn count-flashes [grid]
  (reduce + (map :flashes grid)))

(let [input  (f/read-all-lines-and-parse "puzzle-inputs/2021/day11" parser)
      width  (count (first input))
      height (count input)
      rounds (apply comp (repeat 100 (partial round width height)))]
  (-> (build (apply concat input) width height)
      rounds
      (count-flashes))) ; 1719

; part 2

(let [input  (f/read-all-lines-and-parse "puzzle-inputs/2021/day11" parser)
      width  (count (first input))
      height (count input)]
  (loop [grid (build (apply concat input) width height)
         total-flashes 0
         n 1]
    (let [after-round (round width height grid)
          flashes (count-flashes after-round)]
      (if (= (- flashes total-flashes) 100)
        n
        (recur after-round flashes (inc n)))))) ; 232