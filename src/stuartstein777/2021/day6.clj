(ns stuartstein777.2021.day6
  (:require [stuartstein777.file :as f]
            [stuartstein777.utils :as u]))

(defn puzzle-input []
  (->> (f/parse-csv-ints "puzzle-inputs/2021/day6")
       (frequencies)))

(defn reset [idx-to-reset xs]
  (reduce (fn [acc i] (assoc acc i 6)) xs idx-to-reset))

(defn spawn [to-spawn xs]
  (concat xs (repeat to-spawn 8)))

(loop [input (puzzle-input), day 0]
  (if (= day 80)
    (count input)
    (let [to-spawn (keep-indexed #(when (zero? %2) %1) input)
          updated (->> input 
                       (mapv dec)
                       (reset to-spawn)
                       (spawn (count to-spawn)))]
      (recur updated (inc day)))))

(defn solve [stages day]
  (if (zero? day)
    (->> stages
         vals
         (reduce +))
    (recur {0 (stages 1 0)
            1 (stages 2 0)
            2 (stages 3 0)
            3 (stages 4 0)
            4 (stages 5 0)
            5 (stages 6 0)
            6 (+ (stages 7 0) (stages 0 0))
            7 (stages 8 0)
            8 (stages 0 0)}
           (dec day))))

(solve (puzzle-input) 256)

(->> (iterate (fn [stages] {0 (stages 1 0)
                           1 (stages 2 0)
                           2 (stages 3 0)
                           3 (stages 4 0)
                           4 (stages 5 0)
                           5 (stages 6 0)
                           6 (+ (stages 7 0) (stages 0 0))
                           7 (stages 8 0)
                           8 (stages 0 0)}) (puzzle-input))
     (take 257) 
     (last)
     (vals)
     (reduce +))

(defn update-fish [stages]
  (prn stages)
  (->> (u/map-into (fn [n]
                     (case n
                       8 {8 (stages 0 0)}
                       6 {6 (+ (stages 7 0) (stages 0 0))}
                       {n (stages (inc n) 0)}))
                 {}
                 (u/range-inclusive 0 8))))

(->> (puzzle-input)
     ((apply comp (repeat 256 update-fish)))
     (vals)
     (reduce +))

; 5.79TB memory to hold final nieve solution.
