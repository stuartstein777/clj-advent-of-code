(ns stuartstein777.2016.day6
  (:require [clojure.string :as str]))

;; part 1
(defn biggest-key [m]
  (->> (map (fn [c] [(val c) (key c)]) m)
       (sort-by first)
       (reverse)                                            ;comment out for part 2
       (first)
       (second)))

(->> (slurp "puzzle-inputs/2016/day6")
     (str/split-lines)
     (map (fn [m] (map identity m)))
     (apply mapcat vector)
     (partition 546)
     (map (partial apply str))
     (map frequencies)
     (map biggest-key)
     (apply str))