(ns stuartstein777.2016.day3
  (:require [clojure.string :as str]))

;; shared
(defn is-triangle? [[a b c]]
  (and (> (+ a b) c)
     (> (+ a c) b)
     (> (+ b c) a)))

;; part 1
(->> (slurp "puzzle-inputs/2016/day3")
     (str/split-lines)
     (map str/trim)
     (map #(str/split % #"\s+"))
     (map (fn [m] (map #(Integer/parseInt %) m)))
     (filter is-triangle?)
     (count))

;; part 2
(->> (slurp "puzzle-inputs/2016/day3")
     (str/split-lines)
     (map str/trim)
     (map #(str/split % #"\s+"))
     (map (fn [m] (map #(Integer/parseInt %) m)))
     (apply mapcat vector)
     (partition 3)
     (filter is-triangle?)
     (count))