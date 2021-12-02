(ns stuartstein777.2021.day2
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [stuartstein777.file :as f]
            [stuartstein777.utils :as u]))


(def puzzle-input "puzzle-inputs/2021/day2")

(defn parser [l]
  (let [[dir v] (str/split l #" ")]
    {:dir dir
     :v (Integer/parseInt v)}))

; part 1
(->> (f/read-all-lines-and-parse puzzle-input parser)
     (reduce (fn [[x y] {:keys [dir v]}]
               (condp = dir
                 "forward" [(+ x v) y]
                 "up" [x (- y v)]
                 "down" [x (+ y v)])) [0 0])
     (reduce * 1))

; part 2
(->> (f/read-all-lines-and-parse puzzle-input parser)
     (reduce (fn [[x y aim] {:keys [dir v]}]
               (condp = dir
                 "forward" [(+ x v) (+ y (* aim v)) aim]
                 "up"      [x y (- aim v)]
                 "down"    [x y (+ aim v)])) [0 0 0])
     (take 2)
     (reduce * 1))
