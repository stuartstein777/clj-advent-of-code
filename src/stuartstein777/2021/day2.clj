(ns stuartstein777.2021.day2
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [stuartstein777.file :as f]))

(def puzzle-input "puzzle-inputs/2021/day2")

(defn parser [l]
  l
  )

; part 1
(->> (f/read-all-lines-and-parse puzzle-input parser))

; part 2
(->> (f/read-all-lines-and-parse puzzle-input parser))