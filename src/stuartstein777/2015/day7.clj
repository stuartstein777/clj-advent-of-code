(ns stuartstein777.2015.day7
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [stuartstein777.file :as file]))

(defn not [x]
  ())

(let [line "NOT y -> z"]
  (->> (re-seq #"((\w)\s(\w+)\s(\w+)\s->\s(\w+))|((\d+)\s->\s(\w+))|((\w+)\s(\w+)\s->\s(\w+))" line)
       (first)
       (remove nil?)
       (drop 2))
  )

(bit-and 123 456)
(bit-shift-left 123 2)
(bit-shift-right 456 2)
(bit-not 123)