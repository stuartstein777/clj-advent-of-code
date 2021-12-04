(ns stuartstein777.2021.day4
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [stuartstein777.file :as f]
            [stuartstein777.utils :as u]))

(let [input (->> (slurp "puzzle-inputs/2021/day4-test")
                 (str/split-lines))
      numbers (->> input
                   first
                   (u/str-split #",")
                   (map #(Integer/parseInt %)))
      boards (->> input
                  (drop 1)
                  (partition 6)
                  (map (fn [b] (->> b
                                   (remove #(= "" %))
                                    (map (partial u/str-split #" "))
                                    (map #(Integer/parseInt %))
                                   ))))]
  boards
  )


