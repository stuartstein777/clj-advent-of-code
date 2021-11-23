(ns stuartstein777.2018.day7
  (:require [stuartstein777.file :as file]
            [clojure.set :as set]))

;; part 1

(def initial (->> "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                  (map (fn [l] {(str l) #{}}))
                  (apply merge)))

(defn parse [line]
  (let [[k v] (->> line
                   (re-seq #"Step (\w) must be finished before step (\w) can begin")
                   (first)
                   (rest))]
    {v #{k}}))

(let [parsed-input (->> (file/read-all-lines-and-parse "puzzle-inputs/2018/day7" parse)
                        (apply merge-with into initial))]
  (loop [cur parsed-input
         res ""]
    (if (= {} cur)
      res
      (let [next (->> cur
                      (filter (fn [[_ v]] (empty? v)))
                      (sort-by key)
                      (ffirst))]
        (recur (into {}
                     (map (fn [[k v]] [k (remove #(= next %) v)]))
                     (dissoc cur next))
               (str res next))))))
