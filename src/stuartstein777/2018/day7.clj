(ns stuartstein777.2018.day7
  (:require [stuartstein777.file :as file]))

(def initial (->> "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                  (map (fn [l] {(str l) #{}}))
                  (apply merge)
                  (into (sorted-map))))

(defn parse [line]
  (let [[k v] (->> line
                   (re-seq #"Step (\w) must be finished before step (\w) can begin")
                   (first)
                   (rest))]
    {v #{k}}))

(defn empty-vals? [[_ v]]
  (empty? v))

;; part 1
(defn solve [m res]
  (if (= {} m)
    res
    (let [it (ffirst (filter empty-vals? m))]
      (recur (into (sorted-map)
                   (map (fn [[k v]] [k (remove #(= it %) v)]))
                   (dissoc m it))
             (str res it)))))

(let [parsed-input (->> (file/read-all-lines-and-parse "puzzle-inputs/2018/day7" parse)
                        (apply merge-with into initial))]
  (solve parsed-input ""))

(comment "answer: " "SCLPAMQVUWNHODRTGYKBJEFXZI")

;; part 2
