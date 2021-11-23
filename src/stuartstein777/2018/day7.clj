(ns stuartstein777.2018.day7
  (:require [stuartstein777.file :as file]))

(def initial (zipmap (map str "ABCDEFGHIJKLMNOPQRSTUVWXYZ") (repeat [])))

(defn parse [line]
  (let [[k v] (->> line
                   (re-find #"Step (\w) must be finished before step (\w) can begin")
                   (rest))]
    {v #{k}}))

(defn empty-vals? [[_ v]]
  (empty? v))

(defn remove-it-from-vals [it [k v]]
  [k (remove #(= it %) v)])

;; part 1
(defn solve [m res]
  (if (empty? m)
    res
    (let [it (ffirst (filter empty-vals? m))]
      (recur (into (sorted-map)
                   (map (partial remove-it-from-vals it))
                   (dissoc m it))
             (str res it)))))

(let [parsed-input (->> (file/read-all-lines-and-parse "puzzle-inputs/2018/day7" parse)
                        (apply merge-with into initial))]
  (solve parsed-input ""))

(comment "answer: " "SCLPAMQVUWNHODRTGYKBJEFXZI")

;; part 2
