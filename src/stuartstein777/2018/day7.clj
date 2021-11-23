(ns stuartstein777.2018.day7
  (:require [stuartstein777.file :as file]))

(defn initial []
  (zipmap (map str "ABCDEFGHIJKLMNOPQRSTUVWXYZ") (repeat [])))

(defn parse [line]
  (let [[k v] (->> line
                   (re-find #"Step (\w) must be finished before step (\w) can begin")
                   (rest))]
    {v #{k}}))

;; part 1
;;--------------------------------------------------------------------------------------------
(defn kv-has-empty-vals? [[_ v]]
  (empty? v))

(defn remove-it-from-keys-and-vals [m it]
  (into (sorted-map)
        (map (fn [[k v]] [k (remove #(= it %) v)]))
        (dissoc m it)))

(defn solve
  ([m] (solve m ""))
  ([m res]
   (if (empty? m)
     res
     (let [it (ffirst (filter kv-has-empty-vals? m))]
       (recur (remove-it-from-keys-and-vals m it)
              (str res it))))))

(solve (->> (file/read-all-lines-and-parse "puzzle-inputs/2018/day7" parse)
            (apply merge-with into (initial))))

(comment "answer: " "SCLPAMQVUWNHODRTGYKBJEFXZI")

;; part 2
;;--------------------------------------------------------------------------------------------
