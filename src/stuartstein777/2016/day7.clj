(ns stuartstein777.2016.day7
  (:require [clojure.string :as str]
            [clojure.set :as set]))

;; part 1
(defn has-pairs? [s]
  (->> (partition 4 1 s)
       (filter (fn [[a b c d]] (and (= a d) (= b c) (not= a b))))))

(defn abba? [s]
  (let [non-hyp (->> (re-seq #"(?:^|])([^\]]+)(?:\[|$)" s)
                     (map second)
                     (mapcat has-pairs?))
        hyp (->> (re-seq #"(?<=\[).+?(?=\])" s)
                 (mapcat has-pairs?))]
    (boolean (and (seq non-hyp) (not (seq hyp))))))

(->> (slurp "puzzle-inputs/2016/day7")
     (str/split-lines)
     (map abba?)
     (filter true?)
     (count))

;; part 2
; looking for ABA outside brackets
; with corresponding BAB inside brackets
; can't be AAA, must be different character in middle.

(defn has-ssl-marker? [s]
  (->> (partition 3 1 s)
       (filter (fn [[a b c]] (and (= a c) (not= a b))))))

(defn ssl? [s]
  (let [non-hyp (->> (re-seq #"(?:^|])([^\]]+)(?:\[|$)" s)
                     (mapv second)
                     (mapcat has-ssl-marker?)
                     (into #{}))
        hyp (->> (re-seq #"(?<=\[).+?(?=\])" s)
                 (mapcat has-ssl-marker?)
                 (mapv (fn [[a b _]] [b a b]))
                 (into #{}))]
    (set/intersection non-hyp hyp)))

(->> (slurp "puzzle-inputs/2016/day7")
     (str/split-lines)
     (map ssl?)
     (filter seq)
     (count))