(ns stuartstein777.2021.day13
  (:require [stuartstein777.file :as f]
            [stuartstein777.utils :as u]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-coord [line]
  (->> (str/split line #",")
       (map #(Integer/parseInt %))))

(defn parse-folds [line]
  (let [[xy n] (u/frest (re-seq #"fold along (x|y)=(\d+)" line))]
    [xy (Integer/parseInt n)]))

(defn folder [xy n [x y]]
  (cond (and (= xy "x") (> x n))
        [(- n (Math/abs (- n x))) y]

        (and (= xy "y") (> y n))
        [x (- n (Math/abs (- n y)))]
        
        :else
        [x y]))

(defn row->str [max-x row]
  (let [xs (set (map first row))]
     (map (fn [n] (if (xs n) "🎄" "⬛")) (range (inc max-x)))))

(defn print! [coords]
  (let [max-x (apply max (map first coords))]
    (->> coords
         (group-by #(second %))
         (sort-by key)
         (vals)
         (map (partial row->str max-x))
         (map (partial apply str))
         (str/join "\n")
         (println))))

(defn fold [coords [xy n]]
  (map (partial folder xy n) coords))

(let [input (->> (slurp "puzzle-inputs/2021/day13")
                 (str/split-lines))
      coords (->> input
                  (take-while #(not= "" %))
                  (map parse-coord))
      folds (->> input
                 (drop (inc (count coords)))
                 (map parse-folds)
                 #_(take 1))] ; uncomment to solve part 1
  (->> (reduce fold coords folds)
       distinct
       #_count ; uncomment to solve part 1
       print!))
