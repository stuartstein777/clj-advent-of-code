(ns stuarts.2017day2
  (:require [clojure.string :as str]))

;; part 1
(defn process-line [line]
  (->> (str/split line #"\t")
       (map #(Integer/parseInt %))))

(defn read-spreadsheet []
  (->> (slurp "resources/2017/day2")
       (str/split-lines)
       (map process-line)))

(->> (read-spreadsheet)
     (map (fn [row] (- (apply max row) (apply min row))))
     (reduce +))
;=> 45972

;; part 2
(defn divisibles [row]
  (for [a row
        b (rest row)
        :when (and (not= a b) (zero? (rem a b)))]
    (/ a b)))

(let [spreadsheet (read-spreadsheet)]
  (->> (mapcat divisibles spreadsheet)
       (reduce +)))
;=> 326
