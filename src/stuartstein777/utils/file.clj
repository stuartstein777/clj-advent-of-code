(ns stuartstein777.file
  (:require [clojure.string :as str]
            [stuartstein777.utils :as u]))

(defn read-all-lines [file]
  (->> (slurp file)
       (str/split-lines)
       (remove (fn [l] (= "" l)))))

(defn read-all-lines-and-parse [file parsefn]
  (->> file
       (read-all-lines)
       (map parsefn)))

(defn parse-csv-ints [file]
  (->> file
       (slurp)
       (u/str-split #",")
       (map #(Integer/parseInt %))))
