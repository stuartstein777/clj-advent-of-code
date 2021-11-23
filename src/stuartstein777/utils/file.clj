(ns stuartstein777.file
  (:require [clojure.string :as str]))

(defn read-all-lines [file]
  (->> (slurp file)
       (str/split-lines)))

(defn read-all-lines-and-parse [file parsefn]
  (->> file
       (read-all-lines)
       (map parsefn)))