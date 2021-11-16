(ns stuartstein777.2015.day6
  (:require [clojure.string :as str]))

(defn parse-line [line]
  
  )


(->> (slurp "puzzle-inputs/2015/day6")
     (str/split-lines)
     (map parse-line))