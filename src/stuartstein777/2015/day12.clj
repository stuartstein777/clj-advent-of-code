(ns stuartstein777.2015.day12
  (:require [com.rpl.specter :refer [select walker]]
            [clojure.data.json :as json]))
            
;; part 1
(->> (slurp "puzzle-inputs/2015/day12")
     (json/read-str)
     (select (walker number?))
     (reduce +))
