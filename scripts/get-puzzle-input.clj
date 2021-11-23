(ns download-input
  (:require [babashka.curl :as curl]))

(def cookie (slurp "/home/stuart/Source/.aoc-cookie"))
            
(let [[year day] *command-line-args*
      url     (str "http://adventofcode.com/" year "/day/" day "/input")
      dest    (str "../puzzle-inputs/" year "/" "day" day)]
  (->> (curl/get url {:headers {"Cookie" (str "session=" cookie)}})
       :body
       (spit dest)))