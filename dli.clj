(ns dli
  (:require [babashka.curl :as curl]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def cookie (str/trim-newline (slurp ".session")))

(let [[yr dy] *command-line-args*
      url     (str "https://adventofcode.com/" yr "/day/" dy "/input")
      dest    (format "puzzle-inputs/%s/day%s" yr (Long/parseLong dy))]
  (->> (curl/get url {:headers {"Cookie" (str "session=" cookie)}})
       :body
       (spit dest)))