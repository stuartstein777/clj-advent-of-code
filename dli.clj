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
       (spit dest))
  
  (spit (str "src/stuartstein777/" yr "/day" dy ".clj")
        (str "(ns stuartstein777." yr ".day" dy ")")))

;; run with bb dli.clj >year< >day<
;; e.g. $ bb dli.clj 2024 1
;; needs session cookie in .session, get this by logging into browser and getting value from cookie tab in dev tools

