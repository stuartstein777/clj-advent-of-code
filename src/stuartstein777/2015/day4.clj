(ns stuartstein777.2015.day4
  (:require [clojure.string :as str])
  (:import (java.security MessageDigest)))

(def puzzle-input "ckczppom")

(defn md5
  [^String s]
  (->> s
       .getBytes
       (.digest (MessageDigest/getInstance "MD5"))
       (BigInteger. 1)
       (format "%032x")))

;; Part 1
(loop [i 0]
  (println i)
  (if (str/starts-with? (md5 (str "ckczppom" i)) "00000")
    i
    (recur (inc i))))

;; Part 2
(loop [i 0]
  (if (str/starts-with? (md5 (str "ckczppom" i)) "000000")
    i
    (recur (inc i))))
