(ns stuartstein777.2016.day5
  (:require [clojure.string :as str])
  (:import (java.security MessageDigest)))

(def puzzle-input "ojvtpuvg")

(defn md5
  [^String s]
  (->> s
       .getBytes
       (.digest (MessageDigest/getInstance "MD5"))
       (BigInteger. 1)
       (format "%032x")))

(loop [i 0
       n 0
       pass []]
  (if (= n 8)
    (apply str (map (fn [p] (first (drop 5 p))) pass))
    (let [h (md5 (str puzzle-input i))]
      (if (str/starts-with? h "00000")
        (recur (inc i) (inc n) (conj pass h))
        (recur (inc i) n pass)))))

;; part 2

(defn validate-hash [h]
  (re-matches #"(00000)(\d)(.+)" h))

(defn print-pass [pass]
  (str
    (if (nil? (pass 0)) "_" (pass 0))
    (if (nil? (pass 1)) "_" (pass 1))
    (if (nil? (pass 2)) "_" (pass 2))
    (if (nil? (pass 3)) "_" (pass 3))
    (if (nil? (pass 4)) "_" (pass 4))
    (if (nil? (pass 5)) "_" (pass 5))
    (if (nil? (pass 6)) "_" (pass 6))
    (if (nil? (pass 7)) "_" (pass 7))))

(loop [i 0
       n 0
       pass {0 nil 1 nil 2 nil  3 nil 4 nil 5 nil 6 nil 7 nil}]
  (if (= n 8)
    (println (print-pass pass))
    (let [h (md5 (str puzzle-input i))]
      (if (and (str/starts-with? h "00000") (validate-hash h))
        (let [pos (Integer/parseInt (str (nth h 5)))
              pwd-char (nth h 6)]
          (if (and (<= 0 pos 7) (nil? (pass pos)))
            (do
              (println (print-pass pass))
              (recur (inc i) (inc n) (assoc pass pos pwd-char)))
            (recur (inc i) n pass)))
        (recur (inc i) n pass)))))