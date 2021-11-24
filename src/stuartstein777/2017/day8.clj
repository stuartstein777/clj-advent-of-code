(ns stuartstein777.2017.day8
  (:require [stuartstein777.file :as f]))

(defn parse-op [op]
  (condp = op
    "inc" +
    "dec" -))

(defn parse-condition [con]
  (condp = con
    "==" =
    "!=" not=
    ">=" >=
    ">"  >
    "<=" <=
    "<"  <))

(defn parse [line]
  (let [[reg op v con-reg con cv] (rest (re-find #"(\w+)\s(\w+)\s(-?\d+)\sif\s(\w+)\s(\S+)\s(-?\d+)" line))]
    {:register  reg
     :operation (parse-op op)
     :op-value  (Integer/parseInt v)
     :con-reg   con-reg
     :condition (parse-condition con)
     :con-val   (Integer/parseInt cv)}))

(defn get-max-val [m]
  (->> m
       vals
       (apply max)))

(defn get-largest-register [m]
  (->> (dissoc m :max)
       (vals)
       (apply max)))

(defn reducer [{:keys [max] :as acc}
            {:keys [register operation op-value con-reg condition con-val]}]
  (let [reg-val     (get acc register 0)
        con-reg-val (get acc con-reg 0)
        m (assoc acc register (if (condition con-reg-val con-val)
                                (operation reg-val op-value)
                                reg-val))
        new-max (get-max-val m)]
    (if (> new-max max)
      (assoc m :max new-max)
      m)))

(->> (f/read-all-lines-and-parse "puzzle-inputs/2017/day8" parse)
     (reduce reducer {:max 0})
     ((juxt get-largest-register :max)))
