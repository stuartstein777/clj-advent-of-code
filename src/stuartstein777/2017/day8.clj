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

(defn get-max-register [m]
  (->> m :registers vals (apply max)))

(defn update-running-max [{:keys [max] :as acc}]
  (let [max-reg (get-max-register acc)]
    (if (> max-reg max)
      (assoc acc :max max-reg)
      acc)))

(defn reducer [acc {:keys [register operation op-value con-reg condition con-val]}]
  (let [reg-val     (get-in acc [:registers register] 0)
        con-reg-val (get-in acc [:registers con-reg] 0)]
    (-> acc
        (assoc-in [:registers register] (if (condition con-reg-val con-val)
                                          (operation reg-val op-value)
                                          reg-val))
        update-running-max)))

(->> (f/read-all-lines-and-parse "puzzle-inputs/2017/day8" parse)
     (reduce reducer {:max 0 :registers {}})
     ((juxt get-max-register :max)))
