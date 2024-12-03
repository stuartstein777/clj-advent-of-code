(ns stuartstein777.2024.day3
  (:require [clojure.string :as str]))

;; part 1
(defn parse [mul]
  (->> (re-seq #"\d+", mul)
       (map parse-long)))

(->> "puzzle-inputs/2024/day3"
     (slurp)
     (re-seq #"mul\(\d+,\d+\)")
     (map parse)
     (map #(reduce * 1 %))
     (reduce +)) ;; 174960292

;; part 2
(defn reducer [{:keys [doing total] :as acc} {:keys [instr values] :as i}]
  (cond (= :mul instr) 
        (if doing
          (assoc acc :total (+ total (reduce * 1 values)))
          acc)
        (= :do instr)  (assoc acc :doing true)
        (= :dont instr) (assoc acc :doing false)))

(defn parse2 [instr]
  (cond
    (str/starts-with? instr "mul")    {:instr :mul :values (->> (re-seq #"\d+", instr)
                                                                (map parse-long))}
    (= instr "don't()")               {:instr :dont :values []}
    (= instr "do()")                  {:instr :do :values []}))

(->> "puzzle-inputs/2024/day3"
     (slurp)
     (re-seq #"mul\(\d+,\d+\)|don't\(\)|do\(\)")
     (map parse2)
     (reduce reducer {:doing true, :total 0}))

