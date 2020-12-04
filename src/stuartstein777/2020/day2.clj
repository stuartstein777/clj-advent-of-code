(ns stuartstein777.2020.day2
  (:require [clojure.string :as str]))

;; part 1
(defn valid? [{:keys [min max c pwd]}]
  (<= min ((frequencies pwd) c 0) max))

;; part 2
(defn xor [a b]
  (or (and a (not b))
      (and b (not a))))

(defn valid? [{:keys [min max c pwd]}]
  (let [p1 (nth pwd (dec min) \_)
        p2 (nth pwd (dec max) \_)]
    (xor (= c p1) (= c p2))))

;; shared
(defn parse-entry [entry]
  (let [[min max c pwd] (rest (re-find #"(\d+)-(\d+) (.): (.+)" entry))]
    {:min (Integer/parseInt min)
     :max (Integer/parseInt max)
     :c (first c)
     :pwd pwd}))

(->> (slurp "puzzle-inputs/2020/day2")
     (str/split-lines)
     (map parse-entry)
     (filter valid?)
     (count))