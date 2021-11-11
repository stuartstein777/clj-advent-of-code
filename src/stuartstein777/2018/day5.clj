(ns stuartstein777.2018.day5
  (:require [clojure.string :as str]))

(defn make-reactor [s]
  (let [reactor (str (str/upper-case s) s)]
    [reactor (str/reverse reactor)]))

(def reactors (mapcat make-reactor "abcdefghijklmnopqrstuvwxyz"))

(defn react [s]
  (let [reacted (reduce (fn [acc m] (str/replace acc m "")) s reactors)]
    (if (= s reacted)
      reacted
      (recur reacted))))

;; part 1
(-> (slurp "puzzle-inputs/2018/day5")
    (react)
    (count))

;; part 2
(defn reactor-pattern [s]
  (re-pattern (str (subs s 0 1) "|" (subs s 1))))

(let [orig (slurp "puzzle-inputs/2018/day5")
      all (map #(str/replace orig (reactor-pattern %) "") (take-nth 2 reactors))]
  (->> (map react all)
       (map count)
       (apply min)))

