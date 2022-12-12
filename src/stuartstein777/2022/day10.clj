(ns stuartstein777.2022.day10
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [medley.core :as med]))

(defn parse-line [l]
  (let [[inst v] (str/split l #"\s")]
    (if v
      [{:inst :noop} {:inst inst :v (parse-long v)}]
      [{:inst :noop}])))

(def cycles-to-log (iterate (fn [n] (+ n 40)) 20))

(defn process [input cycles-to-log]
  (reduce 
   (fn [{:keys [cycle x cycles-log] :as acc}
       {:keys [inst v] :as i}] 
     (let [new-cycle (inc cycle)
           new-x (if (= inst :noop)
                      x
                   (+ x v))
           new-cycles-log (if (cycles-to-log new-cycle)
                            (conj cycles-log (* new-cycle new-x))
                            cycles-log)]
       (prn cycle new-x)
            {:cycle new-cycle
             :x new-x
             :cycles-log new-cycles-log}))
          {:cycle 1 :x 1 :cycles-log []} input))

(let [input (->> (slurp "puzzle-inputs/day10")
                 (str/split-lines)
                 (mapcat parse-line))
      cycles-to-log (set (take 11 cycles-to-log))]
  (->> (process input cycles-to-log)
       :cycles-log
       (reduce +)))
