(ns exfn.messing
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.math.combinatorics :as combo]))

(defn parse-line [l]
  (let [[inst v] (str/split l #"\s")]
    (if v
      [{:inst :noop} {:inst inst :v (parse-long v)}]
      [{:inst :noop}])))

(def cycles-to-log (iterate (fn [n] (+ n 40)) 20))

(defn process [input cycles-to-log]
  (reduce 
   (fn [{:keys [cycle x cycles-log x-log] :as acc}
       {:keys [inst v] :as i}] 
     (let [new-cycle (inc cycle)
           new-x (if (= inst :noop)
                      x
                   (+ x v))
           new-cycles-log (if (cycles-to-log new-cycle)
                            (conj cycles-log (* new-cycle new-x))
                            cycles-log)]
       #_(prn cycle new-x)
            {:cycle new-cycle
             :x new-x
             :cycles-log new-cycles-log
             :x-log (conj x-log x)}))
          {:cycle 1 :x 1 :cycles-log [] :x-log []} input))

;; part 1
(let [input (->> (slurp "puzzle-inputs/day10")
                 (str/split-lines)
                 (mapcat parse-line))
      cycles-to-log (set (take 11 cycles-to-log))]
  (->> (process input cycles-to-log)
       :cycles-log
       (reduce +)))


;; part 2
(let [input (->> (slurp "puzzle-inputs/day10")
                 (str/split-lines)
                 (mapcat parse-line))
      cycles-to-log (set (take 11 cycles-to-log))
      x-log (->> (process input cycles-to-log)
                 :x-log)]
  (println
   (->>
    (reduce 
     (fn [acc i]
       (if (<= (Math/abs (- (mod i 40) (nth x-log i))) 1)
         (str acc "⭐")
         (str acc "⬛")))
     "" (range 0 240))
    (partition 40)
    (map (partial apply str))
    (str/join "\n"))))

"
⭐⭐⭐⬛⬛⭐⭐⭐⭐⬛⭐⬛⬛⭐⬛⭐⭐⭐⭐⬛⬛⭐⭐⬛⬛⭐⭐⭐⬛⬛⭐⭐⭐⭐⬛⭐⭐⭐⭐⬛
⭐⬛⬛⭐⬛⭐⬛⬛⬛⬛⭐⬛⭐⬛⬛⬛⬛⬛⭐⬛⭐⬛⬛⭐⬛⭐⬛⬛⭐⬛⭐⬛⬛⬛⬛⭐⬛⬛⬛⬛
⭐⬛⬛⭐⬛⭐⭐⭐⬛⬛⭐⭐⬛⬛⬛⬛⬛⭐⬛⬛⭐⬛⬛⬛⬛⭐⬛⬛⭐⬛⭐⭐⭐⬛⬛⭐⭐⭐⬛⬛
⭐⭐⭐⬛⬛⭐⬛⬛⬛⬛⭐⬛⭐⬛⬛⬛⭐⬛⬛⬛⭐⬛⬛⬛⬛⭐⭐⭐⬛⬛⭐⬛⬛⬛⬛⭐⬛⬛⬛⬛
⭐⬛⭐⬛⬛⭐⬛⬛⬛⬛⭐⬛⭐⬛⬛⭐⬛⬛⬛⬛⭐⬛⬛⭐⬛⭐⬛⬛⬛⬛⭐⬛⬛⬛⬛⭐⬛⬛⬛⬛
⭐⬛⬛⭐⬛⭐⬛⬛⬛⬛⭐⬛⬛⭐⬛⭐⭐⭐⭐⬛⬛⭐⭐⬛⬛⭐⬛⬛⬛⬛⭐⭐⭐⭐⬛⭐⬛⬛⬛⬛
"
