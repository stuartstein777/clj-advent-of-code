(ns stuartstein777.2018.day2
  (:require [clojure.string :as str]
            [clojure.set :as set]))

;; part 1
(let [input (->> (slurp "puzzle-inputs/2018/day2")
                 (str/split-lines)
                 (map frequencies))
      twos (filter (fn [x] (some #{2} (vals x))) input)
      threes (filter (fn [x] (some #{3} (vals x))) input)]
  (* (count twos) (count threes)))

;; part 2
(def test-input "abcde\nfghij\nklmno\npqrst\nfguij\naxcye\nwvxyz")

(let [c (->> (slurp "puzzle-inputs/2018/day2")
             (str/trim)
             (map (fn [p] (if (= p \() 1 -1)))
             (reductions +))]
  (->> (take-while #(not= -1 %) c)
       (count)
       (inc)))

