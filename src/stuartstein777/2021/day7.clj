(ns stuartstein777.2021.day7
  (:require [stuartstein777.file :as f]
            [stuartstein777.utils :as u]))

(defn get-distances [hps n]
  (mapv (fn [h] (Math/abs (- h n))) hps))

(let [input       (f/parse-csv-ints "puzzle-inputs/2021/day7")
      minimum     (apply min input)
      maximum     (apply max input)
      h-positions (u/range-inclusive minimum maximum)]
  (->> input
       (mapv (partial get-distances h-positions))
       (apply mapv vector)
       (mapv #(reduce + 0 %))
       (apply min)))

;; part 2
(defn build-row [min max n]
  (let [to-max (if (= n max) [] (reductions + (u/range-inclusive 1 (- max n))))
        to-min (reverse (reductions + (u/range-inclusive min n)))]
    (concat to-min to-max)))

(let [input   (f/parse-csv-ints "puzzle-inputs/2021/day7")
      minimum (apply min input)
      maximum (apply max input)]
  (->> input
       (mapv (partial build-row minimum maximum))
       (apply map vector)
       (mapv #(reduce + 0 %))
       (apply min)))
