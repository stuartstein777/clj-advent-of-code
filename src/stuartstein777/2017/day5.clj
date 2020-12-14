(ns stuartstein777.2017.day5
  (:require [clojure.string :as str]))
  
  (defn parse-input []
  (->> (slurp "resources/2017/day5")
       (str/trim)
       (str/split-lines)
       (mapv #(Integer/parseInt %))))

;; part 1
(let [input [0 3 0 1 -3] #_(parse-input)
    end (count input)]
  (loop [index 0
         input input
         steps 0]
    (if (>= index end)
      steps
      (let [next (nth input index)]
        (recur (+ index next) (assoc input index (inc next)) (inc steps))))))

;; part 2
(time (let [input (parse-input)
            end (count input)]
        (loop [index 0
               input input
               steps 0]
          (if (>= index end)
            steps
            (let [next (long (nth input index))]
              (recur (+ index next) (assoc input index (if (>= next 3) (dec next) (inc next))) (inc steps)))))))
