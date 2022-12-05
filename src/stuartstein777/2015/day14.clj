(ns stuartstein777.2015.day14
  (:require [clojure.string :as str]))

(defn parse-line [l]
  (let [[_ reindeer speed duration rest] (re-find #"^(\w+) can fly (\d+) km\/s for (\d+) seconds, but then must rest for (\d+)" l)]
       [reindeer (parse-long speed) (parse-long duration) (parse-long rest)]))


(defn calculate-distance [[reindeer kms duration rest-seconds]]
  (let [total-duration 2503
        block-of-time (+ duration rest-seconds)
        complete-blocks (Math/floor (/ (float total-duration)
                                       block-of-time))
        remainder (mod total-duration (+ duration rest-seconds))]
    [reindeer (+
               (* complete-blocks duration kms)
               (if (> remainder duration)
                 (* duration kms)
                 0))]))

;; part 1
(->> (slurp "puzzle-inputs/2015/day14")
     (str/split-lines)
     (map parse-line)
     (map calculate-distance)
     (sort-by second >))

