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

;; part 2
(defn make-reindeer [[reindeer kms duration rest-seconds]]
  {reindeer {:speed kms
             :duration duration
             :allowed-moving-duration duration
             :current-resting-duration 0
             :required-rest rest-seconds       
             :distance-travellled 0
             :points 0}})

(defn update-reindeer [{:keys [speed duration allowed-moving-duration required-rest 
                            current-resting-duration distance-travellled]
                     :as reindeer}]
  
  )

(let [reindeers (->> (slurp "puzzle-inputs/2015/day14")
                     (str/split-lines)
                     (map parse-line)
                     (map make-reindeer)
                     (apply merge))]
  #_(reduce update-all-reindeers reindeers (repeat 2053 1)))


{"Comet"   {:speed                    3
            :duration                 37
            :rest-seconds             76
            :current-moving-duration  0
            :current-resting-duration 0
            :distance-travellled      0
            :points                   0},
 "Cupid"   {:speed                    12
            :duration                 4
            :rest-seconds             43
            :current-moving-duration  0
            :current-resting-duration 0
            :distance-travellled      0
            :points                   0},
 "Blitzen" {:speed                    13
            :duration                 4
            :current-moving-duration  0
            :current-resting-duration 0
            :rest-seconds             49
            :distance-travellled      0
            :points                   0},
 "Rudolph" {:speed                    20
            :duration                 7
            :rest-seconds             132
            :current-moving-duration  0
            :current-resting-duration 0
            :distance-travellled      0
            :points                   0},
 "Donner"  {:speed                    9
            :duration                 5
            :current-moving-duration  0
            :current-resting-duration 0
            :rest-seconds             38
            :distance-travellled      0
            :points                   0},
 "Vixen"   {:speed                    8
            :duration                 8
            :rest-seconds             53
            :current-moving-duration  0
            :current-resting-duration 0
            :distance-travellled      0
            :points                   0}
 "Dancer"  {:speed                    37
            :duration                 1
            :rest-seconds             36
            :current-moving-duration  0
            :current-resting-duration 0
            :distance-travellled      0
            :points                   0}
 "Dasher"  {:speed                    10
            :duration                 4
            :current-moving-duration  0
            :current-resting-duration 0
            :rest-seconds             37
            :distance-travellled      0
            :points                   0}
 "Prancer" {:speed                    9
            :duration                 12
            :current-moving-duration  0
            :current-resting-duration 0
            :rest-seconds             97
            :distance-travellled      0
            :points                   0}}
