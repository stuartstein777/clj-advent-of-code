(ns stuartstein777.2020.day9
  (:require [clojure.string :as str]))

(def test-input [35 20 15 25 47 40 62 55 65 95 102 117 150 182 127 219 299 277 309 576])

(defn parse-input []
  (->> (slurp "puzzle-inputs/2020/day9")
       (str/split-lines)
       (map #(Long/parseLong %))))

(defn is-valid? [part]
  (let [n (last part)
        vals (butlast part)]
    (loop [i 0]
      (let [target (- n (first (drop i vals)))
            remaining (set (drop (inc i) vals))]
        (if (seq remaining)
          (if (remaining target)
            true
            (recur (inc i)))
          false)))))

(defn xmas-part1 [preamble-length]
  (let [input (parse-input)
        valid-entries (for [xs (drop preamble-length (partition (inc preamble-length) 1 input))
                            :let [valid? (is-valid? xs)]
                            :while valid?]
                        (last xs))]
    (first (drop (+ (* 2 preamble-length) (count valid-entries)) input))))

(xmas-part1 25)



;; part 2
(def test-input [35 20 15 25 47 40 62 55 65 95 102 117 150 182 127 219 299 277 309 576])

(defn reduce-cont [part]
  (let [min (apply min part)
      max (apply max part)]
      [(+ min max) (reduce + 0 part)]))

(time (let [invalid 14144619
            input (parse-input)]
        (loop [i 4]
          (let [sums (->> (partition i 1 input)
                          (map reduce-cont))
                found? (for [xs sums
                             :when (= invalid (second xs))]
                         (first xs))]
            (if (seq found?)
              (first found?)
              (recur (inc i)))))))