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

(defn differ-by-1 [x y]
  (and (not= x y)
       (= 1 (count (filter false? (map #(= %1 %2) x y))))))

(defn get-shared-chars [x y]
  (apply str (remove nil? (map #(if (= %1 %2) %1 nil) x y))))

(get-shared-chars "abc" "axc")

(let [strs (->> (slurp "puzzle-inputs/2018/day2")
                #_test-input
                (str/split-lines))]
  (->> (for [x strs
             y strs
             :when (differ-by-1 x y)]
         #{x y}
         )
       (distinct)
       (first)
       (apply get-shared-chars)))