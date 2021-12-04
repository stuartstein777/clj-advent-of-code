(ns stuartstein777.2021.day3
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [stuartstein777.file :as f]
            [stuartstein777.utils :as u]))

(defn parser [l]
  (map identity l))

(defn get-key-with-highest-val [m]
  (if (>= (m \1 0) (m \0 0))
    \1
    \0))

(defn flip-bits [s]
  (map (fn [i] ({\1 \0 \0 \1} i)) s))

(defn binary-str->int [s]
  (Integer/parseInt s 2))

(defn char-seq->str [cs]
  (->> (map str cs)
       (str/join "")))

;; part 1
(let [input (->> (f/read-all-lines-and-parse "puzzle-inputs/2021/day3" parser)
                 (apply map vector)
                 (map frequencies))
      γ-bin (->> input
                 (map get-key-with-highest-val))
      ε     (->> γ-bin
                 (flip-bits)
                 (char-seq->str)
                 (binary-str->int))
      γ     (->> γ-bin
                 (char-seq->str)
                 (binary-str->int))]
  (* ε γ))

;; part 2
(defn has-v-at-idx [idx v xs]
  (= v (nth xs idx)))

(defn reduce-binary-over-f [input f idx]
  (if (= 1 (count input))
    (-> (first input) char-seq->str binary-str->int)
    (let [bits-at-idx (nth (apply map vector input) idx)
          most-common (->> (frequencies bits-at-idx)
                           (get-key-with-highest-val)
                           (f))]
      (recur (filter (partial has-v-at-idx idx most-common) input)
             f
             (inc idx)))))

(let [input (f/read-all-lines-and-parse "puzzle-inputs/2021/day3" parser)
      oxygen-generator (reduce-binary-over-f input identity 0)
      oxygen-scrubber (reduce-binary-over-f input {\1 \0 \0 \1} 0)]
  (* oxygen-generator oxygen-scrubber))