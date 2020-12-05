(ns stuartstein777.2016.day4
  (:require [clojure.string :as str]))

;; part 1
(defn comparer [[k1 v1] [k2 v2]]
  (cond (< v1 v2) 1
        (> v1 v2) -1
        (= v1 v2) (< (int k1) (int k2))))

(defn real-room? [{:keys [room freqs]}]
  (let [char-freqs (frequencies room)]
    (->> (sort comparer char-freqs)
        (take 5)
        (map first)
        (apply str)
        (= freqs))))

(defn parse-room [entry]
  (let [splits (rest (first (re-seq #"([a-z|-]+)(\d+)\[([a-z]+)+\]" entry)))]
    {:room   (str/replace (first splits) "-" "")
     :sector (Integer/parseInt (second splits))
     :freqs  (nth splits 2)}))


(->> (slurp "puzzle-inputs/2016/day4")
     (str/split-lines)
     (map parse-room)
     (filter real-room?)
     (map :sector)
     (reduce +))

;; part 2
(defn shift [sector c]
  (if (= \- c)
    \space
    (char (+ 97 (mod (+ sector (- (int c) 97)) 26)))))

(defn decrypt [{:keys [room sector]}]
  {:sector sector
   :decrypted-room (->> (map identity room)
                        (map (partial shift sector))
                        (apply str))})

(->> (slurp "puzzle-inputs/2016/day4")
     (str/split-lines)
     (map parse-room)
     (filter real-room?)
     (map decrypt)
     (filter #(str/includes? (:decrypted-room %) "north")))