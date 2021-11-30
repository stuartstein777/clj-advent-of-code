(ns stuartstein777.2017.day4
  (:require [clojure.string :as str]))

;; part 1
(defn duplicate-words? [line]
  (->> (frequencies line)
       (vals)
       (filter (fn [wc] (> wc 1)))))

#_(duplicate-words? ["jsdmfw" "ptjwrbl" "hhuv" "uolz" "adyweh" "qpj" "wxyogp" "igvnojq" "jmfw" "pqs" "fsnirby"])

(defn parse-input []
  (->> (slurp "puzzle-inputs/2017/day4")
       (str/split-lines)
       (map #(str/split % #" "))))

(let [input (parse-input)]
  (->> (map duplicate-words? input)
       (filter empty?)
       (count)))

;; part 2
(defn valid? [passphrase]
  (let [word-sets (map set passphrase)
        all-count (count word-sets)
        distinct-count (count (distinct word-sets))]
    (= all-count distinct-count)))

(let [input (parse-input)]
  (->> (filter valid? input)
       (count)))
