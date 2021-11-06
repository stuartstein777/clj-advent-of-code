(ns stuartstein777.2018.day5
  (:require [clojure.string :as str]))

(def reactors (mapcat (fn [s] [(str (str/upper-case s) s) (str s (str/upper-case s))]) "abcdefghijklmnopqrstuvwxyz"))

(defn replacer [s m]
  (str/replace s m ""))

(defn react [s]
  (let [reacted (reduce (fn [acc m] (replacer acc m)) s reactors)]
    (if (= s reacted)
      reacted
      (recur reacted))))

;; part 1
(-> (slurp "puzzle-inputs/2018/day5")
    (react)
    (count))

;; part 2
(let [orig (slurp "puzzle-inputs/2018/day5")
      all (map (fn [s]
                 (-> orig
                     (str/replace (subs s 0 1) "")
                     (str/replace (subs s 1) ""))) (take-nth 2 reactors))]
  (->> (map react all)
       (map count)
       (apply min)))

