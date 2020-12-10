(ns stuartstein777.2020.day8
  (:require [clojure.string :as str]))

(defn parse-line [line]
  (let [[inst op] (rest (first (re-seq #"(\w{3})\s([\+|\-]\d+)" line)))]
    [(keyword inst) (Integer/parseInt op)]))

(defn computer [{:keys [acc eip visited?] :as registers} instructions]
  (cond (visited? eip) [:infinite-loop acc]
        (>= eip (count instructions)) [:finished acc]
        :else
        (let [[instr op] (nth instructions eip)]
          (case instr
            :acc (recur (-> registers
                           (update :acc + op)
                           (update :eip inc)
                           (update :visited? conj eip)) instructions)

            :jmp (recur (-> registers
                           (update :eip + op)
                           (update :visited? conj eip)) instructions)

            :nop (recur (-> registers
                            (update :eip inc)
                            (update :visited? conj eip)) instructions)))))

(def test-input "nop +0\nacc +1\njmp +4\nacc +3\njmp -3\nacc -99\nacc +1\njmp -4\nacc +6")

;; part 1
(->> (slurp "puzzle-inputs/2020/day8")
     (str/split-lines)
     (map parse-line)
     ((partial computer {:acc 0 :eip 0 :visited? #{}})))

;; part 2
;; find all the jmp instructions and nop instructions and their eips
(let [instructions (->> (slurp "puzzle-inputs/2020/day8")
                         (str/split-lines)
                         (map parse-line))
      jmps-nops (->> instructions
                     (map (fn [r i] [r i]) (range (count instructions)))
                     (filter (fn [i] (or (= :jmp (first (second i)))
                                         (= :nop (first (second i))))))
                     (map first))
      updated-instructions (map (fn [n]
                                  (let [[inst op] (nth instructions n)]
                                    (if (= :jmp inst)
                                      (concat (take n instructions) [[:nop op]] (drop (inc n) instructions))
                                      (concat (take n instructions) [[:jmp op]] (drop (inc n) instructions))))
                                  ) jmps-nops)]
  (->> (map (partial computer {:acc 0 :eip 0 :visited? #{}}) updated-instructions)
       (filter (fn [r] (= :finished (first r))))))


