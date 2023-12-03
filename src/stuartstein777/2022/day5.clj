(ns stuartstein777.2022.day5
  (:require [clojure.string :as str]))

(defn remove-empty [stacks]
  (reduce (fn [acc i] (assoc acc i (remove #(= "-" %) (acc i))))
          stacks
          (range 1 (inc (count stacks)))))

(defn parse-stack [stack]
  (-> stack
      (str/replace #"\s\s\s\s" "-")           
      (str/replace #"\[|]| " "")              
      (str/split-lines)                       
      (->> (map #(str/split % #""))           
           (apply map vector)
           (zipmap (range 1 (count stack)))
           (remove-empty))))

(defn parse-instruction [instruction]
  (let [[_ to-move _ from _ to] (str/split instruction #"\s")]
    [(parse-long to-move)
     (parse-long from)
     (parse-long to)]))

(defn parse []
  (let [[stack instructions]
        (-> (slurp "puzzle-inputs/2022/day5")
            (str/split #"\n\n"))]
    [(parse-stack stack) (map parse-instruction (str/split-lines instructions))]))

;; part 1
(defn move-pt1 [stack [to-move from to]]
  (-> stack
      (assoc from (drop to-move (stack from)))
      (update to (partial apply conj) (take to-move (stack from)))))

(defn solve [move-fn]
  (let [[stack instructions] (->> (parse))]
    (as-> (reduce move-fn stack instructions) ¬
          (sort ¬)
          (map (comp first second) ¬)
          (apply str ¬)
          (str/replace ¬ #"\[|]" ""))))

(solve move-pt1)
;; PTWLTDSJV

;; part 2
(defn move-pt2 [stack [to-move from to]]
  (-> stack
      (assoc from (drop to-move (stack from)))
      (update to (partial apply conj) (reverse (take to-move (stack from))))))

(solve move-pt2)
;; "WZMFVGGZP"