(ns stuartstein777.2022.day5
  (:require [clojure.string :as str]))

(defn remove-empty [stacks]
  (reduce (fn [acc i]
            (assoc acc i (drop-while #(= "[-]" %) (acc i))))
          stacks
          (range 1 (inc (count stacks)))))

(defn parse-stack [stack]
  (-> stack
      (str/replace #"\s\s\s\s" "[-]")
      (str/split #"\n")
      (->> (map #(str/replace % #"]\[" "] [")))
      butlast
      (->> (map #(str/split % #" "))
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

(defn move [stack [to-move from to]]
  (-> stack
      (assoc from (drop to-move (stack from)))
      (update to (partial apply conj) (take to-move (stack from)))))


(let [[stack instructions] (->> (parse))]
  (str/replace 
   (->> (reduce move stack instructions)
        (sort)
        (map (comp first second))
        (apply str))
   #"\[|]"
   ""))
  


;; PTWLTDSJV

;; Part 2

