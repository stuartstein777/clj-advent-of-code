(ns stuartstein777.2022.day5
  (:require [clojure.string :as str]))

(defn remove-empty [stacks]
  (reduce (fn [acc i]
            (assoc acc i (drop-while #(= "-" %) (acc i))))
          stacks
          (range 1 (inc (count stacks)))))

(defn parse-stack [stack]
  (-> stack
      (str/replace #"\s\s\s\s" "-")
      #_#_#_#_(str/split-lines)
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
    (parse-stack stack)
    #_[(parse-stack stack) (map parse-instruction (str/split-lines instructions))]
    ))

(parse)
;; part 1
(defn move-pt1 [stack [to-move from to]]
  (-> stack
      (assoc from (drop to-move (stack from)))
      (update to (partial apply conj) (take to-move (stack from)))))

(let [[stack instructions] (->> (parse))]
  (as-> (reduce move-pt1 stack instructions) o
        (sort o)
        (map (comp first second) o)
        (apply str o)
        (str/replace o #"\[|]" "")))

;; PTWLTDSJV

;; Part 2
(defn move-pt2 [stack [to-move from to]]
  (-> stack
      (assoc from (drop to-move (stack from)))
      (update to (partial apply conj) (reverse (take to-move (stack from))))))

(let [[stack instructions] (->> (parse))]
  (as-> (reduce move-pt2 stack instructions) o
        (sort o)
        (map (comp first second) o)
        (apply str o)
        (str/replace o #"\[|]" "")))

;; "WZMFVGGZP"