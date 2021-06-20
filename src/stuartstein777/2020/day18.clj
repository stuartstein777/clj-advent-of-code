(ns stuartstein777.2020.day18
  (:require [clojure.string :as str]))

(defn dash-type [expr loc]
  (let [prev (nth expr (dec loc) \-)]
    (if (or (= \) prev) (number? prev))
      :subtraction
      :negation)))

(defn parse [token]
  (if (Character/isDigit ^char (first token))
    (Double/parseDouble token)
    (first token)))

(defn tokenize [expr]
  (let [expr (str/replace expr #" " "")
        expr-tks (->> (re-seq #"(\d+(\.\d+)?)|." expr)
                      (map first)
                      (map parse))]
    (map-indexed (fn [ix t]
                   (cond (= \- t)
                         (if (= :subtraction (dash-type expr-tks ix)) \- \~)
                         :else t)) expr-tks)))


(def operator-precedence
  {\( 1,
   \) 1,
   \+ 3,   ; change to 2 to solve part 1. Change to 3 to solve part 2.
   \- 2,
   \* 2,
   \/ 2,
   \~ 2})

;; Returns precedence of op1 compared to op2.
;; e.g. (precedence \* \+) => :higher
(defn precedence [op1 op2]
  (cond (< (operator-precedence op1) (operator-precedence op2)) :lower
        (> (operator-precedence op1) (operator-precedence op2)) :higher
        :else :same))

(defn pop-brackets [{:keys [output op-stack]}]
  (let [ops (take-while #(not= % \() op-stack)
        output (apply conj output ops)]
    {:output output :op-stack (vec (drop (inc (count ops)) op-stack))}))

(defn pop-lower-or-same-precedence [{:keys [output op-stack]} t]
  (if (or (empty? op-stack) (= (precedence t (first op-stack)) :higher))
    {:output output :op-stack (concat [t] op-stack)}
    (let [ops      (take-while #(not= :higher (precedence t %)) op-stack)
          op-stack (concat [t] (drop (count ops) op-stack))]
      {:output (apply conj output ops) :op-stack op-stack})))

(defn to-rpn [tokens]
  (let [acc (reduce (fn [acc t]
                      (cond (number? t)
                            (update acc :output conj t)

                            (or (= t \() (= t \~))
                            (update acc :op-stack #(concat [t] %))

                            (= t \))
                            (pop-brackets acc)

                            :else
                            (if (= :higher (operator-precedence t (first (acc :op-stack))))
                              (update acc :op-stack #(concat [t] %))
                              (pop-lower-or-same-precedence acc t))))
                    {:output [] :op-stack []}
                    tokens)]
    (apply conj (:output acc) (:op-stack acc))))


(defn get-function [i]
  (cond (= i \+) +
        (= i \-) -
        (= i \*) *
        (= i \/) /))

(defn rpn-reducer [operand-stack i]
  (cond (number? i)
        (concat [i] operand-stack)

        (= \~ i)
        (concat [(- (first operand-stack))] (drop 1 operand-stack))

        :else
        (let [op1 (first operand-stack)
              op2 (second operand-stack)
              res ((get-function i) op2 op1)]
          (concat [res] (drop 2 operand-stack)))))

(defn evaluate-rpn [rpn]
  (first (reduce rpn-reducer [] rpn)))

(defn evaluate [expr]
  (->> (tokenize expr)
       (to-rpn)
       (evaluate-rpn)))

(defn parse-input []
  (->> (slurp "puzzle-inputs/2020/day18")
       (str/split-lines)))

(->> (parse-input)
     (map evaluate)
     (reduce +)
     (bigint))

