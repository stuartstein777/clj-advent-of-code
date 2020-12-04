(ns stuartstein777.2019.day2)

(def program [1,12,2,3,1,1,2,3,1,3,4,3,1,5,0,3,2,1,10,19,1,6,19,23,2,23,6,27,2,6,27,31,2,13,31,35,1,10,35,39,2,39,13,43,1,43,13,47,1,6,47,51,1,10,51,55,2,55,6,59,1,5,59,63,2,9,63,67,1,6,67,71,2,9,71,75,1,6,75,79,2,79,13,83,1,83,10,87,1,13,87,91,1,91,10,95,2,9,95,99,1,5,99,103,2,10,103,107,1,107,2,111,1,111,5,0,99,2,14,0,0])

;; part 1

(defn intcode-computer [input]
  ((fn [input input-pos]
     (if (= (nth input input-pos) 99)
       (first input)
       (let [output-pos (nth input (+ input-pos 3))
             input-val (nth input input-pos)
             opcode-1 (nth input (nth input (inc input-pos)))
             opcode-2 (nth input (nth input (+ 2 input-pos)))]
         (cond (= 1 input-val) (recur (update input output-pos (fn [_] (+ opcode-1 opcode-2))) (+ 4 input-pos))
               (= 2 input-val) (recur (update input output-pos (fn [_] (* opcode-1 opcode-2))) (+ 4 input-pos))
               :else (first input))))) input 0))
(intcode-computer program)

;; part 2

(defn find-inputs [input]
  ((fn [a b]
     (let [new-code (assoc input 1 a 2 b)
           result (intcode-computer new-code)]
       (if (= 19690720 result)
         (+ b (* 100 a))
         (if (= b 99)
           (recur (inc a) 0)
           (recur a (inc b)))))) 0 0))

(find-inputs program)