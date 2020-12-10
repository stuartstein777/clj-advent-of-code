(ns stuartstein777.2020.day10
  (:require [clojure.string :as str]))

;; test input is adapters.
(def test-input "16\n10\n15\n5\n1\n11\n7\n19\n6\n12\n4")
(def test-input2 "28\n33\n18\n42\n31\n14\n46\n20\n48\n47\n24\n23\n49\n45\n19\n38\n39\n11\n1\n32\n25\n35\n8\n17\n7\n9\n4\n2\n34\n10\n3")

(defn parse-input [input]
  (->> (str/split-lines input)
       (map #(Integer/parseInt %))
       (sort)))

;; part 1
(let [input (->> (slurp "puzzle-inputs\\2020\\day10")
                 (parse-input)
                 (sort))
      diffs (->> (map (fn [x y] (- y x)) input (rest input))
                 (frequencies))]
  (* (inc (diffs 3))
     (inc (diffs 1))))

;; part 2
;; all the ways we can arrange the adapters with gaps of 1 or 3

(sort (parse-input test-input))

;[1 4 5 6 7 10 11 12 15 16 19]
; [1 > 4]       1
; [4 > 5 6 7]   3
; [5 > 6 7]     2
; [6 > 7]       1
; [7 > 10]      1
; [10 > 11 12]  2
; [12 > 15]     1
; [15 > 16]     1
; [16 > 19]     1
;               7 combos + (full combo) = 8

(defn find-combos [input n]
  (filter #(<= 1 (- % n) 3) input))

;; doesnt work :(
(let [input (parse-input test-input2)]
  (->> (map (partial find-combos input) input)
       (map count)
       (filter #(> % 1))
       (reduce *)))

;; part 2 redux
;1, 4, 5, 6, 7, 10, 11, 12, 15, 16, 19
;1, 4, 5, 6, 7, 10, 12, 15, 16, 19
;1, 4, 5, 7, 10, 11, 12, 15, 16, 19
;1, 4, 5, 7, 10, 12, 15, 16, 19
;1, 4, 6, 7, 10, 11, 12, 15, 16, 19
;1, 4, 6, 7, 10, 12, 15, 16, 19
;1, 4, 7, 10, 11, 12, 15, 16, 19
;1, 4, 7, 10, 12, 15, 16, 19

(reduce * 1 (map (fn [n] (cond (= n 4) 4
                               (= n 3) 2))
                 (map count
                      (loop [input      (concat [0] (parse-input test-input2) [52])
                             idx        0
                             partitions []]
                        (println idx "::" input)
                        (if (and (< idx (count input)))
                          (let [cur          (nth input idx)
                                within-three (take-while #(<= % (+ cur 3)) (drop idx input))]
                            (println "   " within-three)
                            (if (> 3 (count within-three))
                              (recur input (inc idx) partitions)
                              (recur input (+ idx (count within-three))  (conj partitions within-three))
                              )) partitions)))))

(let [idx 0
     input [1 4 5 6 7 10 11 12 15 16 19]
    cur (nth input idx )  ]
  (take-while #(<= % (+ cur 3)) (drop idx input)))

(parse-input test-input2)