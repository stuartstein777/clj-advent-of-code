(ns stuartstein777.2021.day10
  (:require [stuartstein777.file :as f]
            [stuartstein777.utils :as u]
            [clojure.string :as str]
            [clojure.math.combinatorics :as combo]
            [clojure.set :as set]))

;; part 1
(def scores {\} 1197
           \) 3
           \] 57
           \> 25137})

(def matching {\{ \}
             \[ \]
             \< \>
             \( \)})

(defn parser [line]
  (map identity line))

(defn find-syntax-error [opens [f & r]]
  (if (empty? r)
    nil
    (cond (#{\{ \( \[ \<} f)
          (recur (conj opens f) r)

          (#{\} \) \] \>} f)   ;
          (if (= f (matching (peek opens)))
            (recur (pop opens) r)
            f))))

(->> (f/read-all-lines-and-parse "puzzle-inputs/2021/day10" parser)
     (map (partial find-syntax-error []))
     (remove nil?)
     (map scores)
     (reduce +))

;; part 2
(def scores {\} 3
           \) 1
           \] 2
           \> 4})

(def matcher {\{ \}
              \[ \]
              \< \>
              \( \)})

(defn get-remaining [open [c & r]]
  (if (nil? c)
    open
    (cond (#{\{ \[ \< \(} c)
          (recur (conj open c) r)

          (#{\} \] \> \)} c)
          (if (= c (matcher (peek open)))
            (recur (pop open) r)
            nil))))

(defn score [auto-completes]
  (reduce (fn [acc i]
            (+ (* acc 5) (scores i))) 0 auto-completes))

(defn auto-complete [line]
  (map matcher line))

(defn get-middle-score [scores]
  (let [sorted-scores (sort scores)]
    (nth sorted-scores (dec (Math/ceil (/ (count scores) 2.0))))))

(->> (f/read-all-lines-and-parse "puzzle-inputs/2021/day10" parser)
     (map (partial get-remaining []))
     (remove nil?)
     (map auto-complete)
     (map reverse)
     (map score)
     (get-middle-score))