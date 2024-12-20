(ns stuartstein777.utils
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn point->idx
  "Returns array index for given [x y] coordinate."
  [[x y] x-width]
  (+ y (* x x-width)))

(defn distance
 "Returns distance between 2 points"
  [[x1 y1] [x2 y2]]
  (Math/sqrt (+ (Math/pow (- x2 x1) 2)
                (Math/pow (- y2 y1) 2))))

(defn manhatten-distance-2d 
  "Returns manhatten distance between any two points in a 2d grid."
  [[x1 y1] [x2 y2]]
  (+ (Math/abs (- x2 x1)) (Math/abs (- y2 y1))))

(defn number->digits
  "Takes a number and returns its digits as a sequence"
  [num]
  (->> num str (map (comp read-string str))))

(defn gcd 
  "greatest common divisor (Euclid's algorithm)"
  [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(defn lcmu 
  "least-common-multiple"
  [a b]
  (/ (* (Math/abs ^long a) (Math/abs ^long b)) (gcd a b)))

; say you want the first item from each list, then second item from each list etc...
; (amv [[:a :b :c] [:d :e :f] [:g :h :i]])
; => ([:a :d :g] [:b :e :h] [:c :f :i])
(defn amv [xs]
  (apply map vector xs))

(defn insert-at [xs i n]
  (vec (concat (subvec xs 0 i) [n] (subvec xs i))))

(defn decimal->ratio [dec]
  (loop [n dec
         d 1
         i 1]
    (let [nxi (* n i)
          dxi (* d i)]
      (if (== (int (* n i)) (* n i))
        (/ (int nxi) (int dxi))
        (recur n d (* 10 i))))))

(defn factorial [n]
  (apply *' (range 1 (inc n))))


(defn reciprocal [n]
  (if (ratio? n)
    (/ (denominator n) (numerator n))
    (/ 1 n)))

(defn is-vowel [s]
  (not= nil (re-matches #"(?i)[aeiou]" s)))

(defn is-vowel-char [c]
  (not (nil? (#{\a \e \i \o \u \A \E \I \O \U} c))))

(defn int-pow [b ^long ex]
  (loop [acc 1 ex ex]
    (case ex
      0 acc
      (recur (* acc b) (dec ex)))))

(defn sum-of-digits [num]
  (reduce + (number->digits num)))

(defn swap [v i1 i2]
  (assoc v i2 (v i1) i1 (v i2)))

(defn remove-it-from-all-vals [m it]
  (into {}
        (map (fn [[k v]] [k (remove #(= it %) v)]))
        m))

(defn frest [s]
  (-> s
      (first)
      (rest)))

(defn sum-values [x]
  (->> x
       vals
       (reduce + 0)))

(defn trim-str-before-it [s it]
  (subs s (.indexOf s it)))


(defn str-split [re s]
  (str/split s re))

(defn drop-last-char [s]
  (->> (butlast s)
       (apply str)))

(defn range-inclusive [a b]
  (range a (inc b)))

(defn map-f-and-keep-true [f & xses]
  (->> (apply (partial map f) xses)
       (filter true?)))

(defn parse-list-of-ints [xs]
  (map #(Integer/parseInt %) xs))

(defn map-into [f i xs]
  (into i (map f xs)))
