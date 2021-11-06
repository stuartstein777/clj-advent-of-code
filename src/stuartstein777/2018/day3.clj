(ns stuartstein777.2018.day3
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-line [line]
  (let [[n _ coords size] (str/split line #" ")
        num               (Integer/parseInt (subs n 1))
        [x y]             (-> (subs coords 0 (dec (count coords)))
                              (str/split #",")
                              (->> (mapv #(Integer/parseInt %))))
        [width height]   (->> (str/split size #"x")
                               (mapv #(Integer/parseInt %)))]
    {:num num
     :x x
     :y y
     :w width
     :h height}))

;; x:4 , y:6 , height:3 , width:3
;;
;;     1   2   3   4   5   6   7   8   9  10 
;; 1   .   .   .   .   .   .   .   .   .   .
;; 2   .   .   .   .   .   .   .   .   .   .
;; 3   .   .   .   .   .   .   .   .   .   .
;; 4   .   .   .   .   .   .   .   .   .   .
;; 5   .   .   .   .   .   .   .   .   .   .
;; 6   .   .   .   #   #   #   .   .   .   .
;; 7   .   .   .   #   #   #   .   .   .   .
;; 8   .   .   .   #   #   #   .   .   .   .
;; 9   .   .   .   .   .   .   .   .   .   .
;; 10  .   .   .   .   .   .   .   .   .   .
;;
;; (4, 6) (5, 6), (6, 6)
;; (4, 7) (5, 7), (6, 7)
;; (4, 8) (5, 8), (6, 8)

(defn get-squares [{:keys [num x y w h]}]
  (for [xs (range x (+ x w))
        ys (range y (+ y h))]
    {:n num
     :coords [xs ys]}))

(defn build-sparse-map [squares]
  (reduce (fn [acc  {:keys [n coords]}]
            (update acc coords (fnil conj []) n)) {} squares))

(defn make-fabric []
  (->> (slurp "puzzle-inputs/2018/day3")
       (str/split-lines)
       (mapcat (comp get-squares parse-line))
       build-sparse-map))

;; part 1
;;========================================
(->> (make-fabric)
     vals
     (filter #(> (count %) 1))
     count)

;; part 2
;;========================================
(defn reducer [acc i]
  (if (> (count i) 1)
    (apply conj acc i)
    acc))

(defn find-unique [vals]
 (let [all (set (apply concat vals))]
   (first (set/difference all (reduce reducer #{} vals)))))

(->> (make-fabric)
     vals
     find-unique)
