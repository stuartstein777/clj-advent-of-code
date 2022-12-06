(ns stuartstein777.2015.day7
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.core.match :refer [match]]
            [clojure.pprint :refer (cl-format)]))

(defn bit-not [n]
  (-> (cl-format nil "~16,'0',B" n)
      (str/replace #"0|1" {"0" "1" "1" "0"})
      (Integer/parseInt 2)))

(defn parse-number [n]
  (if (re-matches #"(\d+)" n)
    (Integer/parseInt n)
    n))

(defn parse-instruction [instr]
  (match [instr]
    [[_ "AND" _ _]]    {:command :and, :params [(parse-number (first instr)) (parse-number (nth instr 2))], :dest (last instr)}
    [[_ "OR" _ _]]     {:command :or, :params [(parse-number (first instr)) (parse-number (nth instr 2))], :dest (last instr)}
    [[_ "LSHIFT" _ _]] {:command :lshift, :params [(parse-number (first instr)) (parse-number (nth instr 2))], :dest (last instr)}
    [[_ "RSHIFT" _ _]] {:command :rshift, :params [(parse-number (first instr)) (parse-number (nth instr 2))], :dest (last instr)}
    [["NOT" _ _]]      {:command :not, :params [(parse-number (second instr))], :dest (last instr)}
    [[_ _]]            {:command :set, :params [(parse-number (first instr))], :dest (last instr)}
    :else (do (prn "error:" instr) :Blach)))

(defn parse-line [line]
  (->> (re-seq #"((\w+)\s(\w+)\s(\w+)\s->\s(\w+))|((\w+)\s->\s(\w+))|((\w+)\s(\w+)\s->\s(\w+))|(\w+)\s->\s(\w+)" line)
       (first)
       (remove nil?)
       (drop 2)
       (vec)
       (parse-instruction)))


(comment
  (parse-line "0 -> c") 
  (parse-line "iu RSHIFT 1 -> jn")
  )


(defn in-mem-or-num? [mem x]
  (or (number? x)
      (and (string? x) (mem x))))

(defn can-process? [mem params]
  (every? (partial in-mem-or-num? mem) params))

(defn get-from-mem [mem p]
  (if (number? p)
    p
    (mem p)))

(def memo-bit-not (memoize bit-not))
(def memo-bit-and (memoize bit-and))
(def memo-bit-or (memoize bit-or))
(def memo-bit-shift-left (memoize bit-shift-left))
(def memo-bit-shift-right (memoize bit-shift-right))
(def memo-can-process? (memoize can-process?))
(def memo-set (memoize set))

(defn set [mem param dest]
  (assoc mem dest (get-from-mem mem param)))

(defn i-and [mem [x y] dest]
  (assoc mem dest (memo-bit-and (get-from-mem mem x) (get-from-mem mem y))))

(defn i-or [mem [x y] dest]
  (assoc mem dest (memo-bit-or (get-from-mem mem x) (get-from-mem mem y))))

(defn i-not [mem [x] dest]
  (assoc mem dest (memo-bit-not (get-from-mem mem x))))

(defn i-lshift [mem [x y] dest]
  (assoc mem dest (memo-bit-shift-left (get-from-mem mem x) y)))

(defn i-rshift [mem [x y] dest]
  (assoc mem dest (memo-bit-shift-right (get-from-mem mem x) y)))

;; need to check we have all values, so if the param isn't a number then
;; that location has to exist in mem
(defn process-instruction [mem {:keys [command params dest]}]
  (condp = command
    :set (memo-set mem (first params) dest) 
    :and (i-and mem params dest)
    :or (i-or mem params dest)
    :not (i-not mem params dest)
    :lshift (i-lshift mem params dest)
    :rshift (i-rshift mem params dest)))

(defn process-instructions [mem [instr & rest]]
  (if instr
    (if (memo-can-process? mem (:params instr))
      (let [new-mem (process-instruction mem instr)]
        (recur new-mem rest))
      (recur mem (conj (vec rest) instr)))
    mem))

;; part 1
(defn run-part-1 []
  (->> (slurp "puzzle-inputs/2015/day7")
       (str/split-lines)
       (mapv parse-line)
       (vec)
       (process-instructions {"b" 956})))
 
(get (run-part-1) "a")

;; part 2
(defn run-part-2 []
  (->>  (str/replace (slurp "puzzle-inputs/2015/day7")
                     "14146 -> b"
                     "956 -> b")
   
   (str/split-lines)
   (mapv parse-line)
   (vec)
   (process-instructions {})))

(get (run-part-1) "a")
  

