(ns stuartstein777.2024.day7 
  (:require
    [clojure.string :as str]
    [clojure.math.combinatorics :as combo]))

(defn parse-line [line]
  (let [[total values] (str/split line #":")]
    {:total (parse-long total)
     :values (-> (str/trim values)
                 (str/split #" ")
                 (->> (mapv parse-long)))}))

(defn parse []
  (->> "puzzle-inputs/2024/day7"
       (slurp)
       (str/trim)
       (str/split-lines)
       (mapv parse-line)))

(defn comb [m n]
  (parse-long (str m n)))

(defn solve 
  "Takes an equestion of the form [1 \+ 2 \* 3 \+ 4] and returns 13"
  [eq]
  (:total (reduce (fn [{:keys [total op] :as acc} i]
                    (if (or (= i \*) (= i \+) (= i \c))
                      (assoc acc :op i)
                      (condp = op
                        \* (assoc acc :total (* total i))
                        \+ (assoc acc :total (+ total i))
                        \c (assoc acc :total (comb total i))
                        nil (assoc acc :total i))))
                  {:total 0.0 :op nil}
                  eq)))

(defn merge-lists [main-list interleaved-items]
  (let [pairs (interleave main-list interleaved-items)
        remaining (drop (count interleaved-items) main-list)]
    (concat pairs remaining)))

(defn equation-can-be-solved [operators equation]
  (let [perm-count (dec (count (:values equation)))
        operator-permutations (combo/selections operators perm-count)
        equations (map (partial merge-lists (:values equation)) operator-permutations)]
    (reduce (fn [_ eq]
              (if (= (solve eq) (:total equation))
                (reduced (:total equation))
                0)) 0 equations)))

(defn part-one []
  (let [input (parse)]
    (->> (map (partial equation-can-be-solved [\+ \*]) input)
         (reduce +))))

(defn part-two []
  (let [input (parse)]
    (->> (map (partial equation-can-be-solved [\+ \* \c]) input)
         (reduce +))))

(part-one) ; 6083020304036
(part-two) ; 59002246504791
