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

;; takes an equation in the form [1 + 2 * 3 + 4 * 5]
(defn solve [eq]
  (:total (reduce (fn [{:keys [total op] :as acc} i]
                    (if (or (= i *) (= i +))
                      (assoc acc :op i)
                      (if (= * op)
                        (assoc acc :total (* total i))
                        (assoc acc :total (+ total i)))))
                  {:total 0 :op nil}
                  eq)))

(defn merge-lists [main-list interleaved-items]
  (let [pairs (interleave main-list interleaved-items)
        remaining (drop (count interleaved-items) main-list)]
    (concat pairs remaining)))

(defn generate-permutations [items length]
  (->> (repeat length items)
       (apply combo/cartesian-product)))

(defn equation-can-be-solved [equation]
  (let [operators [+ * ]
        perm-count (dec (count (:values equation)))
        operator-permutations (generate-permutations operators perm-count)
        equations (map (partial merge-lists (:values equation)) operator-permutations)]
    (reduce (fn [_ eq]
              (if (= (solve eq) (:total equation))
                (reduced (:total equation))
                0)) 0 equations)))

(comment
  (equation-can-be-solved {:total 18
                           :values [4 5 3]})
  )



(defn part-one []
  (let [input (parse)]
    (->> (map equation-can-be-solved input)
         (reduce +))))

(part-one) ;; 6083020304036

