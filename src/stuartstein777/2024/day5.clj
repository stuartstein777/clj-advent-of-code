(ns stuartstein777.2024.day5
  (:require [clojure.string :as str]))

(defn parse-input []
  (let [[ordering-rules update-page-numbers]
        (-> "puzzle-inputs/2024/day5"
            (slurp)
            (str/split #"\n\n"))]
    {:ordering-rules (->> (str/split ordering-rules #"\n")
                          (mapv (fn [r] (->> (str/split r #"\|")
                                             (mapv parse-long)))))
     :update-page-numbers (->> (str/split update-page-numbers #"\n")
                               (mapv (fn [x] (->> (str/split x #",")
                                                  (mapv parse-long)
                                                  (partition 2 1)))))}))

(defn reducer [rules [before after]]
  (if (rules before)
    (update rules before conj after)
    (assoc rules before #{after})))

(defn build-ordering-rules-map [ordering-rules]
  (reduce reducer {} ordering-rules))

(defn get-middle [xs]
  (nth xs (/ (count xs) 2)))

(defn valid? [rules page-numbers]
  (reduce (fn [_ [before after]]
            (if (and (rules before) ((rules before) after))
              true
              (reduced false))) true page-numbers))

(defn part-one []
  (let [input (parse-input)
        page-numbers (:update-page-numbers input)
        rules (build-ordering-rules-map (:ordering-rules input))]
    (->> (filter (partial valid? rules) page-numbers)
         (mapv (fn [xs] (conj (mapv first xs) (last (last xs)))))
         (map get-middle)
         (reduce +))))

(part-one) ;; 5762

(defn insert-at [xs i n]
  (vec (concat (subvec xs 0 i) [n] (subvec xs i))))

;; for each number n
;;      cur-idx: 0
;;      x: res[cur-idx]
;;      check map for x
;;         does it contain n ?
;;             yes: at end of list?
;;                 yes: add n to end
;;                 no: increment cur-idx
;;             no: check map for n, does it contain x
;;                  yes: add n at cur-idx + 1
;;                  no: increment cur-idx
(defn repair-reducer [rules acc n]
  (prn acc n)
  (let [cnt (count acc)]
    (loop [idx 0]
      (if (= idx cnt)
        (conj acc n)
        (let [x (nth acc idx)]
          (if (and (rules x) ((rules x) n))
            (if (= idx cnt)
              (conj acc n)          ;; at end of list
              (recur (inc idx)))    ;; not at end of list, but doesnt go here.
            (if (and (rules n) ((rules n) x))
              (insert-at acc idx n) ;; found where it should be, insert it.
              (recur (inc idx))))))))) ;; not at end of list, but doesnt go here.

(defn repair [rules page-numbers]
  (reduce (partial repair-reducer rules) [(first page-numbers)] (rest page-numbers)))

(defn part-two []
  (let [input (parse-input)
        page-numbers (:update-page-numbers input)
        rules (build-ordering-rules-map (:ordering-rules input))]
    (->> (remove (partial valid? rules) page-numbers)
         (mapv (fn [xs] (conj (mapv first xs) (last (last xs)))))
         (mapv (partial repair rules))
         (map get-middle)
         (reduce +))))

(part-two) ;; 4130

