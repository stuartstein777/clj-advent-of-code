


(defn parse-monkey-no [monkey]
  (-> (re-seq #"(\d+)" monkey)
      ffirst
      parse-long))

(defn parse-starting-items [s]
  (->> (re-seq #"(\d+)" s)
       (mapv (comp parse-long first))))

(defn parse-operation [s d]
  (let [[op v] (->> (re-seq #"(\+|\*)\s(\d+|old)" s)
                    first
                    rest)]
    (fn [n]
      (let [op (condp = op
                 "+" +
                 "*" *)]
        (if (= "old" v)
          (mod (op n n) d)
          (mod (op (parse-long v) n) d))))))


(defn parse-test [test t f]
  (let [divisible-by (-> (re-find #"(\d+)" test)
                         first
                         parse-long)
        to-monkey-false (parse-monkey-no f)
        to-monkey-true (parse-monkey-no t)]
    [divisible-by (fn [n]
                    (if (zero? (mod n divisible-by))
                      to-monkey-true
                      to-monkey-false))]))

(defn parse-monkey [m]
  (let [[monkey items op test if-true if-false] (str/split m #"\n")
        [d fn] (parse-test test if-true if-false)]
    {:monkey (parse-monkey-no monkey)
     :items  (parse-starting-items items)
     :op (parse-operation op d)
     :test fn}))


(as-> (slurp "puzzle-inputs/day11") o
  (str/split o #"\nMonkey")
  (mapv parse-monkey o))
