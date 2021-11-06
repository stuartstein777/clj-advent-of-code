(ns stuartstein777.2015.day1)

(defn parse-aunt [aunt-line]
  (let [num-idx (.indexOf aunt-line ":")
        num (Integer/parseInt (subs aunt-line 4 num-idx))
        stats (subs aunt-line (+ 2 num-idx))]
    (->> (str/split stats #", ")
         (map #(str/split % #": "))
         (map (fn [[k v]] {(keyword k) (Integer/parseInt v)}))
         (into {:num num}))))

(defn matches [f v k a]
  (or (nil? (k a)) (f v (k a))))

;; part 1

(let [all-aunts (->> (slurp  "puzzle-inputs/2015/day16")
                     (str/split-lines)
                     (mapv parse-aunt))]  
  (-> (filter (every-pred
                (partial matches = 3 :children)
                (partial matches = 7 :cats)
                (partial matches = 2 :samoyeds)
                (partial matches = 3 :pomeranians)
                (partial matches = 0 :akitas)
                (partial matches = 0 :vizslas)
                (partial matches = 5 :goldfish)
                (partial matches = 3 :trees)
                (partial matches = 2 :cars)
                (partial matches = 1 :perfumes))
               all-aunts)
      first
      :num))

;; part 2

(let [all-aunts (->> (slurp  "puzzle-inputs/2015/day16")
                     (str/split-lines)
                     (mapv parse-aunt))]  
  (-> (filter (every-pred
                (partial matches = 3 :children)
                (partial matches < 7 :cats)
                (partial matches = 2 :samoyeds)
                (partial matches > 3 :pomeranians)
                (partial matches = 0 :akitas)
                (partial matches = 0 :vizslas)
                (partial matches > 5 :goldfish)
                (partial matches < 3 :trees)
                (partial matches = 2 :cars)
                (partial matches = 1 :perfumes))
               all-aunts)
      first
      :num))
