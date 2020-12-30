(ns stuarts.2020day16
  (:require [clojure.string :as str]
            [clojure.set :as set]))

;; part 1
(def test-input "class: 1-3 or 5-7\nrow: 6-11 or 33-44\nseat: 13-40 or 45-50\n\nyour ticket:\n7,1,14\n\nnearby tickets:\n7,3,47\n40,4,50\n55,2,20\n38,6,12")

(defn read-input []
  (->> (slurp "resources/2020/day16")
       (str/split-lines)))

(defn build-range [r]
  (let [[s e] (->> (str/split r #"-")
                   (map #(Integer/parseInt %)))]
    [s e]))

(defn valid-ticket-entry? [ranges n]
  (not-any? (fn [[a b]]
              (<= a n b)) ranges))

(defn get-ticket-fields [fields]
  (->> (str/split fields #",")
       (map #(Integer/parseInt %))))

(defn valid-ticket? [valid-ranges ticket]
  (not-any? (partial valid-ticket-entry? valid-ranges) ticket))

(defn get-valid-tickets []
  (let [input (read-input)
        ranges (->> input
                    (take 20)
                    (map #(re-find #"(\w+): (\d+-\d+) or (\d+-\d+)" %))
                    (mapcat #(drop 2 %))
                    (map build-range))
        tickets (->> input
                     (drop 25)
                     (mapcat get-ticket-fields))
        invalid-fields (filter (partial valid-ticket-entry? ranges) tickets)]
    (prn invalid-fields)
    (reduce + invalid-fields)))

(get-valid-tickets)

;; part 2
;; invalid fields
;; (989 3 18 0 975 990 997 18 4 989 990 17 976 988 21 984 19 8 1 979 982 13 13 21 11 13 3 18 991 975 985 20 15
;; 19 996 989 15 982 994 995 16 16 20 994 20 999)

(def part2-test-input "class: 0-1 or 4-19\nrow: 0-5 or 8-19\nseat: 0-13 or 16-19\n\nyour ticket:\n11,12,13\n\nnearby tickets:\n3,9,18\n15,1,5\n5,14,9")
(def test-rule-count 3)
(def test-pre-tickets 8)

(def rule-count 20)
(def pre-tickets 25)

(defn get-range [r]
  (let [[s e] (->> (str/split r #"-")
                   (map #(Integer/parseInt %)))]
    [s e]))

(defn build-range2 [[desc r1 r2]]
  [desc (get-range r1) (get-range r2)])

(defn satisfies-rule? [[name [r1min r1max] [r2min r2max]] [ticket-pos ticket-items]]
  (every? (fn [i]
            (or (<= r1min i r1max)
                (<= r2min i r2max))) ticket-items))

(defn get-satisfied-rules [rules ticket-items]
    (for [rule rules
          ticket ticket-items]
      (if (satisfies-rule? rule ticket)
        [(first ticket) (first rule)])))

(defn update-values [m f & args]
  (set/map-invert (reduce (fn [r [k v]] (assoc r k (apply f v args))) {} (set/map-invert m))))

(update-values {["row" "class"] 1, ["seat" "row" "class"] 2, ["row"] 0}
               (fn [i r] (remove #(= % r) i))
               "row")

:input (set/map-invert {["row" "class"] 1, ["seat" "row" "class"] 2, ["row"] 0}) {}
:ouput {0 "row", 1 "class", 2 "seat"}
(defn reduce-rules-and-positions [satisfied-rules result]
  (let [values (keys satisfied-rules)
        sole (ffirst (filter #(= 1 (count %)) values))
        updated (update-values satisfied-rules (fn [i r] (remove #(= % r) i))  sole)]
    (if sole
      (recur updated (assoc result (satisfied-rules [sole]) sole))
      result)))

(reduce-rules-and-positions (set/map-invert {1 ["row" "class"], 2 ["seat" "row" "class"], 0 ["row"]}) {})

(defn part2 []
  (let [input (read-input)
        ;input  (->> part2-test-input (str/split-lines))
        ranges (->> input
                    (take rule-count)
                    (map #(re-find #"(.+): (\d+-\d+) or (\d+-\d+)" %))
                    (mapcat #(drop 2 %))
                    (map build-range))
        tickets (->> input
                     (drop pre-tickets)
                     (map get-ticket-fields))
        rules (->> input
                   (take rule-count)
                   (map #(re-find #"(.+): (\d+-\d+) or (\d+-\d+)" %))
                   (map #(drop 1 %))
                   (map build-range2))
        valid-tickets (filter (partial valid-ticket? ranges) tickets)
        transposed-tickets (zipmap (range) (apply map vector valid-tickets))
        satisfied-rules (->> (reduce (fn [acc [i rule]] (update acc i (fnil conj []) rule))
                                     {}
                                     (->> (get-satisfied-rules rules transposed-tickets)
                                          (remove nil?)))
                             (set/map-invert))]
    (-> (reduce-rules-and-positions satisfied-rules {})
        (sort))))

(part2)

(* 67 113 61 139 83 59)
