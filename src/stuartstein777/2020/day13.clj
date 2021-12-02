(ns stuarts.day13
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

(set! *unchecked-math* nil)

;; Part 1
(defn parse-real-input []
  (let [[dep buses] (->> (slurp "puzzle-inputs/2020/day13")
                         (str/split-lines))
        earliest-departure (Integer/parseInt dep)
        bus-ids (->> (str/split buses #",")
                     (remove #(= "x" %))
                     (map #(Integer/parseInt %)))]
    [earliest-departure bus-ids]))

(defn solve-part1 []
  (let [[earliest buses] (parse-real-input)]
    (loop [time earliest]
      (let [departures (->> (map (fn [n] {:id n :rem (mod time n) :time time}) buses)
                            (filter #(= 0 (:rem %))))]
        (if (seq departures)
          (let [{:keys [id rem time]} (first departures)]
            (* (- time earliest) id))
          (recur (inc time)))))))

(time (solve-part1))

;; Part 2
(defn next-match [start increment busid busno]
  (loop [cur (+ start increment)]
    (if (zero? (mod (+ cur busno) busid))
      cur
      (recur (+ cur increment)))))

(defn parse-part-2 []
  (->> (str/split (->> (slurp "puzzle-inputs/2020/day13")
                       (str/trim)
                       (str/split-lines)
                       (second)) #",")
       (zipmap (range))
       (remove (fn [x] (= (val x) "x")))
       (map (fn [[pos id]] {:pos pos :id (Integer/parseInt id)}))
       (sort-by :pos)))

(time
 (let [input (parse-part-2)]
   (->> (reduce (fn [{:keys [cur increment]} {:keys [pos id]}]
                  {:cur       (next-match cur increment id pos)
                   :increment (* increment id)})
                {:cur (:pos (first input)) :increment (:id (first input))}
                (rest input))
        (:cur))))
