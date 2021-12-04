(ns stuartstein777.2021.day4
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [stuartstein777.file :as f]
            [stuartstein777.utils :as u]))

(defn print-board [board]
  (println (str/join "\n" (->> board
                               (partition 5)
                               (map (fn [x]
                                      (->> (map (fn [{:keys [called n]}]
                                                  (if called "**" (format "%02d" n))) x))))
                               (map #(str/join " " %)))))
  (println))

(defn row-matched? [board]
  (->> board
       (map :called)
       (partition 5)
       (map #(every? true? %))
       (some #{true})))

(defn column-matched? [board]
  (->> board
       (map :called)
       (partition 5)
       (apply map vector)
       (map #(every? true? %))
       (some #{true})))

(defn mark-number-called [number boards]
  (map (fn [board]
         (map (fn [{:keys [n called]}]
                (if (= n number)
                  {:n n :called true}
                  {:n n :called called})) board)) boards))

(defn winning-board? [boards]
  (if (empty? boards)
    nil
    (let [board (first boards)]
      (print-board board)
      (if (or (row-matched? board) (column-matched? board))
        board
        (recur (rest boards))))))  

(defn score-board [board number]
  (prn board)
  (let [not-called (reduce + (map :n (filter #(false? (:called %)) board)))]
    (* number not-called)))

(defn play [boards numbers]
  (when (seq numbers)
    (println (first numbers))
    (println "--------------------------")
    (let [next-num (first numbers)
          updated-boards (mark-number-called next-num boards)
          winning-board (winning-board? updated-boards)]
      (if winning-board
        (score-board winning-board next-num)
        (recur updated-boards (rest numbers))))))

; part 1`
(let [input   (-> (slurp "puzzle-inputs/2021/day4")
                  (str/replace "  " " ")
                  (str/split-lines))
      numbers (->> input
                   first
                   (u/str-split #",")
                   (map #(Integer/parseInt %)))
      boards  (->> input
                   (drop 1)
                   (mapcat #(re-seq #"(\d+)" %))
                   (remove nil?)
                   (mapv first)
                   (mapv #(Integer/parseInt %))
                   (partition 25)
                   (map #(map (fn [b] {:n b :called false}) %)))]
  (play boards numbers)
  
  )

(comment
  (column-matched?
   '({:n 22, :called false} {:n 13, :called false} {:n 17, :called true} {:n 11, :called false} {:n 0, :called true}
                            {:n 8, :called false} {:n 2, :called false} {:n 23, :called true} {:n 4, :called false} {:n 24, :called true}
                            {:n 21, :called false} {:n 9, :called false} {:n 14, :called true} {:n 16, :called false} {:n 7, :called true}
                            {:n 6, :called false} {:n 10, :called false} {:n 3, :called true} {:n 18, :called false} {:n 5, :called true}
                            {:n 1, :called false} {:n 12, :called false} {:n 20, :called false} {:n 15, :called false} {:n 19, :called true}))
  )


