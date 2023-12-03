(ns stuartstein777.2023.day1
  (:require [clojure.string :as str]))

(defn parse-turn [turn]
  (->> (str/split (str/triml turn) #", ")
       (map (fn [t] (str/split t #" ")))
       (map (fn [[count color ]]
              {color (Integer/parseInt count)}))))

(defn foo [x y]
  (max x y))

(defn parse-line [idx input]
  {:id (inc idx)
  :game (->>
            (str/split input #":")
            (rest)
            (mapcat (fn [i] (str/split i #";")))
            (map str/triml)
            (mapcat parse-turn)
            (apply merge-with foo))})

(defn parse-input [input]
  (->> input
       (slurp)
       (str/split-lines)
       (map-indexed parse-line)))

(defn possible-game? [{:keys [game]}]
  (tap> game)
  (let [max-red 12
        max-green 13
        max-blue 14]
    (and (<= (game "red") max-red)
         (<= (game "green") max-green)
         (<= (game "blue") max-blue))))
  
(->> "puzzle-inputs/2023/day2"
     parse-input
     #_(filter possible-game?)
     #_(map :id)
     #_(reduce +))

;; 2204

;; part 2

(defn parse-line [idx input]
  {:id (inc idx)
   :game (->>
          (str/split input #":")
          (rest)
          (mapcat (fn [i] (str/split i #";")))
          (map str/triml)
          (mapcat parse-turn)
          (apply merge-with foo))})

(defn game-power [{:keys [game]}]
  (->> game
       (vals)
       (reduce * 1)))

(->> "puzzle-inputs/2023/day2"
     parse-input
     (map game-power)
     (reduce +))
