(ns stuartstein777.2020.day21
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [stuartstein777.utils :as u]
            [stuartstein777.file :as f]))

(defn parse [s]
  (let [[ingredients allergens] (str/split s #"(contains )")]
    {:allergens (->> (butlast allergens)
                     (apply str)
                     (u/str-split #",")
                     (map str/trim)
                     (into #{}))
     :ingredients (->> ingredients
                       (u/drop-last-char)
                       (u/str-split #" ")
                       (map str/trim)
                       (into #{}))}))

(defn foods-containing-allergen [allergen foods]
  (filter (fn [{:keys [allergens]}]
            (some #{allergen} allergens)) foods))


(defn isolated-ingredient? [allergen foods]
  (let [foods-with-allergen (foods-containing-allergen allergen foods)
        ingredients         (map :ingredients foods-with-allergen)
        intersections       (apply set/intersection ingredients)]
    (if (= 1 (count intersections))
      (first intersections)
      nil)))

(defn isolate-ingredient [allergens foods]
  (loop [allergens allergens]
    (let [allergen (first allergens)
          isolated-ingredient (isolated-ingredient? allergen foods)]
      (if isolated-ingredient
        [isolated-ingredient allergen]
        (recur (rest allergens))))))

(defn count-ingredients [foods]
  (->> foods
       (mapcat :ingredients)
       (count)))

(defn get-ingredients-string [isolated-allergens]
  (->> (sort-by second isolated-allergens)
       (map first)
       (str/join ",")))

(defn remove-allergen-and-ingredient [allergen ingredient food]
  {:allergens (disj (:allergens food) allergen)
   :ingredients (disj (:ingredients food) ingredient)})

(let [foods (->> (f/read-all-lines-and-parse "puzzle-inputs/2020/day21" parse))
      allergens (->> foods
                     (mapcat :allergens)
                     (into #{}))]
  
  (loop [foods                 foods
         outstanding-allergens allergens
         isolated-allergens []]
      (if (empty? outstanding-allergens)
        [(count-ingredients foods)
         (get-ingredients-string isolated-allergens)]
        (let [[isolated-ingredient isolated-allergen] (isolate-ingredient outstanding-allergens foods)]
            (recur
             (mapv (partial remove-allergen-and-ingredient isolated-allergen isolated-ingredient) foods)
             (disj outstanding-allergens isolated-allergen)
             (conj isolated-allergens [isolated-ingredient isolated-allergen]))))))
