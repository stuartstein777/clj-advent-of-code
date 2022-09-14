(ns stuartstein777.2021.day14
  (:require [clojure.string :as str]))

(defn parser [line]
  {(subs line 0 2) [(str (subs line 0 1) (subs line 6))
                    (str (subs line 6) (subs line 1 2))]})

(defn solve [a b m]
  (->
   (reduce-kv (fn [acc [k1 k2] v]
                (as-> acc o

                  (merge-with + o {k1 v})
                  (merge-with + o {k2 v}))) {} m)
   (update a inc)
   (update b inc)))

(defn get-keys [m]
  (mapcat (fn [[k v]] (repeat v k)) m))

(def rnd (atom 0))

(defn react [reactions m]
  (let [ks (get-keys m)]
    (prn @rnd)
    (swap! rnd inc)
    (->> (mapcat reactions ks)
         (frequencies))))

(comment
  (let [reactions {"AB" ["ZZ" "ZA"]
                   "BC" ["YX" "XY"]
                   "DD" ["FF" "GG"]}
        m {"AB" 2
           "BC" 500
           "DD" 285}]

    (reduce (fn [acc [k v]]
              (let [[r1 r2] (reactions k)]
                (prn r1 r2)
                (-> acc
                    (update r1 (fnil + 0) v)
                    (update r2 (fnil + 0) v)))) {} m)))



(time
 (let [input            (->> (slurp "puzzle-inputs/2021/day14")
                             (str/split-lines))
       polymer-template (first input)
       reactions        (->> input
                             (drop 2)
                             (map parser)
                             (apply merge))
       reactors         (->> (partial react reactions)
                             (repeat 40)
                             (apply comp))
       final-pairs      (->> (partition 2 1 polymer-template)
                             (map (partial apply str))
                             (frequencies)
                             (reactors))
       final-vals       (->> final-pairs
                             (solve (first polymer-template) (last polymer-template))
                             (vals)
                             (map #(/ % 2)))
       max-c            (apply max final-vals)
       min-c            (apply min final-vals)]
   (- max-c min-c)
   #_(->> final-pairs
          (sort-by first)
          (remove (fn [[_ b]] (zero? b))))

   #_final-pairs))

; 3048

(comment
  (let [reactions {"AB" ["ZZ" "ZA"]
                   "BC" ["YX" "XY"]
                   "DD" ["FF" "GG"]}
        m {"AB" 2
           "BC" 500
           "DD" 285}]
    
    (reduce (fn [acc [k v]] 
              (let [[r1 r2] (reactions k)]
                (prn r1 r2)
                (-> acc
                    (update r1 (fnil + 0) v)
                    (update r2 (fnil + 0) v)))
              ) {} m)
    )  
  
  )