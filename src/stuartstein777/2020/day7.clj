(ns stuartstein777.2020.day7
  (:require [clojure.string :as str]
            [loom.graph :as lg]
            [loom.derived :as ld]))

(def demo-input "light red bags contain 1 bright white bag, 2 muted yellow bags.\ndark orange bags contain 3 bright white bags, 4 muted yellow bags.\nbright white bags contain 1 shiny gold bag.\nmuted yellow bags contain 2 shiny gold bags, 9 faded blue bags.\nshiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.\ndark olive bags contain 3 faded blue bags, 4 dotted black bags.\nvibrant plum bags contain 5 faded blue bags, 6 dotted black bags.\nfaded blue bags contain no other bags.\ndotted black bags contain no other bags.")

(defn parse-bag [bag]
  (when (not= bag "no other bags")
    (let [reg (first (re-seq #"(\d+) (\w+ \w+) bag" bag))]
      [(nth reg 2) (Integer/parseInt (nth reg 1))])))

(defn parse-line [line]
  (let [[parent & rest] (str/split line #"contain|,")
       node (second (re-find #"(\w+ \w+) bags" (str/trim parent)))
       contained-bags (if (= "no other bags." (str/trim (first rest)))
                        {}
                        (->> (map str/trim rest)
                             (map parse-bag)
                             (reduce conj {})))]
      {node contained-bags}))

(parse-line "dotted black bags contain no other bags.")

(defn parse-input []
  (->>
    #_(slurp "puzzle-inputs/2020/day7")
    demo-input
       (str/split-lines)
       (map parse-line)))

;; part 1
(defn reachable-from-shiny-gold []
  (-> (apply lg/weighted-digraph (parse-input))
      (lg/transpose)
      (ld/subgraph-reachable-from "shiny gold")
      (lg/nodes)
      (count)
      (dec)))

(reachable-from-shiny-gold)

;; part 2
(defn count-weights-from-shiny-gold [graph]
  (lg/weight graph "shiny gold" "dark olive"))



(let [graph (apply lg/weighted-digraph (parse-input))]
  (count-weights-from-shiny-gold graph)
  ;(lg/successors graph "shiny gold")
  )


(count-weights-from-shiny-gold (apply lg/weighted-digraph (parse-input)))

(defn nested-count [g node]
  (reduce + 1 (map (fn [x] (prn x) (* (lg/weight g node x) (nested-count g x))) (lg/successors g node))))

(let [graph (apply lg/weighted-digraph (parse-input))]
  (dec (nested-count graph "shiny gold")))