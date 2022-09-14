(ns stuartstein777.2021.day12b
  (:require [stuartstein777.file :as f]
            [stuartstein777.utils :as u]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn parser [line]
  (let [[start end] (str/split line #"-")]
    [[start end] [end start]]))

(defn merge-vector-of-maps [xs]
  (reduce (fn [acc [k v]] (update acc k conj v)) {} xs))

(defn find-continuation [continuations path-so-far allowed-twice]
  (let [possible-caves (continuations (peek path-so-far))]
    (remove (fn [cave] 
              (cond (= cave allowed-twice)
                    (as-> path-so-far o
                        (filter (fn [c] (= (str c) allowed-twice)) o)
                        (count o)
                        (>= o 2))
                    
                    :else (and (= (str/lower-case cave) cave)
                               (not= (.indexOf path-so-far cave) -1)))) possible-caves)))

(defn next-paths [continuations [path allowed-twice]]
  (let [next-caves (find-continuation continuations path allowed-twice)]
    (if (empty? next-caves)
      []
      (mapv (fn [nc] [(conj path nc) allowed-twice]) next-caves))))

(defn path->str [[path _ allowed]]
  (str allowed ": " (str/join "->" path)))

(defn print-paths [paths]
  (->> (mapv path->str paths)
      (prn)))

(time
 (let [continuations (-> (->> (f/read-all-lines-and-parse "puzzle-inputs/2021/day12" parser)
                              (apply concat)
                              (merge-vector-of-maps))
                         (u/remove-it-from-all-vals "start"))

       lower-case-caves (->> continuations
                             (keys)
                             (filter #(= (str/lower-case %) %))
                             (filter (fn [s] (and (not= "start" s)
                                                  (not= "end" s)))))

       starts (map (fn [x] [["start"] x]) lower-case-caves)]
   (loop [possible-paths starts
          finished       []]
     (if (empty? possible-paths)
       (count (set (map (fn [x] (nth x 0)) finished)))
       (let [cur-path     (nth possible-paths 0)
             new-paths    (set (next-paths continuations cur-path))
             new-finished (set (filter (fn [[path _ _]] (= "end" (peek path))) new-paths))
             unfinished   (set/difference new-paths new-finished)]
         (if (empty? new-paths)
            ;; got no new paths for this path, so just drop it and recur (it's a dead end.) 
           (recur (rest possible-paths)
                  finished)
            ;; otherwise, drop it and add on all the new unfinished paths and add the finished paths to finished.
           (recur (apply conj (rest possible-paths) unfinished)
                  (apply conj finished new-finished))))))))

;; ~7.2 seconds but it works!