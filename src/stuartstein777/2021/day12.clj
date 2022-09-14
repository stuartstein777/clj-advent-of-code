(ns stuartstein777.2021.day12
  (:require [stuartstein777.file :as f]
            [stuartstein777.utils :as u]
            [clojure.string :as str]))

(defn parser [line]
  (let [[start end] (str/split line #"-")]
    [[start end] [end start]]))

;; input  -> [{"start" "A"} {"start" "b"} {"A" "c"} {"A" "d"}]
;; output -> {"start" ["A" "b"], "A" ["c" "d"]}
(defn merge-vector-of-maps [xs]
  (reduce (fn [acc [k v]] (update acc k conj v)) {} xs))

;; path-so-far -> {:path ["start" "A" "b"] :finished false}
;; given a map of continuations it gets the continatuations for the last item in the path-so-far path.
;; it excludes those that are lower-case and have already appeared in (:path path-so-far)
;;
;; Continuations looks like this:
;; {"start" '("b" "A")
;;   "A" '("end" "b" "c" "start")
;;   "b" '("A" "end" "d" "start")
;;   "c" '("A")
;;   "d" '("b")
;;   "end" '("b" "A")} 
(defn find-continuation [continuations path-so-far]
  (let [possible-caves (continuations (last (:path path-so-far)))]
    (remove (fn [cave] (and (= (str/lower-case cave) cave)
                            (not= (.indexOf (:path path-so-far) cave) -1))) possible-caves)))

;; Given a path-so-far, it gets all possible continuations and returns a
;; list of the path-so-far with continuations.
(defn next-path [continuations path-so-far]
  (let [next-caves (find-continuation continuations path-so-far)]
    (if (empty? next-caves)
      [(assoc path-so-far :finished true)]
      (mapv (fn [nc] (update path-so-far :path conj nc)) next-caves))))

;; Marks the :finished key in a path as true if the last item in the path is "end"
(defn mark-finished [paths]
  (mapv (fn [{:keys [path] :as cur-path}]
          (if (= (last path) "end")
            (assoc cur-path :finished true)
            cur-path)) paths))

;; Finds all the paths for the given map of continuations.
(defn find-paths [paths continuations]
  (let [{:keys [path] :as first-unfinished} (nth (filter (fn [{:keys [finished]}] (false? finished)) paths) 0)]
    (if (nil? first-unfinished)
      paths
      (recur (->> (remove (fn [p] (= path (:path p))) paths)
                  (concat (next-path continuations first-unfinished))
                  (mark-finished))
             continuations))))

;; Needed because their is a bug where dead-ends end up as two vectors intead of maps.
(defn count-paths [paths]
  (->> (mapv :path paths)
       (remove (fn [path] (not= "end" (last path))))
       (count)))

(time
 (->> (f/read-all-lines-and-parse "puzzle-inputs/2021/day12" parser)
      (apply concat)
      (merge-vector-of-maps)
      (find-paths [{:path ["start"] :finished false}])
      (count-paths)))
; test    226
; test2    10
; actual 4411



;; part 2

(defn valid-cave [cave allowed-twice path-so-far]
  (let [count-of-cave-so-far (-> (frequencies path-so-far)
                                 (get cave 0))]
    (if (= (str/lower-case cave) cave)
      (cond (= allowed-twice cave) (< count-of-cave-so-far 2)     ; its the cave that is allowed twice.
            :else (zero? count-of-cave-so-far))                   ; its not allowed twice, but it hasn't appeard yet

      true)))


;; path-so-far -> {:path ["start" "A" "b"] :finished false}
;; given a map of continuations it gets the continatuations for the last item in the path-so-far path.
;; it excludes those that are lower-case and have already appeared in (:path path-so-far)
;;
;; Continuations looks like this:
;; {"start" '("b" "A")
;;   "A" '("end" "b" "c" "start")
;;   "b" '("A" "end" "d" "start")
;;   "c" '("A")
;;   "d" '("b")
;;   "end" '("b" "A")} 
(defn find-continuation [continuations path-so-far]
  (let [possible-caves (continuations (last (:path path-so-far)))
        allowable-twice (:allowed-twice path-so-far)]
    (filter (fn [cave]
              (valid-cave cave allowable-twice (:path path-so-far)))
            possible-caves)))



;; Finds all the paths for the given map of continuations.
(defn find-paths [paths continuations finished-paths]
  (prn (count paths))
  (let [{:keys [path allowed-twice] :as first-unfinished} (nth (filter (fn [{:keys [finished]}] (false? finished)) paths) 0)]
    (if (nil? first-unfinished)
      paths
      (let [finished ()])
      (recur (->> (remove (fn [p] (and (= path (:path p))
                                       (= allowed-twice (:allowed-twice p)))) paths)
                  (concat (next-path continuations first-unfinished))
                  (mark-finished))
             continuations))))

(let [continuations    (-> (->> (f/read-all-lines-and-parse "puzzle-inputs/2021/day12" parser)
                                (apply concat)
                                (merge-vector-of-maps))
                           (u/remove-it-from-all-vals "start"))

      lower-case-caves (->> continuations
                            (keys)
                            (filter #(= (str/lower-case %) %))
                            (filter (fn [s] (and (not= "start" s)
                                                 (not= "end" s)))))

      starts           (mapv (fn [x] {:path          ["start"]
                                      :finished      false
                                      :allowed-twice x}) lower-case-caves)]
  (->> (find-paths starts continuations)
       (map :path)
       (distinct)
       (filter (fn [path] (= "end" (last path))))
       (count)))

; can keep track of the set of available start->end paths, and remove them as we go.
; will this make it quicker ?
; if a path is finished, move it to finished set.
; maybe less items to run through to find next unfinished.
; instead of maps, use vectors and positional destructuring.
; if can only store paths, then we dont need (first (filter ....)
; 
;; reattempt at part 2

(defn finished? [paths]
  (mapv (fn [{:keys [path] :as cur-path}]
          (if (= (last path) "end")
            (assoc cur-path :finished true)
            cur-path)) paths))

(defn find-paths [paths continuations]
  (let [{:keys [path allowed-twice] :as first-unfinished} (nth (filter (fn [{:keys [finished]}] (false? finished)) paths) 0)]
    (if (nil? first-unfinished)
      paths
      (recur (->> (remove (fn [p] (and (= path (:path p))
                                       (= allowed-twice (:allowed-twice p)))) paths)
                  (concat (next-path continuations first-unfinished))
                  (mark-finished))
             continuations))))

(let [continuations    (-> (->> (f/read-all-lines-and-parse "puzzle-inputs/2021/day12" parser)
                                (apply concat)
                                (merge-vector-of-maps))
                           (u/remove-it-from-all-vals "start"))

      lower-case-caves (->> continuations
                            (keys)
                            (filter #(= (str/lower-case %) %))
                            (filter (fn [s] (and (not= "start" s)
                                                 (not= "end" s)))))

      starts           (mapv (fn [x] {:path          ["start"]
                                      :finished      false
                                      :allowed-twice x}) lower-case-caves)]
  
  )

