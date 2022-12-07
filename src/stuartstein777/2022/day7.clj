(ns stuartstein777.2022.day7
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [clojure.core.match :refer [match]]))

(defn get-dir [cmd]
  (-> cmd
      (str/split #" ")
      (nth 2)))

(defn get-file-info [file]
  (let [[size name] (-> (str/split file #"\s"))]
    [name (parse-long size)]))

(defn get-dir-name [dir]
  (tap> dir)
  (second (str/split dir #" ")))

(defn process-listings [listings]
  (let [files-dirs (group-by #(str/starts-with? % "dir ") listings)
        dirs (get files-dirs true [])
        files (get files-dirs false [])]
    [(mapv get-dir-name dirs)
     (mapv get-file-info files)]))

(defn update-file-structure [[dirs files] dir fs]
    (assoc fs dir {:files (set files)
                   :dirs (set dirs)
                   :sizes (apply + (map second files))}))

(defn get-listings [rcmds]
  (->> rcmds
      (take-while #(not (str/starts-with? % "$")))))

(defn process-commands [stack file-structure [cmd & rcmds]]
  (if  (seq cmd)
    (cond
      (str/starts-with? cmd "$ cd ..")
      (recur (vec (butlast stack)) file-structure rcmds)

      (str/starts-with? cmd "$ cd /")
      (recur ["/"] file-structure rcmds)
      
      (str/starts-with? cmd "$ cd")
      (recur (conj stack (get-dir cmd))
             file-structure
             rcmds)
      
      (str/starts-with? cmd "$ ls")
      (let [listings (get-listings rcmds)
            files-dirs (process-listings listings)
            updated-file-structure
            (update-file-structure files-dirs stack file-structure)]
        (recur stack updated-file-structure (drop (count listings) rcmds)))
      
      :else
      (recur stack file-structure rcmds))
    file-structure))

(defn reducer [acc i]
  (let [k (key i)
        size (:sizes (val i))]
    (loop [s k
           acc acc]
      (if (seq s)
        (recur (pop s) (update acc s (fnil (partial + size) 0)))
        acc))))

(defn simplify-dir-sizes [fs]
  (let [dir-sizes (zipmap (keys fs) (repeat 0))]
    (reduce reducer dir-sizes fs)))

(defn get-dirs-under-100k [dirs]
  (->> (filter (fn [[_ x]] (<= x 100000)) dirs)
       (map second)
       (apply +)))

(->> (slurp "puzzle-inputs/2022/day7")
     (str/split-lines)
     (process-commands ["/"] {})
     (simplify-dir-sizes)
     (get-dirs-under-100k))

;; 1077191

;; part 2
;; Total space available 70000000
;; Need unused space of  30000000
;; 

(defn get-dirs-big-enough [to-free dirs]
  (filter (fn [[_ x]] (>= x to-free)) dirs))

(let [dir-sizes (->> (slurp "puzzle-inputs/2022/day7")
                     (str/split-lines)
                     (process-commands ["/"] {})
                     (simplify-dir-sizes))
      total-space    70000000
      required-space 30000000
      total-taken    (dir-sizes ["/"])
      available      (- total-space total-taken)
      to-free        (- required-space available)]
  (->> dir-sizes
       (get-dirs-big-enough to-free)
       (sort-by second <)
       (first)
       (second)))
