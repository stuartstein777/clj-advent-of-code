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
    (assoc fs dir {:files (set files) :dirs (set dirs)}))

(defn get-listings [rcmds]
  (->> rcmds
      (take-while #(not (str/starts-with? % "$")))))

(defn process-commands [stack file-structure [cmd & rcmds]]
  (prn stack)
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
            (update-file-structure files-dirs (last stack) file-structure)]
        (recur stack updated-file-structure (drop (count listings) rcmds)))
      
      :else
      (recur stack file-structure rcmds))
    file-structure))

(->> (slurp "puzzle-inputs/2022/day7-test")
     (str/split-lines)
     (process-commands ["/"] {}))

;; outer dirctory = /


;; keys are directories
{"/" {:dirs #{"a" "d"} :files #{{:name "b.txt" :size 14848514}
                            {:name "c.dat" :size 8504156}}}
 
 "a" {:dirs #{"e"} :files #{{:name "f" :size 29116}
                            {:name "g" :size 2557}
                            {:name "h.lst" :size 62596}}}
 
 "d" {:dirs #{} :files #{{:name "j" :size 4060174}
                       {:name "d.log" :size 8033020}
                       {:name "d.ext" :size 5626152}
                       {:name "k" :size 721496}}}
 
 "e" {:dirs #{} :files #{{:name "i" :size 584}}}
 }



