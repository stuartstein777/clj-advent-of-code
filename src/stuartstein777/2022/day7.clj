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


(comment
  {["/"]         {:files #{["c.dat" 8504156] ["b.txt" 14848514]},
                  :dirs #{"d" "a"},
                  :sizes 23352670},
   ["/" "a"]     {:files #{["f" 29116] ["g" 2557] ["h.lst" 62596]},
                  :dirs #{"e"},
                  :sizes 94269},
   ["/" "a" "e"] {:files #{["i" 584]},
                  :dirs #{},
                  :sizes 584},
   ["/" "d"]     {:files #{["k" 7214296] ["j" 4060174] ["d.log" 8033020] ["d.ext" 5626152]},
                  :dirs #{},
                  :sizes 24933642}}


  (->>
   (filter (fn [[_ x]] (<= x 100000))
           [[["/"] (+ 23352670 94269 584 24933642)]
            [["/" "a"] (+ 94269 584)]
            [["/" "a" "e"] (+ 584)]
            [["/d"] (+ 24933642)]])
   (map second)
   (reduce +))

  (let [stack ["/" "a" "e"]]
    (pop stack)
    )
  )


;; iterate the keys, pull each directory out of the key,
;; update each previous directory.
;; pop each dir off the end, update all earlier stacks.

;; acc here is map of directory stack to directory size
;; i is directory stack and :size of that stack.



(update {} ["/" "a"] (fnil (partial + 50) 0))

(let [fs {["/"]         {:files #{["c.dat" 8504156] ["b.txt" 14848514]},
                         :dirs #{"d" "a"},
                         :sizes 23352670},
          ["/" "a"]     {:files #{["f" 29116] ["g" 2557] ["h.lst" 62596]},
                         :dirs #{"e"},
                         :sizes 94269},
          ["/" "a" "e"] {:files #{["i" 584]},
                         :dirs #{},
                         :sizes 584},
          ["/" "d"]     {:files #{["k" 7214296] ["j" 4060174] ["d.log" 8033020] ["d.ext" 5626152]},
                         :dirs #{},
                         :sizes 24933642}}]
  (let [dir-sizes (zipmap (keys fs) (repeat 0))]
    (reduce reducer dir-sizes fs)
    ))









