(ns stuartstein777.2018.day5
  (:require [clojure.string :as str]))

(def reactors ["aA" "Aa" "bB" "Bb" "Cc" "cC" "dD" "Dd" "eE" "Ee"
               "Ff" "fF" "gG" "Gg" "Hh" "hH" "iI" "Ii" "Jj" "jJ"
               "Kk" "kK" "Ll" "lL" "mM" "Mm" "nN" "Nn" "oO" "Oo" "Pp" "pP"
               "Qq" "qQ" "Rr" "rR" "Ss" "sS" "Tt" "tT" "uU" "Uu"
               "Vv" "vV" "wW" "Ww" "xX" "Xx" "Yy" "yY" "Zz" "zZ"])

(defn replacer [s m]
  (str/replace s m ""))

(defn react [s]
  (let [reacted (reduce (fn [acc m] (replacer acc m)) s reactors)]
    (if (= s reacted)
      reacted
      (recur reacted))))

;; part 1
(-> (slurp "puzzle-inputs/2018/day5")
    (react)
    (count))

;; part 2
(let [orig (slurp "puzzle-inputs/2018/day5")
      all (map (fn [s]
                (let [l (subs s 0 1)
                      u (subs s 1)]
                  (-> orig
                     (str/replace l "")
                     (str/replace u "")))) (take-nth 2 reactors))]
  (->> (map react all)
       (map count)
       (apply min)))