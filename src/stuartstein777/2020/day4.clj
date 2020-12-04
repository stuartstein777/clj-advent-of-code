(ns stuartstein777.2020.day4
  (:require [clojure.string :as str]))

;; -------------------------- part 1
(defn valid? [passport]
  (let [required-fields [:hgt :byr :eyr :ecl :iyr :pid :hcl]]
    (every? #(passport %) required-fields)))

;; -------------------------- part 2
(defn valid-year [min max byr]
  (and byr (re-matches #"\d+" byr) (<= min (Integer/parseInt byr) max)))

(defn valid-height [height]
  (and height
       (re-matches #"(\d+)cm|(\d+)in" height)
       (let [h (Integer/parseInt (subs height 0 (- (count height) 2)))
             unit (subs height (- (count height) 2))]
         (if (= unit "in")
           (<= 59 h 76)
           (<= 150 h 193)))))

(defn valid? [{:keys [hgt byr eyr ecl iyr pid hcl]}]
  (and ((partial valid-year 1920 2002) byr)
       ((partial valid-year 2010 2020) iyr)
       ((partial valid-year 2020 2030) eyr)
       (valid-height hgt)
       hcl (re-matches #"#([0-9]|[a-f]){6}" hcl)
       (#{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"} ecl)
       pid (re-matches #"[0-9]{9}" pid)))

;; ---------------------- shared
(defn parse [entry]
  (->> (str/split entry #"\r\n|\s")
       (map (fn [e]
              (let [[k v] (str/split e #":")]
                {(keyword k) v})))
       (apply merge)))

(as-> (slurp "puzzle-inputs/2020/day4") o
      (str/split o #"[\r\n]{2}")
      (map parse o)
      (filter valid? o)
      (count o))