(ns stuartstein777.2019.day4)

;; part 1

(defn digits-are-ascending-left-to-right? [digits]
  (apply <= digits))

(defn has-adjacent-digits? [password]
  (not= nil (some #(>= (second %) 2) (frequencies password))))

(defn password-filter [password]
  (and (has-adjacent-digits? password) (digits-are-ascending-left-to-right? password)))

(count (filter password-filter (map #(for [n (str %)] (- (byte n) 48)) (range 197487 673252))))

;; part 2

(defn adjacent-digits-not-part-of-group? [password]
  (not= nil (some #(= 2 (second %)) (frequencies password))))

(defn password-filter2 [password]
  (and (adjacent-digits-not-part-of-group? password) (digits-are-ascending-left-to-right? password)))

(count (filter password-filter2 (map #(for [n (str %)] (- (byte n) 48)) (range 197487 673252))))