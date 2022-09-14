(ns stuartstein777.2021.day16
  (:require [clojure.string :as str]))

(def hex-map
  {\0 "0000"
   \1 "0001"
   \2 "0010"
   \3 "0011"
   \4 "0100"
   \5 "0101"
   \6 "0110"
   \7 "0111"
   \8 "1000"
   \9 "1001"
   \A "1010"
   \B "1011"
   \C "1100"
   \D "1101"
   \E "1110"
   \F "1111"})

(defn decode-hex [hex]
  (->> hex
       (map hex-map)
       (apply str)))

(defn get-packet-version [decoded]
  (Integer/parseInt (subs decoded 0 3) 2))

(defn get-packet-type-id [decoded]
  (Integer/parseInt (subs decoded 3 6) 2))

(defn get-literal-message [decoded]
  (subs decoded 6))

(defn get-message-length [message]
  (->> message
       (partition 5)
       (map (partial apply str))
       (take-while #(str/starts-with? % "1"))
       count
       inc
       (* 5)))

(defn get-message-value [decoded]
  (let [message        (get-literal-message decoded)
        message-length (get-message-length message)]
    (Integer/parseInt (->> (subs decoded 6 (+ 6 message-length))
                           (partition 5)
                           (mapcat (partial drop 1))
                           (apply str)) 2)))

(let [hex-string     "D2FE28"
      decoded        (decode-hex hex-string) 
      packet-version (get-packet-version decoded)
      packet-type-id (get-packet-type-id decoded)]
      (when (= packet-type-id 4)
        (get-message-value decoded)))

(let [hex "38006F45291200"
      decoded (decode-hex hex)]
  
  )
