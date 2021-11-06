(ns stuarts.day12
  (:require [clojure.set :as set]
            [clojure.string :as str]))

(def test-input "F10\nN3\nF7\nR90\nF11")
(def test-input2 "E5\n;R90\n;F2\n;W10\n;L180\n;N7\n;F4\n;R270\n;S3\n;L90\n;R180\n;W8\n;L270\n;F5")
(def test-input3 "E5\nR90\nF2\nW10\nL180\nN7\nF4\nR270\nS3\nL90\nR180\nW8\nL270\nF5")

;; shared =========================================================================================================

(defn parse-line [line]
  (let [[inst v] (rest (re-find #"(\w)(\d+)" line))]
    [(keyword inst) (Integer/parseInt v)]))

(defn parse-input [input]
  (->> input
       (str/split-lines)
       (map parse-line)))

;; part 1 =========================================================================================================

;; East = 0, South = 1, West = 2, North = 3
(defn turn [facing turning degrees]
  (let [turning-amount (if (= turning :L)
                         (- (/ degrees 90))
                            (/ degrees 90))]
    (mod (+ facing turning-amount) 4)))

(defn forward [facing x y distance]
  (case facing
        0 [(+ x distance) y]                                ; east
        1 [x (- y distance)]                                ; south
        2 [(- x distance) y]                                ; west
        3 [x (+ y distance)]))                              ; north

(defn move [{:keys [facing location] :as acc} [inst distance]]
  (let [[cur-x cur-y] location]
    (case inst
      :W (assoc acc :location [(- cur-x distance) cur-y])
      :E (assoc acc :location [(+ cur-x distance) cur-y])
      :N (assoc acc :location [cur-x (+ cur-y distance)])
      :S (assoc acc :location [cur-x (- cur-y distance)])
      :R (assoc acc :facing (turn facing inst distance))
      :L (assoc acc :facing (turn facing inst distance))
      :F (assoc acc :location (forward facing cur-x cur-y distance)))))

(->> (reduce move {:facing 0 :location [0 0]} (parse-input (slurp "puzzle-inputs/2020/day12")))
     (:location)
     (map #(Math/abs %))
     (reduce +))
;=> 1482

;; part 2 =========================================================================================================

(defn rotate-waypoint [[wx wy] inst ang]
  (cond (or (and (= inst :R) (= ang 90))
           (and (= inst :L) (= ang 270))) [wy (- wx)]
        (or (and (= inst :R) (= ang 270))
           (and (= inst :L) (= ang 90))) [(- wy) wx]
        (= ang 180) [(- wx) (- wy)]
        (= ang 360) [wx wy]))

(defn forward2 [[sx sy] [wx wy] distance]
  [(+ sx (* wx distance)) (+ sy (* wy distance))])

(defn move2 [{:keys [ship waypoint] :as acc} [inst d]]
  (let [[wx wy] waypoint]
    (case inst
      :N (assoc acc :waypoint [wx (+ wy d)])
      :S (assoc acc :waypoint [wx (- wy d)])
      :E (assoc acc :waypoint [(+ wx d) wy])
      :W (assoc acc :waypoint [(- wx d) wy])
      :L (assoc acc :waypoint (rotate-waypoint waypoint inst d))
      :R (assoc acc :waypoint (rotate-waypoint waypoint inst d))
      :F (assoc acc :ship (forward2 ship waypoint d)))))

(->> (reduce move2 {:ship [0 0] :waypoint [10 1]} (parse-input (slurp "puzzle-inputs/2020/day12")))
     (:ship)
     (map #(Math/abs ^int %))
     (reduce +))
