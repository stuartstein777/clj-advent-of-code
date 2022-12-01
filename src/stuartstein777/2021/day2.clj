(ns stuartstein777.2021.day2
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [stuartstein777.file :as f]
            [stuartstein777.utils :as u]))

(def puzzle-input "puzzle-inputs/2021/day2")

;; Advent of Code 2021 - Day 2 - Part 1

(defn parse [line]
  (let [[dir vel] (str/split line #" ")]
    {:dir dir
     :vel (Integer/parseInt vel)}))

(defn move [[x y] {:keys [dir vel]}]
  (condp = dir
    "forward" [(+ x vel) y]
    "down"    [x (+ y vel)]
    "up"      [x (- y vel)]))

(->> puzzle-input
     slurp
     str/split-lines
     (map parse)
     (reduce move [0 0])
     (reduce * 1))

;; Answer for part 1: 1893605

;; Part 2


(defn move2 [[x y aim] {:keys [dir vel]}]
  (condp = dir
    "forward" [(+ x vel) (+ y (* aim vel)) aim]
    "down"    [x y (+ aim vel)]
    "up"      [x y (- aim vel)]))

(->> puzzle-input
     slurp
     str/split-lines
     (map parse)
     (reduce move2 [0 0 0])
     (take 2)
     (reduce * 1))

;; Answer for part 2: 2120734350
