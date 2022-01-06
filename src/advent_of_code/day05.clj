(ns advent-of-code.day05
  (:require [clojure.string :as s])
  (:gen-class))

(defn parse-nums [nums] (mapv #(Integer/parseInt %) nums))

(def coordinates
  (->> (slurp "resources/day05.txt")
       (s/split-lines)
       (mapv #((comp parse-nums rest first re-seq) #"(\d+),(\d+) -> (\d+),(\d+)" %))))

(defn x-or-y-match? [[x0 y0 x1 y1]] (or (= x0 x1) (= y0 y1)))

(defn get-range [a b] (range (min a b) ((comp inc max) a b)))

(defn into-lines [[x0 y0 x1 y1]]
  (if (= x0 x1)
    (for [y (get-range y0 y1)] [x0 y])
    (for [x (get-range x0 x1)] [x y0])))

(defn determine-overlaps [coll]
  (->> (group-by identity coll)
       (filterv #(>= ((comp count val) %) 2))))

(defn -part1 [input]
  (->> input
       (filterv x-or-y-match?)
       ((comp count determine-overlaps mapcat) into-lines)))

(-part1 coordinates)
;; => 5147
