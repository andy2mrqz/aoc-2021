(ns advent-of-code.day05
  (:require [clojure.string :as s])
  (:gen-class))

(defn parse-nums [nums] (mapv #(Integer/parseInt %) nums))

(def coordinates
  (->> (slurp "resources/day05.txt")
       (s/split-lines)
       (mapv #((comp parse-nums re-seq) #"\d+" %))))

(defn x-or-y-match? [[x0 y0 x1 y1]] (or (= x0 x1) (= y0 y1)))

(defn get-range [a b] (range (min a b) ((comp inc max) a b)))

(defn into-lines [[x0 y0 x1 y1]]
  (if (= x0 x1)
    (for [y (get-range y0 y1)] [x0 y])
    (for [x (get-range x0 x1)] [x y0])))

(defn into-lines-pt-2 [[x0 y0 x1 y1 :as input]]
  (if (x-or-y-match? input)
    (into-lines input)
    (let [x-range (if (> x1 x0) (range x0 (inc x1)) (range x0 (dec x1) -1))
          y-range (if (> y1 y0) (range y0 (inc y1)) (range y0 (dec y1) -1))]
      (map vector x-range y-range))))

(defn determine-overlaps [coll] (->> (frequencies coll) (filterv #(>= (val %) 2))))

(defn solve [into-line-fn input] ((comp count determine-overlaps mapcat) into-line-fn input))

(defn -part1 [input] (->> (filterv x-or-y-match? input) (solve into-lines)))
(defn -part2 [input] (solve into-lines-pt-2 input))

(-part1 coordinates)
;; => 5147

(-part2 coordinates)
;; => 16925
