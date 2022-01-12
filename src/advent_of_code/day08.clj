(ns advent-of-code.day08
  (:require [clojure.string :as s]))

(def input
  (->> (slurp "resources/day08.txt")
       (s/split-lines)
       (mapv #(re-seq #"\w+" %))
       (mapv (fn [l] {:signals (take 10 l) :outputs (drop 10 l)}))))

(defn is1 [i] (= 2 (count i)))
(defn is4 [i] (= 4 (count i)))
(defn is7 [i] (= 3 (count i)))
(defn is8 [i] (= 7 (count i)))

(defn -part1
  [entries]
  (->> (mapv :outputs entries)
       flatten
       (filter (fn [i] (or (is1 i) (is4 i) (is7 i) (is8 i))))
       count))

(-part1 input)
;; => 397
