(ns advent-of-code.day01
  (:require [clojure.string :as s])
  (:gen-class))

(def input
  (->> "resources/day01.txt"
       slurp
       (s/split-lines)
       (map #(Integer/parseInt %))))

(defn -part1 [nums]
  (loop [xs (rest nums) prev (first nums) count 0]
    (if (empty? xs)
      count
      (if (> (first xs) prev)
        (recur (rest xs) (first xs) (inc count))
        (recur (rest xs) (first xs) count)))))

(-part1 input)
