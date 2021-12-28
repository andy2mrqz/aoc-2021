(ns advent-of-code.day01
  (:require [clojure.string :as s])
  (:gen-class))

(def input
  (->> "resources/day01.txt"
       slurp
       (s/split-lines)
       (map #(Integer/parseInt %))))

(defn -part1
  "Solution I came up with myself!"
  [nums]
  (loop [xs (rest nums) prev (first nums) count 0]
    (if (empty? xs)
      count
      (if (> (first xs) prev)
        (recur (rest xs) (first xs) (inc count))
        (recur (rest xs) (first xs) count)))))

(-part1 input)
;; 1451

(defn -part2
  "Inspired by the cleaner solution to part 1, but I did it all myself!"
  [nums]
  (->> (partition 2 1 (partition 3 1 nums))
       (filter (fn [[x y]] (> (reduce + y) (reduce + x))))
       count))

(-part2 input)

;; (comment
;;   (defn -part1
;;     "Solution I found after that I like:
;;      https://gist.github.com/Solaxun/a84b956dc885657dc23e15f826495133"
;;     [nums]
;;     (->> (partition 2 1 nums)
;;          (filter (fn [[x y]] (> y x)))
;;          count))
;;   (-part1 input))
