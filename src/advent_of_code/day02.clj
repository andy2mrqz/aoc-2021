(ns advent-of-code.day02
  (:require [clojure.string :as s])
  (:gen-class))

(def input
  (->> "resources/day02.txt"
       slurp
       (s/split-lines)
       (map #(s/split % #" "))))

(defn navigate-part1
  [state [direction units]]
  (let [units (Integer/parseInt units)]
    (condp = direction
      "forward" (update state :x + units)
      "down" (update state :y + units)
      "up" (update state :y - units))))

(defn -part1
  [directions]
  (as-> directions $
    (reduce navigate-part1 {:x 0 :y 0} $)
    (* (:x $) (:y $))))

(-part1 input)
;; => 1660158

(defn navigate-part2
  [state [direction units]]
  (let [units (Integer/parseInt units)]
    (condp = direction
      "forward" (as-> state $
                  (update $ :x + units)
                  (update $ :y + (* (:aim $) units)))
      "down" (update state :aim + units)
      "up" (update state :aim - units))))

(defn -part2
  [directions]
  (as-> directions $
    (reduce navigate-part2 {:x 0 :y 0 :aim 0} $)
    (* (:x $) (:y $))))

(-part2 input)
;; => 1604592846
