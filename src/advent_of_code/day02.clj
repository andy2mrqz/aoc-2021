(ns advent-of-code.day02
  (:require [clojure.string :as s])
  (:gen-class))

(def input
  (->> (slurp "resources/day02.txt")
       (s/split-lines)
       (map #(let [[a b] (s/split % #" ")]
               [(keyword a) (Integer/parseInt b)]))))

(defn solve
  [directions navigator]
  (->> (reduce navigator {:x 0 :y 0 :aim 0} directions)
       ((fn [{:keys [x y]}] (* x y)))))

(defn navigate-part1
  [state [direction units]]
  (direction {:forward (update state :x + units)
              :down (update state :y + units)
              :up (update state :y - units)}))

(defn -part1 [directions] (solve directions navigate-part1))

(-part1 input)
;; => 1660158

(defn navigate-part2
  [{aim :aim :as state} [direction units]]
  (direction {:forward (-> (update state :x + units)
                           (update :y + (* aim units)))
              :down (update state :aim + units)
              :up (update state :aim - units)}))

(defn -part2 [directions] (solve directions navigate-part2))

(-part2 input)
;; => 1604592846
