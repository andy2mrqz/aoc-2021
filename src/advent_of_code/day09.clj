(ns advent-of-code.day09
  (:require [clojure.string :as str]))

(defn parse-ints [coll] (mapv #(Character/getNumericValue %) coll))

(defn transpose [c] (apply mapv vector c))

(def input
  (->> (slurp "resources/day09.txt")
       (str/split-lines)
       (mapv parse-ints)))

(defn maybe-lowest [m x y]
  (let [it (get-in m [x y])
        n (get-in m [(dec x) y] 9)
        s (get-in m [(inc x) y] 9)
        e (get-in m [x (inc y)] 9)
        w (get-in m [x (dec y)] 9)]
    (when (and (= it (min it n s e w)) (not= it n s e w)) (inc it))))

(defn -part1 [hm]
  (->> (for [r (-> hm count range)
             c (-> (first hm) count range)]
         (maybe-lowest hm r c))
       (remove nil?)
       (reduce +)))

(-part1 input)
;; => 550
