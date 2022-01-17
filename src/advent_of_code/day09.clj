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
    (when (and (= it (min it n s e w)) (not= it n s e w)) it)))

(defn low-points [hm]
  (->> (for [r (-> hm count range)
             c (-> (first hm) count range)]
         [(maybe-lowest hm r c) r c])
       (remove #(-> % first nil?))))

(defn -part1 [hm] (reduce #(+ %1 (-> %2 first inc)) 0 (low-points hm)))

(-part1 input)
;; => 550

(defn valid? [hm visited k]
  (false? (or (contains? visited k)
              (when-let [val (get-in hm k)] (= val 9)))))

(defn dfs [visited hm [x y :as key]]
  (if (valid? hm visited key)
    (-> (conj visited key)
        (dfs hm [(dec x) y])  ;; north
        (dfs hm [(inc x) y])  ;; south
        (dfs hm [x (inc y)])  ;; east
        (dfs hm [x (dec y)])) ;; west
    visited))

(defn -part2 [hm]
  (->> (low-points hm)
       (mapv (fn [[_v r c]] (dfs #{} hm [r c])))
       (mapv count)
       (sort >)
       (take 3)
       (apply *)))

(-part2 input)
;; => 1100682
