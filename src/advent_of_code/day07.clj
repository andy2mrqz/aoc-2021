(ns advent-of-code.day07
  (:gen-class))

(def input
  (->> (slurp "resources/day07.txt")
       (re-seq #"\d+")
       (mapv #(Integer/parseInt %))))

(defn distances
  [positions rate-fn x]
  (->> (map (comp rate-fn #(Math/abs (- x %))) positions)
       (reduce +)))

(defn solve [positions rate-fn]
  (let [[s e] (apply (juxt min max) positions)]
    (->> (range s (inc e))
         (map (partial distances positions rate-fn))
         (apply min))))

;; Part 1
(solve input identity)
;; => 340987

;; 1 + 2 + 3 + ... + n => n(n + 1) / 2
(defn triangle [n] (/ (* n (inc n)) 2))

;; Part 2
(solve input triangle)
;; => 96987874
