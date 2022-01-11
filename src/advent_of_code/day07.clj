(ns advent-of-code.day07
  (:gen-class))

(def input
  (->> (slurp "resources/day07.txt")
       (re-seq #"\d+")
       (mapv #(Integer/parseInt %))))

(defn distances
  [x positions rate-fn]
  (->> (map #(Math/abs (- x %)) positions)
       (map rate-fn)
       (reduce +)))

(defn solve [positions rate-fn]
  (let [s (apply min positions)
        e (inc (apply max positions))]
    (->> (into {} (for [x (range s e)] {x (distances x positions rate-fn)}))
         (apply min-key val)
         val)))

;; Part 1
(solve input identity)
;; => 340987

;; 1 + 2 + 3 + ... + n
(defn triangle [n] (reduce + (range 1 (inc n))))

;; Part 2
(solve input triangle)
