(ns advent-of-code.day07
  (:gen-class))

(def input
  (->> (slurp "resources/day07.txt")
       (re-seq #"\d+")
       (mapv #(Integer/parseInt %))))

(defn distances
  [x positions]
  (->> (mapv #(Math/abs (- x %)) positions)
       (reduce +)))

(defn -part1 [positions]
  (let [s (apply min positions)
        e (inc (apply max positions))]
    (->> (into {} (for [x (range s e)] {x (distances x positions)}))
         (apply min-key val)
         val)))

(-part1 input)
;; => 340987
