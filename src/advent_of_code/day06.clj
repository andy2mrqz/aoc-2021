(ns advent-of-code.day06
  (:gen-class))

(def input
  (->> (slurp "resources/day06.txt")
       (re-seq #"\d")
       (mapv #(Integer/parseInt %))))

(defn simulate
  [fish]
  (let [births (->> (filter #(= -1 %) fish) (count))]
    (concat (replace {-1 6} fish) (repeat births 8))))

(defn -part1
  [fish simulation-days]
  (loop
   [day 1
    fish fish]
    (let [new-fish (simulate (map dec fish))]
      (if (>= day simulation-days)
        (count new-fish)
        (recur (inc day) new-fish)))))

(-part1 input 80)
;; => 352872
