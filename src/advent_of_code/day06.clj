(ns advent-of-code.day06
  (:gen-class))

(def input
  (->> (slurp "resources/day06.txt")
       (re-seq #"\d")
       (mapv #(Integer/parseInt %))))

(comment ;; Initial approach was brute-force and memory consuming.
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
  )

;; Much better approach that doesn't construct a list in memory
(defn new-age [age] (if (zero? age) 6 (dec age)))
(defn safe-sum [old qty] (+ (or old 0) qty))

(defn breed
  [ages]
  (let [births (or (ages 0) 0)]
    (-> (reduce (fn [m [age qty]] (update m (new-age age) safe-sum qty)) {} ages)
        (update 8 safe-sum births))))

(defn -part2
  [fish days]
  (->> (nth (iterate breed fish) days)
       vals
       (reduce +)))

;; Can also be used for part 1
(-part2 (frequencies input) 80)
;; => 352872

(-part2 (frequencies input) 256)
;; => 1604361182149
