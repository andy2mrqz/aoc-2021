(ns advent-of-code.day13
  (:require [clojure.string :as str]))

(defn get-coords [l] (->> (re-seq #"\d+" l)
                          (mapv #(Integer/parseInt %))))
(defn get-folds [l] (first (re-seq #"([xy])=(\d+)" l)))
(defn third->int [coll] (Integer/parseInt (nth coll 2)))

(def input
  (->> (slurp "resources/day13.txt")
       (str/split-lines)))

(def coords (->> (take-while (complement str/blank?) input)
                 (mapv get-coords)))
(def folds  (->> (drop-while #(not (str/starts-with? % "fold")) input)
                 (mapv (comp (juxt second third->int) get-folds))))

(defn apply-folds [sheet [dim center-idx]]
  (set (for [point sheet
             :let [coord-idx (if (= "x" dim) 0 1)]]
         (if (< (get point coord-idx) center-idx)
           point
           (update point coord-idx #(- (* 2 center-idx) %))))))

;; part 1
(->> (apply-folds coords (first folds))
     count)
;; => 765

;; part 2
(let [diagram (reduce apply-folds coords folds)
      max-x (apply max (map first diagram))
      max-y (apply max (map second diagram))]
  (run! println
        (for [y (range (inc max-y))]
          (apply str (for [x (range (inc max-x))]
                       (if (contains? diagram [x y]) \█ \space))))))
