(ns advent-of-code.day10
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def input
  (->> (slurp "resources/day10.txt")
       (str/split-lines)
       (mapv seq)))

(def pairs
  {\) \(
   \] \[
   \} \{
   \> \<})

(def score-table
  {\) 3
   \] 57
   \} 1197
   \> 25137})

(defn find-corrupted
  [stack char]
  (if-let [open (pairs char)]
    (if (= open (peek stack))
      (pop stack)
      (reduced char))
    (conj stack char)))

(defn corrupted [chunks] (mapv #(reduce find-corrupted '() %) chunks))

(defn -part1 [chunks]
  (->> (filter char? (corrupted chunks))
       (mapv score-table)
       (apply +)))

(-part1 input)
;; => 166191

(def score-table2
  {\) 1
   \] 2
   \} 3
   \> 4})

(defn score [res completion]
  (-> (* 5 res)
      (+ (score-table2 completion))))

(defn final-score [scores]
  (scores (-> (count scores)
              (quot 2))))

(defn -part2 [chunks]
  (->>
   (filter list? (corrupted chunks))
   (mapv #(mapv (set/map-invert pairs) %))
   (mapv #(apply str %))
   (mapv #(reduce score 0 %))
   sort
   vec
   final-score))

(-part2 input)
;; => 1152088313
