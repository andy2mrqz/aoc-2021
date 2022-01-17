(ns advent-of-code.day10
  (:require [clojure.string :as str]))

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

(defn -part1
  [chunks]
  (->> (mapv #(reduce find-corrupted '() %) chunks)
       (filter char?)
       (mapv score-table)
       (apply +)))

(-part1 input)
;; => 166191
