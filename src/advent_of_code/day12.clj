(ns advent-of-code.day12
  (:require [clojure.string :as str]))

(defn safe-conj-set [s i] (conj (or s #{}) i))

(defn adjacency-list [m [edge1 edge2]]
  (-> (update m edge1 safe-conj-set edge2)
      (update edge2 safe-conj-set edge1)))

(def adj-list
  (->> (slurp "resources/day12.txt")
       (str/split-lines)
       (mapv #(str/split % #"-"))
       (reduce adjacency-list {})))

(defn end? [cave] (= cave "end"))
(defn large-cave? [cave] (= cave (str/upper-case cave)))
(defn not-visited? [cave history] (not (contains? (set history) cave)))

(defn visitable [cave history]
  (filterv (fn [opt] (or (large-cave? opt) (not-visited? opt history))) (adj-list cave)))

(defn find-paths [cave history]
  (let [new-history (conj history cave)]
    (if (end? cave)
      [new-history]
      (mapcat #(find-paths % new-history) (visitable cave new-history)))))

;; part 1
(->> (find-paths "start" [])
     count)
;; => 3713
