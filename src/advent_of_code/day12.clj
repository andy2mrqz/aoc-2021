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

(defn large-cave? [cave] (= cave (str/upper-case cave)))
(defn not-visited? [small-cave history] (not (contains? (set history) small-cave)))

(defn visitable [cave history nvf]
  (filter (fn [opt] (or (large-cave? opt) (nvf opt history))) (adj-list cave)))

(defn find-paths [cave history nvf]
  (let [new-history (conj history cave)]
    (if (= cave "end")
      [new-history]
      (mapcat #(find-paths % new-history nvf) (visitable cave new-history nvf)))))

;; part 1
(->> (find-paths "start" [] not-visited?)
     count)
;; => 3713

(defn small-cave-history [history] (filter (complement large-cave?) history))

(defn duplicate-exists? [history]
  (contains? (->> (small-cave-history history) frequencies vals set) 2))

(defn not-visited-allowing-one-duplicate? [cave history]
  (if (duplicate-exists? history)
    (not-visited? cave history)
    (not= "start" cave)))

;; part 2
(->> (find-paths "start" [] not-visited-allowing-one-duplicate?)
     count)
;; => 91292
