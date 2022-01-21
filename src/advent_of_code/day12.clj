(ns advent-of-code.day12
  (:require [clojure.string :as str]))

(defn safe-conj-set [s i] (conj (or s #{}) i))

(defn adjacency-list [m [edge1 edge2]]
  (-> (update m edge1 safe-conj-set edge2)
      (update edge2 safe-conj-set edge1)))

(def adj-list
  (->> (slurp "resources/day12.sample2")
       (str/split-lines)
       (mapv #(str/split % #"-"))
       (reduce adjacency-list {})))

(defn end? [cave] (= cave "end"))
(defn large-cave? [cave] (= cave (str/upper-case cave)))
(defn not-visited? [small-cave history] (not (contains? (set history) small-cave)))

(defn visitable [cave history nvf]
  (filter (fn [opt] (or (large-cave? opt) (nvf opt history))) (adj-list cave)))

(defn find-paths [cave history nvf]
  (let [new-history (conj history cave)]
    (if (end? cave)
      [new-history]
      (mapcat #(find-paths % new-history nvf) (visitable cave new-history nvf)))))

;; part 1
(->> (find-paths "start" [] not-visited?)
     count)
;; => 3713

(defn at-most-one-duplicate? [history]
  (let [freqs (frequencies (filter (complement large-cave?) history))
        count-freqs (frequencies (vals freqs))
        duplicates (or (count-freqs 2) 0)]
    (< duplicates 2)))

(defn not-visited-allowing-one-duplicate? [small-cave history]
  (and (not= "start" small-cave)
       (at-most-one-duplicate? history)
       (< (or ((frequencies history) small-cave) 0) 2)))

;; part 2
(->> (find-paths "start" [] not-visited-allowing-one-duplicate?)
     count)
;; => 91292
