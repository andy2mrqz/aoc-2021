(ns advent-of-code.day08
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn sort-str [s] ((comp str/join sort) s))

(def input
  (->> (slurp "resources/day08.txt")
       (str/split-lines)
       (mapv #(re-seq #"\w+" %))
       (mapv (fn [l] {:signals (mapv sort-str (take 10 l))
                      :outputs (mapv sort-str (drop 10 l))}))))

(defn is1? [i] (= 2 (count i)))
(defn is4? [i] (= 4 (count i)))
(defn is7? [i] (= 3 (count i)))
(defn is8? [i] (= 7 (count i)))

(defn -part1
  [entries]
  (->> (mapv :outputs entries)
       flatten
       (filter (fn [i] (or (is1? i) (is4? i) (is7? i) (is8? i))))
       count))

(-part1 input)
;; => 397

(defn take-first [pred coll] (first (filter pred coll)))

(defn sort-length [coll] (sort-by count coll))

(defn string-diff [a b]
  (let [[smaller larger] (sort-length [a b])]
    (-> (set/difference (set larger) (set smaller))
        str/join)))

(defn str-has-all? [s part]
  (every? #(str/includes? s (str %)) part))

(defn deduction [entry]
  (let [signals (sort-length (:signals entry))
        n1 (take-first is1? signals)
        n4 (take-first is4? signals)
        n7 (take-first is7? signals)
        n8 (take-first is8? signals)

        len5 (filter #(= 5 (count %)) signals)
        is3? (fn [s] (str-has-all? s n1))
        n3 (take-first is3? len5)
        ls2 (string-diff n1 n4)
        is5? (fn [s] (str-has-all? s ls2))
        n5 (take-first is5? len5)
        n2 (first (set/difference (set len5) (set [n3 n5])))

        len6 (filter #(= 6 (count %)) signals)
        is0? (fn [s] (not (str-has-all? s ls2)))
        n0 (take-first is0? len6)
        leftlen6 (remove #(= n0 %) len6)
        is9? (fn [s] (str-has-all? s n1))
        n9 (take-first is9? leftlen6)
        n6 (first (set/difference (set len6) (set [n0 n9])))
        m {n0 0 n1 1 n2 2
           n3 3 n4 4 n5 5
           n6 6 n7 7 n8 8 n9 9}
        outputs (:outputs entry)]
    (->> (map (fn [i] (m i)) outputs)
         str/join)))
(defn -part2 [entries]
  (->> (mapv deduction entries)
       (mapv #(Integer/parseInt %))
       (reduce +)))

(-part2 input)
;; => 1027422
