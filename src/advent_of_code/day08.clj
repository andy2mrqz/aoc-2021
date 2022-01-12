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

;; A messy approach to part 2 using a series of deductions
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

;; Idea for this drastically more elegant solution borrowed from here:
;; https://github.com/abyala/advent-2021-clojure/blob/main/src/advent_2021_clojure/day08_joepittsy.clj
;; I guess it was also inspired from a guy named Joe Pittsy but it wasn't on his github
;;
;; As soon as I understood the idea for the solution I implemented it myself to try and still
;; grasp it.  I really like the idea of finding an array of unique keys given known inputs - one
;; of those times you wonder why you don't think like this all the time XD

(defn shared [a b] (count (set/intersection (set a) (set b))))

(def digit-keys
  ;; [len digits-in-1 digits-in-4] digit
  {[6 2 3] 0
   [2 2 2] 1
   [5 1 2] 2
   [5 2 3] 3
   [4 2 4] 4
   [5 1 3] 5
   [6 1 3] 6
   [3 2 2] 7
   [7 2 4] 8
   [6 2 4] 9})

(defn better-pt2 [{:keys [signals outputs]}]
  (let [one  (take-first is1? signals)
        four (take-first is4? signals)]
    (->> (mapv (juxt count (partial shared one) (partial shared four)) outputs)
         (mapv digit-keys)
         (apply str))))

(->> (mapv better-pt2 input)
     (mapv #(Integer/parseInt %))
     (reduce +))
;; => 1027422
