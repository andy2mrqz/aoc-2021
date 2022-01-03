(ns advent-of-code.day03
  (:require [clojure.string :as s])
  (:gen-class))

(def input
  (->> (slurp "resources/day03.txt")
       (s/split-lines)))

(defn transpose [v] (apply mapv vector v))

(defn find-bits-by
  [op bits-at-position]
  (as-> (frequencies bits-at-position) freqs
    (case (compare (freqs \0) (freqs \1))
      1  (if (= op >) \0 \1)
      0  (if (= op >) \1 \0)
      -1 (if (= op >) \1 \0))))

(defn binary->decimal [binary-string] (Integer/parseInt binary-string 2))

(defn -part1
  [nums]
  (let [transposed (transpose nums)
        rate-fn (fn [op] ((comp binary->decimal s/join) (map (partial find-bits-by op) transposed)))
        [gamma-rate epsilon-rate] (map rate-fn [> <])]
    (* gamma-rate epsilon-rate)))

(-part1 input)
;; => 1307354

(defn find-rating
  [op remaining pos]
  (let [most-or-least-common ((partial find-bits-by op) (nth (transpose remaining) pos))
        bits-starting-with (filter #(= most-or-least-common (nth % pos)) remaining)]
    (if (= 1 (count bits-starting-with))
      (reduced bits-starting-with)
      bits-starting-with)))

(defn -part2
  [nums]
  (let [rating-fn (fn [op] (reduce (partial find-rating op) nums (range (count (first nums)))))
        [[o2-rating] [co2-rating]] (map rating-fn [> <])]
    (reduce * (map binary->decimal [o2-rating co2-rating]))))

(-part2 input)
;; => 482500
