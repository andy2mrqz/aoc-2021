(ns advent-of-code.day03
  (:require [clojure.string :as s])
  (:gen-class))

(def input
  (->> (slurp "resources/day03.txt")
       (s/split-lines)))

(defn transpose [v] (apply mapv vector v))

(defn find-bits-by
  [gt-or-lt]
  (fn [bits-at-position]
    (as-> (frequencies bits-at-position) freqs
      (case (compare (freqs \0) (freqs \1))
        1  (if (= > gt-or-lt) \0 \1)
        0  (if (= > gt-or-lt) \1 \0)
        -1 (if (= > gt-or-lt) \1 \0)))))

(defn binary->decimal [binary-string] (Integer/parseInt binary-string 2))

(defn -part1
  [nums]
  (let [transposed (transpose nums)
        gamma-rate  (s/join (map (find-bits-by >) transposed))
        epsilon-rate (s/join (map (find-bits-by <) transposed))]
    (reduce * (map binary->decimal [gamma-rate epsilon-rate]))))

(-part1 input)
;; => 1307354

(defn find-rating
  [gt-or-lt]
  (fn [remaining pos]
    (let [most-or-least-common ((find-bits-by gt-or-lt) (nth (transpose remaining) pos))
          bits-starting-with (filter #(= most-or-least-common (nth % pos)) remaining)]
      (if (= 1 (count bits-starting-with))
        (reduced bits-starting-with)
        bits-starting-with))))

(defn -part2
  [nums]
  (let [[o2-rating] (reduce (find-rating >) nums (range (count (first nums))))
        [co2-rating] (reduce (find-rating <) nums (range (count (first nums))))]
    (reduce * (map binary->decimal [o2-rating co2-rating]))))

(-part2 input)
;; => 482500
