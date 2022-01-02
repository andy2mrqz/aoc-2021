(ns advent-of-code.day03
  (:require [clojure.string :as s])
  (:gen-class))

(def input
  (->> (slurp "resources/day03.txt")
       (s/split-lines)))

(defn transpose [v] (apply mapv vector v))

(defn find-bits-by
  [find-min-or-max-by]
  (fn [bits-at-position]
    (->> (frequencies bits-at-position)
         (apply find-min-or-max-by val)
         (key))))

(defn flip-bits [binary-string] (s/join (map #(if (= \0 %) "1" "0") binary-string)))

(defn binary->decimal [binary-string] (Integer/parseInt binary-string 2))

(defn -part1
  [binary-numbers]
  (let [transposed (transpose binary-numbers)
        gamma-rate  (s/join (map (find-bits-by max-key) transposed))
        epsilon-rate (flip-bits gamma-rate)
        gamma-int (binary->decimal gamma-rate)
        epsilon-int (binary->decimal epsilon-rate)]
    (* gamma-int epsilon-int)))

(-part1 input)
;; => 1307354
