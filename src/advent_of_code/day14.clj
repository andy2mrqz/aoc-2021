(ns advent-of-code.day14
  (:require [clojure.string :as str]))

(def input
  (->> (slurp "resources/day14.txt")
       (str/split-lines)))

(def template (seq (first input)))
(def rules (->> (drop 2 input)
                (map (comp (juxt (comp seq first) (comp first second))
                           #(re-seq #"\w+" %)))
                (into {})))

(defn update-safe+ [m k x] (update m k (fn [a b] (+ (or a 0) b)) x))

(defn pair-insert [pair-freqs]
  (reduce (fn [pairs [pairA pairB :as pair]]
            (let [match (rules pair)]
              (-> (update-safe+ pairs (list pairA match) (pair-freqs pair))
                  (update-safe+ (list match pairB) (pair-freqs pair))))) {} (keys pair-freqs)))

(defn solve [steps]
  (let [pair-iterations (iterate pair-insert (frequencies (partition 2 1 template)))
        results (nth pair-iterations steps)
        char-freqs (reduce (fn [fqs [[a] occurrences]] (update-safe+ fqs a occurrences)) {(last template) 1} results)
        most-common (val (apply max-key val char-freqs))
        least-common (val (apply min-key val char-freqs))]
    (- most-common least-common)))

;; part 1
(solve 10)
;; => 2447

;; part 2
(solve 40)
;; => 3018019237563
