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

(defn safe+ [a b] (+ (or a 0) b))
(defn update-ab-safe+ [m ka kb x] (-> (update m ka safe+ x)
                                      (update kb safe+ x)))

(defn pair-insert [pair-freqs]
  (reduce (fn [pairs [pairA pairB :as pair]]
            (let [match (rules pair)]
              (update-ab-safe+ pairs
                               (list pairA match)
                               (list match pairB) (pair-freqs pair)))) {} (keys pair-freqs)))

(defn solve [steps]
  (let [pair-iterations (iterate pair-insert (frequencies (partition 2 1 template)))
        pair-freqs (nth pair-iterations steps)
        char-freqs (reduce (fn [fqs [[a b] count]]
                             (update-ab-safe+ fqs a b count)) {} pair-freqs)
        most-common (val (apply max-key val char-freqs))
        least-common (val (apply min-key val char-freqs))]
    (str (bigdec (Math/ceil (/ (- most-common least-common) 2))))))

;; part 1
(solve 10)
;; => "2447.0"

;; part 2
(solve 40)
;; => "3018019237563"
