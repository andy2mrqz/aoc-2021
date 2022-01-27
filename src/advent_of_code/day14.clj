(ns advent-of-code.day14
  (:require [clojure.string :as str]))

(def input
  (->> (slurp "resources/day14.txt")
       (str/split-lines)))

(def template (first input))

(def rules (->> (drop 2 input)
                (map (comp (juxt (comp seq second) #(nth % 2))
                           #(re-matches #"(\w+) -> (\w)" %)))
                (into {})))

(defn pair-insertion [template]
  (let [pairs (partition 2 1 template)
        inserted (for [pair pairs]
                   (apply str ((juxt first rules second) pair)))
        lastc ((comp last last) inserted)]
    (apply str (-> (mapcat #(take 2 %) inserted)
                   vec
                   (conj lastc)))))

;; part 1
(let [char-freqs (frequencies (nth (iterate pair-insertion template) 10))
      most-common (apply max-key val char-freqs)
      least-common (apply min-key val char-freqs)]
  (- (val most-common) (val least-common)))
;; => 2447
