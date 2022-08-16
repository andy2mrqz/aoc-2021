(ns advent-of-code.day15
  (:require [clojure.string :as str]))

(defn safe-conj-set [s i] (conj (or s #{}) i))

(def cave
  (->> (slurp "resources/day15.sample")
       (str/split-lines)
       (map-indexed (fn [y line]
                      (map-indexed (fn [x horiz]
                                     [[x y] (Integer/parseInt (str horiz))])
                                   (seq line))))
       (apply concat)
       (into {})))

(defn adjacent [cell]
  (mapv #(mapv + cell %) [[-1 0]  [0 -1] [0 1]  [1 0]]))

(defn make-adjacency-list [m cell]
  (reduce (fn [res neighbor]
            (if (cave neighbor)
              (update res cell safe-conj-set neighbor)
              res)) m (adjacent cell)))

(def adj-list
  (->> (keys cave)
       (reduce make-adjacency-list {})))

(defn not-visited? [cell history] (not (contains? (set history) cell)))
(defn visitable [cell history]
  (filter (fn [cell] (not-visited? cell history)) (adj-list cell)))

(def cave-dimensions (int (Math/sqrt (count cave))))
(def end [(dec cave-dimensions) (dec cave-dimensions)])

(defn find-paths [cell history]
  (let [new-history (conj history cell)]
    (if (= cell end)
      [new-history]
      (mapcat #(find-paths % new-history) (visitable cell new-history)))))

;; part 1
(->> (find-paths [0 0] [])
     count)
