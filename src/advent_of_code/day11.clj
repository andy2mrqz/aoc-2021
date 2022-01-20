(ns advent-of-code.day11
  (:require [clojure.string :as str]))

(defn parse-ints [coll] (mapv #(Character/getNumericValue %) coll))

(def input
  (->> (slurp "resources/day11.txt")
       (str/split-lines)
       (mapv parse-ints)))

(defn update-energy [octos coords update-fn]
  (if coords
    (reduce (fn [res coord]
              (if (get-in res coord)
                (update-in res coord update-fn)
                res)) octos coords)
    (mapv #(mapv update-fn %) octos)))

(defn coords-test [octos test-fn]
  (remove nil? (for [r (-> octos count range)
                     c (-> octos first count range)]
                 (when-let [val (get-in octos [r c])]
                   (when (test-fn val) [r c])))))

(defn adjacent
  "Returns the coordinates neighbors including diagonals"
  [coord]
  (mapv #(mapv + coord %) [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]]))

(defn run-step [octos coords]
  (let [coords' (if (seq coords) coords (coords-test octos identity))
        inced (update-energy octos coords' inc)]
    (if-some [mature (seq (coords-test inced #(<= 10 % 500)))]
      (let [adj (mapcat adjacent mature)
            inced-mature (update-energy inced mature (constantly 5000))]
        (recur inced-mature adj))
      (let [flashed (coords-test inced #(> % 9))
            final-octos (update-energy inced flashed (constantly 0))]
        [final-octos nil (count flashed)]))))

(defn octos-count [octos] (count (coords-test octos identity)))
(defn iterations [initial-octos] (iterate (fn [[o c]] (run-step o c)) [initial-octos nil 0]))

(defn -part1
  [octos steps]
  (->> (iterations octos)
       (take (inc steps))
       (mapv #(nth % 2))
       (apply +)))

(-part1 input 100)
;; => 1659

(defn -part2
  [octos]
  (->> (iterations octos)
       (map #(nth % 2))
       (keep-indexed #(when (= %2 (octos-count octos)) %1))
       first))

(-part2 input)
;; => 227

;; PS:
;; Ideas to refactor part1 and part2 using iterate come from abyala:
;; https://github.com/abyala/advent-2021-clojure/blob/main/src/advent_2021_clojure/day11.clj
