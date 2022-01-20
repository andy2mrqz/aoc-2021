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

(defn run-step [octos]
  (let [;; step 1
        octos1  (update-energy octos nil inc)
        ;; step 2.1
        octoloop (loop [os octos1]
                   (let [mature (coords-test os #(<= 10 % 500))]
                     (if (seq mature)
                       (let [adj (mapcat adjacent mature)
                             inc-mature (update-energy os mature (constantly 5000))
                             next-os (update-energy inc-mature adj inc)]
                         (recur next-os))
                       os)))
        octoloop-gt9 (coords-test octoloop #(> % 9))
        flashes (count octoloop-gt9)
        ;; step 3
        octos3 (update-energy octoloop octoloop-gt9 (constantly 0))]
    [octos3 flashes]))

(defn -part1
  [octos steps]
  (loop [octos octos, flashes 0, step 0]
    (if (= step steps)
      flashes
      (let [[new-octos next-flashes] (run-step octos)]
        (recur new-octos (+ flashes next-flashes) (inc step))))))

(-part1 input 100)
;; => 1659

(defn -part2
  [octos]
  (loop [octos octos, flashes 0, step 0]
    (if (= flashes (count (coords-test octos identity)))
      step
      (let [[new-octos next-flashes] (run-step octos)]
        (recur new-octos next-flashes (inc step))))))

(-part2 input)
;; => 227
