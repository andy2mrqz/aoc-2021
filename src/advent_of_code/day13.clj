(ns advent-of-code.day13
  (:require [clojure.string :as str]))

(defn get-coords [l] (->> (re-seq #"\d+" l)
                          (mapv #(Integer/parseInt %))))
(defn get-folds [l] (first (re-seq #"([x|y])=(\d+)" l)))
(defn third [coll] (Integer/parseInt (nth coll 2)))

(def input
  (->> (slurp "resources/day13.txt")
       (str/split-lines)))

(def coords (->> (take-while (complement str/blank?) input)
                 (mapv get-coords)))
(def folds  (->> (drop-while #(not (str/starts-with? % "fold")) input)
                 (mapv (comp (juxt second third) get-folds))))

(defn x? [dim] (= dim "x"))
(defn mirror [center to-mirror] (- center (- to-mirror center)))
(defn mirror-x [center [x y]] [(mirror center x) y])
(defn mirror-y [center [x y]] [x (mirror center y)])

(defn apply-folds [sheet [dim idx]]
  (let [[unaffected to-fold] (-> (group-by (fn [[x y]] (< (if (x? dim) x y) idx)) sheet) vals)
        folded (map #((if (x? dim) mirror-x mirror-y) idx %) to-fold)]
    (set (concat unaffected folded))))

;; part 1
(->> (apply-folds coords (first folds))
     count)
;; => 765

;; part 2
(->> (reduce apply-folds coords folds)
     count)