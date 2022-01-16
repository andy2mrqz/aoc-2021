(ns advent-of-code.day09
  (:require [clojure.string :as str]))

(defn parse-ints [coll] (mapv #(Character/getNumericValue %) coll))

(defn transpose [c] (apply mapv vector c))

(def input
  (->> (slurp "resources/day09.txt")
       (str/split-lines)
       (mapv parse-ints)))

(defn maybe-lowest [m x y]
  (let [it (get-in m [x y])
        n (get-in m [(dec x) y] 9)
        s (get-in m [(inc x) y] 9)
        e (get-in m [x (inc y)] 9)
        w (get-in m [x (dec y)] 9)]
    (when (and (= it (min it n s e w)) (not= it n s e w)) it)))

(defn low-points [hm]
  (->> (for [r (-> hm count range)
             c (-> (first hm) count range)]
         [(maybe-lowest hm r c) r c])
       (remove #(-> % first nil?))))

(defn -part1 [hm] (reduce #(+ %1 (-> %2 first inc)) 0 (low-points hm)))

(-part1 input)
;; => 550

(defn valid? [hm s k]
  (false? (or (contains? s k)
              (when-let [val (get-in hm k)] (= val 9)))))

(defn dfs [hm s [x y :as key]]
  (let [visited (conj s key)
        nkey [(dec x) y]
        skey [(inc x) y]
        ekey [x (inc y)]
        wkey [x (dec y)]
        nvis (if (valid? hm s nkey) (dfs hm visited nkey) visited)
        svis (if (valid? hm s skey) (dfs hm nvis skey) nvis)
        evis (if (valid? hm s ekey) (dfs hm svis ekey) svis)
        wvis (if (valid? hm s wkey) (dfs hm evis wkey) evis)]
    wvis))

(defn -part2 [hm]
  (->> (low-points hm)
       (mapv (fn [[_v r c]] (dfs hm #{} [r c])))
       (sort-by count)
       (take-last 3)
       (mapv count)
       (apply *)))

(-part2 input)
;; => 1100682
