(ns advent-of-code.day04
  (:require [clojure.string :as s])
  (:gen-class))

(def puzzle-input
  (->> (slurp "resources/day04.txt")
       (s/split-lines)))

(defn bingo-nums [input]
  (as-> (first input) $
    (s/split $ #",")
    (map #(Integer/parseInt %) $)))

(def EMPTY :e)
(def MARKED :m)

(defn create-line [m n] (assoc m (Integer/parseInt n) EMPTY))

(defn create-board
  [acc line]
  (if (s/blank? line)
    (conj acc [])
    (update acc ((comp dec count) acc)
            conj (reduce create-line {} (re-seq #"\d+" line)))))

(defn bingo-boards [input] (reduce create-board [] (rest input)))

(defn find-first [pred coll] (first (filter pred coll)))

(defn winning-row? [line] (every? #(= % MARKED) (vals line)))

(defn transpose [v] (apply map vector v))

(defn winning-board?
  [board]
  (or
   (some winning-row? board)
   (some winning-row? (transpose board))))

(defn mark-line
  [number board line]
  (conj board (if (line number) (assoc line number MARKED) line)))

(defn mark-board
  [number board]
  (reduce (partial mark-line number) [] board))

(defn score
  [board winning-number]
  (->> (apply merge board)
       (filter #(= EMPTY (val %)))
       (map first)
       (reduce +)
       (* winning-number)))

(defn solve-part-1
  [boards number]
  (let [updated-boards (map (partial mark-board number) boards)
        winning-board (find-first #(winning-board? %) updated-boards)]
    (if winning-board
      (reduced (score winning-board number))
      updated-boards)))

(defn -part1 [input] (reduce solve-part-1 (bingo-boards input) (bingo-nums input)))

(-part1 puzzle-input)
;; => 58412

(defn solve-part-2
  [boards number]
  (let [updated-boards (map (partial mark-board number) boards)
        except-winner (if (= 1 (count updated-boards))
                        updated-boards
                        (filter #((comp not winning-board?) %) updated-boards))
        maybe-winner (first except-winner)]
    (if (and (= 1 (count except-winner)) (winning-board? maybe-winner))
      (reduced (score maybe-winner number))
      except-winner)))

(defn -part2 [input] (reduce solve-part-2 (bingo-boards input) (bingo-nums input)))

(-part2 puzzle-input)
;; => 10030
