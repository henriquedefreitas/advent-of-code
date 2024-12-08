(ns x
  (:require
   [clojure.string :as str]))

(require 'clojure.set)

(def test-input (slurp "test"))

(def input (slurp "input"))

(defn matrix-of [text]
  (str/split-lines text))

(defn height [matrix]
  (count matrix))

(defn width [matrix]
  (count (first matrix)))

(defn -bounds? [matrix [row col]]
  (not (and (< -1 row (height matrix))
            (< -1 col (width matrix)))))

(defn get-from-coord [matrix row col]
  (nth (nth matrix row) col))

(defn freq-to-antennas [grid]
  (reduce
   (fn [acc [key val]]
     (if (not= key \.)
       (assoc acc key (conj (get acc key) val))
       acc))
   {}
   (for [row (range (height grid))
         col (range (width grid))]
     [(get-from-coord grid row col)
      [row col]])))

(defn get-antinodes [grid coords]
  (set
   (filter (fn [coord] (not (-bounds? grid coord)))
           (for [antenna1 coords
                 antenna2 coords
                 :when (not= antenna1 antenna2)]
             (let [diff (map - antenna2 antenna1)]
               (map + antenna2 diff))))))

(defn solve-part-one [text]
  (let [grid (matrix-of text)]
    (count
     (set
      (reduce clojure.set/union
              (for [[_ antennas] (freq-to-antennas grid)]
                (get-antinodes grid antennas)))))))

(defn get-antinodes-2 [grid coords]
  (letfn [(generate-antinode [start diff]
            (loop [current start
                   acc #{}]
              (let [next (map + current diff)]
                (if (-bounds? grid next)
                  acc
                  (recur next (conj acc next))))))]
    (set
     (apply concat
            (for [antenna1 coords
                  antenna2 coords
                  :when (not= antenna1 antenna2)]
              (let [diff (map - antenna2 antenna1)]
                (generate-antinode antenna1 diff)))))))

(defn solve-part-two [text]
  (let [grid (matrix-of text)]
    (count
     (set
      (reduce clojure.set/union
              (for [[_ antennas] (freq-to-antennas grid)]
                (get-antinodes-2 grid antennas)))))))

(defn run-with-printing [solver expected]
  (println "Test =" (solver test-input) "| Expected =" expected
           "\nAnswer = " (solver input)))

(defn print-part-one []
  (run-with-printing solve-part-one 14))

(defn print-part-two []
  (run-with-printing solve-part-two 34))
