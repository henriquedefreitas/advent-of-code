(ns x
  (:require
   [clojure.string :as str]))

(def input-txt (slurp "input"))

(def test-txt (slurp "test"))

(def shifts [-1 0 1])

(defn crossword [text]
  (str/split-lines text))

(defn get-width [matrix] (count matrix))

(defn get-height [matrix] (count (nth matrix 0)))

(defn get-at-pos [matrix i j] (nth (nth matrix i) j))

(defn find-word [word i j row-shift col-shift matrix]
  (cond
    (empty? word) 1
    (not (<= 0 i (dec (get-height matrix)))) 0
    (not (<= 0 j (dec (get-width matrix)))) 0
    :else
    (if (= (first word) (get-at-pos matrix i j))
      (find-word (rest word) (+ i row-shift) (+ j col-shift) row-shift col-shift matrix)
      0)))

(defn run-problem-one [cw]
  (apply + (for [i (range (get-height cw))
                 j (range (get-width cw))
                 row-shift shifts
                 col-shift shifts]
             (find-word "XMAS" i j row-shift col-shift cw))))

(defn part-one []
  (println "Test ->" (run-problem-one (crossword test-txt)) "Expected = 18"
           "\nChallenge ->" (run-problem-one (crossword input-txt))))
