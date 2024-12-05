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

(defn are-x-corners? [matrix i1 j1 i2 j2]
  (or
   (and
    (= (get-at-pos matrix i1 j1) \S)
    (= (get-at-pos matrix i2 j2) \M))
   (and
    (= (get-at-pos matrix i1 j1) \M)
    (= (get-at-pos matrix i2 j2) \S))))

(defn is-x-center? [matrix i j]
  (and
   (= (get-at-pos matrix i j) \A)
   (are-x-corners? matrix (inc i) (dec j) (dec i) (inc j))
   (are-x-corners? matrix (dec i) (dec j) (inc i) (inc j))))

(defn run-problem-two [cw]
  (get
   (frequencies
    (for
     [i (range 1 (dec (get-height cw)))
      j (range 1 (dec (get-width cw)))]
      (is-x-center? cw i j)))
   true))

(defn part-two []
  (println "Test ->" (run-problem-two (crossword test-txt)) "Expected = 9"
           "\nChallenge ->" (run-problem-two (crossword input-txt))))

