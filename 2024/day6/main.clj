(ns x
  (:require [clojure.string :as str]))

(def test-input (slurp "test"))

(def input (slurp "input"))

(defn matrix-from [text]
  (str/split-lines text))

; Up -> Right -> Down -> Left
(def directions [[-1 0] [0 1] [1 0] [0 -1]])

(defn get-height [matrix]
  (count matrix))

(defn get-width [matrix]
  (count (first matrix)))

(defn out-of-bounds? [coord matrix]
  (let [row (first coord) col (second coord)]
    (not (and (< -1 row (get-height matrix))
              (< -1 col (get-width matrix))))))

(defn move [coord direction]
  (map + coord direction))

(defn get-from-coord [coord matrix]
  (let [row (first coord)
        col (second coord)]
    (nth (nth matrix row) col)))

(defn is-blocked? [matrix coord]
  (= (get-from-coord coord matrix) \#))

(defn find-coordinates [matrix element]
  (first (for [[row-index row] (map-indexed vector matrix)
               [col-index col] (map-indexed vector row)
               :when (= col element)]
           [row-index col-index])))

(defn get-guard-coord [matrix]
  (find-coordinates matrix \^))

(defn next-direction-idx [idx]
  (mod (inc idx) 4))

;; This recursive function resulted in stack overflow
;; (defn do-round [matrix coord direction-idx visited]
;;   (let [direction (nth directions direction-idx)
;;         next-coord (move coord direction)]
;;     (cond
;;       (out-of-bounds? next-coord matrix)
;;       (conj visited coord)
;;       (is-blocked? matrix next-coord)
;;       (do-round
;;        matrix coord (next-direction-idx direction-idx) visited)
;;       :else
;;       (do-round
;;        matrix next-coord direction-idx (conj visited coord)))))

;; Version with tail call optimization
(defn do-round [matrix coord direction-idx visited]
  (loop [current-coord coord
         current-direction-idx direction-idx
         current-visited visited]
    (let [direction (nth directions current-direction-idx)
          next-coord (move current-coord direction)]
      (cond
        (out-of-bounds? next-coord matrix)
        (conj current-visited current-coord)

        (is-blocked? matrix next-coord)
        (recur current-coord
               (next-direction-idx current-direction-idx)
               current-visited)

        :else
        (recur next-coord
               current-direction-idx
               (conj current-visited current-coord))))))

(defn solve-part-one [text]
  (let [matrix (matrix-from text)]
    (count (do-round matrix (get-guard-coord matrix) 0 #{}))))

(defn print-part-one []
  (println "Test =" (solve-part-one test-input) "| Expected = 41"
           "\nAnswer = " (solve-part-one input)))

(defn solve-part-two [text])

(defn print-part-two []
  (println "Test =" (solve-part-two test-input) "| Expected = "
           "\nAnswer = " (solve-part-two input)))
