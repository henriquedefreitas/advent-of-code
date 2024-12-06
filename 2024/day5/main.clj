(ns x
  (:require [clojure.string :as str]))

(def test-input (slurp "test"))

(def input (slurp "input"))

(defn split-rule [rule]
  (str/split rule #"\|"))

(defn split-ordering [rule]
  (str/split rule #","))

(defn get-rules [text]
  (map split-rule (first (partition-by empty? (str/split-lines text)))))

(defn get-orderings [text]
  (map split-ordering (last (partition-by empty? (str/split-lines text)))))

; a[b] = -1 ----> b precedes a
(defn get-order-mapping [rules]
  (reduce
   (fn [m [a b]]
     (-> m
         (assoc-in [a b] true)))
   {}
   rules))

(defn are-pages-correct? [ordering rules-mapping]
  (for [i (range (count ordering))
        j (range (count ordering))
        :when (< i j)]
    (not (true? (get-in rules-mapping [(nth ordering j) (nth ordering i)])))))

(defn get-middle-page [ordering]
  (nth ordering (/ (dec (count ordering)) 2)))

(defn solve-part-one [text]
  (let
   [rules-mapping (get-order-mapping (get-rules text))
    orderings (get-orderings text)]
    (apply + (map
              (fn [ordering]
                (if (true? (are-pages-correct? ordering rules-mapping))
                  (get-middle-page ordering)
                  0))
              orderings))))

(defn print-part-one []
  (println "Test =" (solve-part-one test-input) "| Expected = 143"
           "\nAnswer = " (solve-part-one input)))

(defn solve-part-two [text])

(defn print-part-two []
  (println "Test =" (solve-part-two test-input) "| Expected = "
           "\nAnswer = " (solve-part-two input)))
