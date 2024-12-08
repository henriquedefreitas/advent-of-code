(ns x
  (:require [clojure.string :as str]))

(def test-input (slurp "test"))

(def input (slurp "input"))

(defn split-rule [rule]
  (str/split rule #"\|"))

(defn split-ordering [rule]
  (str/split rule #","))

(defn get-rules [text]
  (map (fn [pair] (map Integer/parseInt pair))
       (map split-rule
            (first (partition-by empty? (str/split-lines text))))))

(defn get-orderings [text]
  (map (fn [pages] (map Integer/parseInt pages))
       (map split-ordering
            (last (partition-by empty? (str/split-lines text))))))

; a[b] = 1 ---> a precedes b
(defn get-order-mapping [rules-pairs]
  (reduce
   (fn [m [a b]]
     (-> m
         (assoc a (conj (get m a #{}) b))))
   {}
   rules-pairs))

(defn is-ordering-correct? [ordering pages-sucessors]
  (every? true? (for [i (range (count ordering))
                      j (range (count ordering))
                      :when (< i j)]
                  (not (contains? (get pages-sucessors (nth ordering j) #{}) (nth ordering i))))))

(defn get-middle-page [ordering]
  (nth ordering (/ (dec (count ordering)) 2)))

(defn solve-part-one [text]
  (let
   [rules-mapping (get-order-mapping (get-rules text))
    orderings (get-orderings text)]
    (apply + (map
              (fn [ordering]
                (if (is-ordering-correct? ordering rules-mapping)
                  (get-middle-page ordering)
                  0))
              orderings))))

(defn print-part-one []
  (println "Test =" (solve-part-one test-input) "| Expected = 143"
           "\nAnswer = " (solve-part-one input)))

(defn pages-sort [pages rules-mapping]
  (let [comparator
        (fn [a b]
          (cond
            (contains? (get rules-mapping a) b)  1
            (contains? (get rules-mapping b) a) -1
            :else 0))]
    (sort comparator pages)))

(defn solve-part-two [text]
  (let
   [rules-mapping (get-order-mapping (get-rules text))
    orderings (get-orderings text)]
    (apply + (map
              (fn [ordering]
                (if (not (is-ordering-correct? ordering rules-mapping))
                  (get-middle-page (pages-sort ordering rules-mapping))
                  0))
              orderings))))

(defn print-part-two []
  (println "Test =" (solve-part-two test-input) "| Expected = 123"
           "\nAnswer = " (solve-part-two input)))
