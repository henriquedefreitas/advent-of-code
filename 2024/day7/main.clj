(ns x
  (:require
   [clojure.string :as str]))

(require '[clojure.math :as math])

(def test-input (slurp "test"))

(def input (slurp "input"))

(defn parse-result-operands [text]
  (for [line (str/split-lines text)]
    (let [splitted (str/split line #"\h*:\h*")
          result (first splitted)
          operands (str/split (second splitted) #" ")]
      [(parse-long result)
       (map parse-long operands)])))

(defn is-result-achievable? [target operands acc]
  (cond
    (empty? operands) (= target acc)
    :else (or
           (is-result-achievable? target (rest operands) (+ acc (first operands)))
           (is-result-achievable? target (rest operands) (* acc (first operands))))))

(defn run-with-printing [solver expected]
  (println "Test =" (solver test-input) "| Expected =" expected
           "\nAnswer = " (solver input)))

(defn solve-problem [text achievable-predicate]
  (apply +
         (map first
              (filter
               (fn [line]
                 (let [result (first line)
                       operands (second line)]
                   (achievable-predicate result (rest operands) (first operands))))
               (parse-result-operands text)))))

(defn solve-part-one [text]
  (solve-problem text is-result-achievable?))

(defn print-part-one []
  (run-with-printing solve-part-one 3749))

;; Has rounding problems
;; (defn length [num]
;;   (math/floor (/ (math/log num) (math/log 10))))

(defn length [num]
  (count (str num)))

;; (defn concat-numbers [a b]
;;   (+ (* a (math/pow (length b) 10)) b))

(defn concat-numbers [a b]
  (parse-long (str a b)))

(defn is-result-achievable-v2? [target operands acc]
  (cond
    (empty? operands) (= target acc)
    :else (or
           (is-result-achievable-v2? target (rest operands) (+ acc (first operands)))
           (is-result-achievable-v2? target (rest operands) (* acc (first operands)))
           (is-result-achievable-v2? target (rest operands)
                                     (concat-numbers acc (first operands))))))

(defn solve-part-two [text]
  (solve-problem text is-result-achievable-v2?))

(defn print-part-two []
  (run-with-printing solve-part-two 11387))
