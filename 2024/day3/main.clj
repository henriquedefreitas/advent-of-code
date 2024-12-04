(ns x
  (:require [clojure.string :as str]))

(def test1-txt (slurp "part1.test"))

(def test2-txt (slurp "part2.test"))

(def input-txt (slurp "input"))

(defn get-mul-pairs [input]
  (map rest (re-seq #"mul\((\d{1,3}),(\d{1,3})\)" input)))

(defn run-problem-one [input]
  (apply +
         (map
          (fn [pair] (apply * (map Integer/parseInt pair)))
          (get-mul-pairs input))))

(defn part-one []
  (println "Test -> Answer =" (run-problem-one test1-txt) ". Expected = 161"
           "\nChallenge -> Answer =" (run-problem-one input-txt)))

(def regex1 #"don't\(\).*?do\(\)")

(def regex2 #"don't\(\).*?\n")

;; It didn't work, I don't know why
(defn run-problem-two [input]
  (run-problem-one (str/replace (str/replace input regex1 "X") regex2 "X")))

(defn part-two []
  (println "Test -> Answer =" (run-problem-two test2-txt) ". Expected = 48"
           "\nChallenge -> Answer =" (run-problem-two input-txt)))
