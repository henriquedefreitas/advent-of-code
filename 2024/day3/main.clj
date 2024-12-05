(ns x)

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

;; This didn't work, I don't know why
;; (defn run-problem-two [input]
;;   (run-problem-one (str/replace (str/replace input regex1 "X") regex2 "X")))

(def enable-instruction "do()")

(def disable-instruction "don't()")

(defn get-instructions [input]
  (re-seq #"mul\(\d{1,3},\d{1,3}\)|don't\(\)|do\(\)" input))

(defn solve-mul [mul-instruction]
  (apply * (map Integer/parseInt (first (get-mul-pairs mul-instruction)))))

(defn run-instruction [acc instruction]
  (cond
    (= instruction enable-instruction) [(first acc) true]
    (= instruction disable-instruction) [(first acc) false]
    (false? (second acc)) acc
    (true? (second acc)) [(+ (first acc) (solve-mul instruction)) true]))

(defn run-problem-two [input]
  (first (reduce run-instruction [0 true] (get-instructions input))))

(defn part-two []
  (println "Test -> Answer =" (run-problem-two test2-txt) ". Expected = 48"
           "\nChallenge -> Answer =" (run-problem-two input-txt)))
