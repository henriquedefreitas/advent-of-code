(ns x
  (:require
   [clojure.string :as str]))

(def test-input (slurp "test"))

(def input (slurp "input"))

(defn parse-char [c]
  (- (int c) (int \0)))

(defn parse-line [string]
  (map parse-char string))

(defn id [block]
  (first block))

(defn qty [block]
  (second block))

(defn free? [block]
  (= (id block) -1))

(defn process-disk-map
  "Return a list where each element is [block-id quantity].
  ID = -1 represents free space"
  [disk-map]
  (first
   (reduce
    (fn [[processed id free] qty]
      [(conj processed [(if free -1 id) qty]) (if free id (inc id)) (not free)])
    [[] 0 false]
    disk-map)))

(defn sum-range [a1 an]
  (/ (* (+ a1 an) (inc (- an a1))) 2))

(defn mv-last-block-to-beggining [blocks]
  (cond
    (= (qty (first blocks)) (qty (last blocks)))
    (concat
     [(last blocks)]
     (rest (butlast blocks)))
    (< (qty (first blocks)) (qty (last blocks)))
    (concat
     [[(id (last blocks)) (qty (first blocks))]]
     (rest (butlast blocks))
     [[(id (last blocks)) (- (qty (last blocks)) (qty (first blocks)))]])
    :else (concat
           [[(id (last blocks)) (qty (last blocks))]]
           [[(id (first blocks)) (- (qty (first blocks)) (qty (last blocks)))]]
           (rest (butlast blocks)))))

(defn compacted-disk-sum [blocks idx]
  (letfn [(checksum-block [idx qty cur-id]
            (*
             (sum-range idx (dec (+ idx qty)))
             cur-id))]
    (loop [blocks blocks
           idx idx
           sum 0]
      (cond
        (empty? blocks) sum
        (free? (last blocks)) (recur (butlast blocks) idx sum)
        (free? (first blocks)) (recur (mv-last-block-to-beggining blocks) idx sum)
        :else (let [current-sum (checksum-block idx (qty (first blocks)) (id (first blocks)))]
                (recur (rest blocks)
                       (+ idx (qty (first blocks)))
                       (+ sum current-sum)))))))

(defn solve-part-one [line]
  (compacted-disk-sum (process-disk-map (parse-line (str/trim line))) 0))

(defn solve-part-two [line])

(defn test-part-one []
  (str "Test = " (solve-part-one test-input) " | Expected = 1928"))

(defn run-part-one []
  (str "Answer = " (solve-part-one input)))

(defn test-part-two []
  (str "Test = " (solve-part-two test-input) " | Expected = 1928"))

(defn run-part-two []
  (str "Answer = " (solve-part-two input)))
