(ns x
  (:require
   [clojure.string :as str]))

(def txt (slurp "input"))

(defn matrix-of [text]
  (for [line (str/split-lines text)]
    (str/split line #"\W+")))

(def matrix-txt (matrix-of txt))

(defn parse-list [str-list] (map Integer/parseInt str-list))

(def matrix (map parse-list matrix-txt))

(defn increasing-by-at-most? [diff a b]
  (<= 1 (- b a) diff))

(defn decreasing-by-at-most? [diff a b]
  (<= 1 (- a b) diff))

(defn compare-pairs [f coll]
  (map (fn [[a b]] (f a b)) (partition 2 1 coll)))

(defn inc-list? [list]
  (every? true? (compare-pairs (partial increasing-by-at-most? 3) list)))

(defn dec-list? [list]
  (every? true? (compare-pairs (partial decreasing-by-at-most? 3) list)))

(defn safe? [row]
  (or (inc-list? row) (dec-list? row)))

(defn remove-one-at-a-time [coll]
  (map-indexed (fn [idx _]
                 (concat (take idx coll) (drop (inc idx) coll)))
               coll))

(defn part-one []
  (println
   (get
    (frequencies
     (map safe? matrix))
    true)))

(defn part-two []
  (println
   (get
    (frequencies
     (for [row matrix]
       (or
        (safe? row)
        (some safe? (remove-one-at-a-time row)))))
    true)))
