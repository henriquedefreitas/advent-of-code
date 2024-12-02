(ns x
  (:require 
    [clojure.string :as str]))

(defn load-txt [] (slurp "input.txt"))

(defn matrix-of [text] 
  (for [line (str/split-lines text)]
    (str/split line #"\W+")))

(def matrix (matrix-of (load-txt)))

(defn get-nth-col [matrix n]
  (for [row matrix] (nth row n)))

(def left-list (sort (get-nth-col matrix 0)))

(def right-list (sort (get-nth-col matrix 1)))

(defn parse-list [str-list] (map Integer/parseInt str-list))

(def abs-differences 
    (map abs 
         (map - 
           (parse-list left-list)
           (parse-list right-list))))

(defn part-one []
  (println (apply + abs-differences)))

(defn map-nils-to-zero [values] 
  (map 
    (fn [x] (if (nil? x) 0 x))
    values))

(def freq
  (map-nils-to-zero
   (map (partial get (frequencies right-list)) left-list)))

(defn part-two []
  (println 
    (apply + (map * (parse-list left-list) freq))))
