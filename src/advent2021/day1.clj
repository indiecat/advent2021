(ns advent2021.day1
  (:require
   [clojure.string :as str]
   [advent2021.core :refer [parse-long]]))

(set! *warn-on-reflection* true)

(def input (slurp "inputs/day1"))
(def lines (str/split input #"\n"))
(def depths (map parse-long lines))

(defn part1 [depths]
  (let [first-values (butlast depths)
        second-values (drop 1 depths)]
    (->> 
      (map - second-values first-values)
      (filter pos?)
      count)))

#_(time (part1 depths))


(defn part2 [depths]
  (let [windows (partition 3 1 depths)
        first-windows (butlast windows)
        second-windows (drop 1 windows)]
    (->> 
      (map - 
           (map #(reduce + %) second-windows)
           (map #(reduce + %) first-windows))
      (filter pos?)
      count)))

#_(time (part2 depths))

#_(require 'advent2021.day1 :reload)
