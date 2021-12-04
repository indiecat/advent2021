(ns advent2021.day3
  (:require
   [clojure.string :as str]
   [advent2021.core :refer [parse-int parse-binary]]))

(set! *warn-on-reflection* true)

(def input (slurp "inputs/day3"))
(def lines (str/split-lines input))

(defn line->digits [line]
 (mapv parse-int (str/split line #"")))

(def report (mapv line->digits lines))
(def report-length (count report))
(def entry-width (count (first report)))

(defn count-digits-at-index [digit index report]
 (->> report
  (map #(nth % index))
  (filter #(= digit %))
   count))

(def gamma-str 
  (->> (mapv #(count-digits-at-index 1 % report) (range entry-width))
      (mapv #(if (> % (- report-length %)) 1 0))
       str/join))

(def gamma (parse-binary gamma-str))
   
(def max-decimal-value (dec (int (Math/pow 2 entry-width)))) 
   
(defn part1 []
  (* gamma (- max-decimal-value gamma)))

(comment (time (part1)))
(println "Part 1: " (part1))





(defn most-common-at-index [index entries]
  (as-> entries $
    (map #(nth % index) $)
    (frequencies $)
     (cond 
      (> (get $ 1 0) (get $ 0 0)) 1
      (< (get $ 1 0) (get $ 0 0)) 0
      :else 1)))

(defn least-common-at-index [index entries]
  (as-> entries $
    (map #(nth % index) $)
    (frequencies $)
     (cond 
      (> (get $ 1 0) (get $ 0 0)) 0
      (< (get $ 1 0) (get $ 0 0)) 1
      :else 0)))

(defn bit-at-index? [index bit entry]
  (= bit (nth entry index)))

(def oxy-gen-rating 
  (-> (loop [index 0
        entries report
        entries-count report-length]
        (if (= entries-count 1)
          (first entries)
          (let [common-bit (most-common-at-index index entries)
            new-entries (filter #(bit-at-index? index common-bit %) entries)
            new-entries-count (count new-entries)]
            (recur
              (inc index)
              new-entries
              new-entries-count))))
  str/join
  parse-binary))

(def co2-scrub-rating 
  (-> (loop [index 0
        entries report
        entries-count report-length]
        (if (= entries-count 1)
          (first entries)
          (let [common-bit (least-common-at-index index entries)
            new-entries (filter #(bit-at-index? index common-bit %) entries)
            new-entries-count (count new-entries)]
            (recur
              (inc index)
              new-entries
              new-entries-count))))
  str/join
  parse-binary))

(defn part2 []
  (* oxy-gen-rating co2-scrub-rating))

(comment (time (part2)))
(println "Part 2: " (part2))

