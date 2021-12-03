(ns advent2021.day2
  (:require
   [clojure.string :as str]
   [advent2021.core :refer [parse-long]]))

(set! *warn-on-reflection* true)

(def input (slurp "inputs/day2"))
(def lines (str/split-lines input))

(defn parse-line [line]
 (let [[direction distance] (str/split line #" ")]
  {:direction direction :distance (parse-long distance)}))

(def course (mapv parse-line lines))

(defn part1 [course]
  (let [ups (filter #(= "up" (:direction %)) course)
        downs (filter #(= "down" (:direction %)) course)
        forwards (filter #(= "forward" (:direction %)) course)]
    (*
     (reduce + (mapv :distance forwards))
     (-
      (reduce + (mapv :distance downs))
      (reduce + (mapv :distance ups))))))

(comment (time (part1 course)))


(defn advance [state step]
 (case (:direction step)
  "down" (assoc state :aim (+ (:aim state) (:distance step)))
  "up" (assoc state :aim (- (:aim state) (:distance step)))
  "forward" (assoc state 
             :h-pos (+ (:h-pos state) (:distance step))
             :depth (+ (:depth state) (* (:aim state) (:distance step))))))
        
(def initial-state {:aim 0 :h-pos 0 :depth 0})        
        
(defn part2 [course]
  (let [final-state (reduce advance initial-state course)]
   (* (:h-pos final-state) (:depth final-state))))

(comment (time (part2 course)))

