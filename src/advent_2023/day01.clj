(ns advent-2023.day01
  (:require [clojure.string :as str]))

(defn extract-digits [s]
  (let [ns (re-seq #"\d" s)]
    (Integer/parseInt (str (first ns) (last ns)))))

(extract-digits "treb7uchet")

(def example (list "1abc2"
                  "pqr3stu8vwx"
                  "a1b2c3d4e5f"
                  "treb7uchet"))

(defn solve-part1 [lines]
  (->> lines
       (map extract-digits)
       (reduce +)))

(solve-part1 example)

(solve-part1
 (str/split-lines (slurp "data/day01.input")))

(defn parse-digit [s]
  (case s
    "one" 1
    "two" 2
    "three" 3
    "four" 4
    "five" 5
    "six" 6
    "seven" 7
    "eight" 8
    "nine" 9
    (Integer/parseInt s)))

(defn extract-digits-2 [s]
  "On ne peut pas utiliser re-seq avec une regex à cause des 'digits' qui s'overlap.

Ex. oneight donne 18"
  (let [ns (loop [ns []
                  remaining-s s]
             (if (empty? remaining-s)
               ns
               (let [[_ n] (re-find #"^(\d|zero|one|two|three|four|five|six|seven|eight|nine).*"
                                    remaining-s)]
                 (recur
                  (if (some? n)
                    (conj ns n)
                    ns)
                  (subs remaining-s 1)))))
        a (parse-digit (first ns))
        b (parse-digit (last ns))]
    (+ (* 10 a) b)))

(assert (= 18 (extract-digits-2 "oneight")))

(defn solve-part2 [lines]
  (->> lines
       (map extract-digits-2)
       (reduce +)))

(solve-part2
 (str/split-lines
  "two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen"))

(solve-part2
 (str/split-lines (slurp "data/day01.input")))
;; 54676
