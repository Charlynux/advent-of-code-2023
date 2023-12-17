(ns advent-2023.day12
  (:refer-clojure :exclude [==])
  (:require [clojure.string :as str]
            [clojure.core.logic
                      :refer [== lvar defne fresh succeed fail]
             :as logic]
            [clojure.core.logic.fd :as fd]))

(comment
  "https://adventofcode.com/2023/day/12"
  "???.### 1,1,3"
  "?###???????? 3,2,1")

(defne arrange-group [spring tail group-size rest]
  (["#" [] 1 []] succeed)
  (["." _ 0 tail] succeed)
  (["#" [s . tail-springs] _ _]
   (fd/>= group-size 1)
   (fresh [n]
     (fd/- group-size 1 n)
     (arrange-group s tail-springs n rest))))

(defne arrange [springs groups]
  ([[] []] succeed)
  ([["." . tail] _]
   (arrange tail groups))
  ([["#" . tail] [n . tail-groups]]
   (fresh [rest]
     (arrange-group "#" tail n rest)
     (arrange rest tail-groups)))
  ([_ _] fail))

(defn parse-springs [s]
  (->>
   (str/split s #"")
   (map (fn [c]
          (case c
            "?" (lvar)
            c)))))

(defn parse-line [line]
  (let [[springs groups]
        (str/split line #" ")]
    [(parse-springs springs)
     (map #(Integer/parseInt %) (str/split groups #","))]))

(defn find-solutions [springs groups]
  (->>
   (run* [q]
     (== springs q)
     (arrange q groups))
   (map str/join)))

(defn parse-input [input]
  (->>
   (slurp input)
   str/split-lines
   (map parse-line)))

(defn solve-part1 [lines]
  (->> lines
       (map (fn [[springs groups]] (find-solutions springs groups)))
       (map count)
       (reduce + 0)))

(solve-part1 (parse-input "data/day12-example.input"))
(time
 (solve-part1 (parse-input "data/day12.input")))
;; Prend 3s, mais ça marche.
