(ns counting.core
    (:require [clojure.core.reducers :as r]))

; counting numbers
(defn recursive-sum [numbers]
    (if (empty? numbers) 0
        (+ (first numbers (recursive-sum (rest numbers))))))

(defn reduce-sum [numbers]
    (reduce + numbers))

(defn parallel-sum [numbers]
    (r/fold + numbers))

(def numbers (range 0 10000000))

(time (recursive-sum numbers))
(time (reduce-sum numbers))
(time (parallel-sum numbers))


; counting words
(defn get-words [text] (re-seq #"\w+" text))​

(defn count-words-sequential [pages]
    (frequencies (mapcat get-words pages)))

(def merge-counts (partial merge-with +))

(defn count-words-parallel [pages]
    (reduce merge-counts
        (pmap #(frequencies (get-words %)) pages)))

(defn count-words-batch [pages]
    (reduce merge-counts
        (pmap count-words-sequential (partition-all 100 pages)))

(def pages ["one potato two potato three potato four" "five potato six potato seven potato more"])

(time (count-words-sequential pages))
(time (count-words-parallel pages))
(time (count-words-batch pages))