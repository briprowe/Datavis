(ns datavis.parameter-file
  (:use datavis.util)
  (:require [clojure.java [io :as io]]
            [clojure [string :as str]]))

;; This file is intended to implement reading the list of input data
;; sets and the color associated with each set.
;;
;; The file is line-oriented.  Each line is of the form:
;;
;; <dataset filename> <red color> <green color> <blue color>
;;
;; There can be an arbitrary number of data files.

(defn parameter-sequence
  [tokens]
  (lazy-seq
   (if-let [toks (seq (take 4 tokens))]
     (cons {:filename (nth toks 0)
            :color (vec (for [x (drop 1 toks)] (Integer. x)))}
           (parameter-sequence (drop 4 (seq tokens))))
     nil)))

(defn get-parameter-files
  [filename]
  (let [input (io/make-reader filename {:encoding "UTF-8"})
        tokens (tokenize-line (read-lines input))]
    (parameter-sequence tokens)))