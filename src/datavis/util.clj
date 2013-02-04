(ns datavis.util
  (:require [clojure.java [io :as io]]
            [clojure [string :as str]]))

(defn parse-double
  [s]
  (Double. s))

(defn tokenize-line
  "Given a sequence of lines, return a lazy sequence of tokens."
  [lines]
  (remove #(= "" %)
          (mapcat #(str/split % #"\s")
                  lines)))

(defn read-lines
  "Take a clojure.java.io.Reader and return a lazy sequence of lines
   in the file."
  [input]
  (lazy-seq
   (if-let [line (.readLine input)]
     (cons line (read-lines input))
     ())))
