(ns datavis.test.core
  (:use [datavis.core])
  (:use [clojure.test])
  (:require [clojure.java [io :as io]]))

(def *file1* nil)
(def *file2* nil)
(def *file3* nil)

(deftest test-get-color
  (with-open [file1 (read-data-file *file1*)
              file2 (read-data-file *file2*)
              file3 (read-data-file *file3*)]
    ))
