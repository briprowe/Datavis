(ns datavis.core
  (:import [java.awt Transparency]
           [java.awt.image
            DataBuffer
            BufferedImage
            WritableRaster
            ComponentColorModel]
           [java.awt.color ColorSpace]
           [javax.imageio ImageIO])
  (:use datavis.util)
  (:require [clojure.java [io :as io]]
            [clojure [string :as str]]
            [datavis [parameter-file :as p]])
  (:gen-class))

(defonce +epsilon+ 1.0e-10)

(defn get-ncols
  [line]
  (let [s (str/trim (second (remove #(= "" %) (str/split line #"\s"))))]
    (Integer. s)))

(defn get-nrows
  [line]
  (let [s (str/trim (second (remove #(= "" %) (str/split line #"\s"))))]
    (Integer. s)))

(defn get-nodata
  [line]
  (let [s (str/trim (second (remove #(= "" %) (str/split line #"\s"))))]
    (Double. s)))

(defn make-pixel
  [[a r g b]]
  (+ (bit-shift-left (bit-and a 0xff) 24)
     (bit-shift-left (bit-and r 0xff) 16)
     (bit-shift-left (bit-and g 0xff) 8)
     (bit-and b 0xff)))

(defn make-red
  [data red]
  {:post [(<= % 255)]}
  (int (reduce + (map * data red))))

(defn make-green
  [data green]
  {:post [(<= % 255)]}
  (int (reduce + (map * data green))))

(defn make-blue
  [data blue]
  {:post [(<= % 255)]}
  (int (reduce + (map * data blue))))

(defn normalize
  [data]
  (let [s (reduce + data)]
    (if (< s +epsilon+)
      (vec (repeat (count data) 0))
      (vec (map #(/ % s) data)))))

(defn clean-data
  [data no-data]
  (map #(if (= %1 %2) 0 %1) data no-data))

(defn make-pixel-renderer
  [^BufferedImage raster red green blue no-data]
  (fn [data x y]
    (let [n-data (normalize (clean-data data no-data))
          x (int x)
          y (int y)]
      (.setRGB raster x y (make-pixel [255
                                       (make-red   n-data red)
                                       (make-green n-data green)
                                       (make-blue  n-data blue)])))))

(defn data-seq
  "Takes a sequence of sequences of data and makes a sequence of
  vectors where the nth vector contains the nth element of each data
  sequence."
  [data]
  (lazy-seq
   (if (some (comp not nil?) data)
     (cons (vec (map first data)) (data-seq (map next data)))
     nil)))

(defn render-data
  [files pixel-renderer x y]
  (loop [data (data-seq (map :data files))
         width  (:width (first files))
         height (:height (first files))
         x 0, y 0]
    (cond
     (and (< y (dec height)) (< x width)
          (some nil? (first data)))
     (throw (Exception. (format "Out of data: (%d,%d)" x y)))
   
     (= x width)
     (recur data width height 0 (inc y))
   
     (< y height)
     (do (pixel-renderer (first data) x y)
         (recur (rest data) width height (inc x) y)))))

(defn read-data
  [input]
  (map parse-double
       (tokenize-line (read-lines input))))

(defn read-data-file
  [filename color]
  (let [input (io/make-reader filename {:encoding "UTF-8"})
        nrows (get-nrows (.readLine input))
        ncols (get-ncols (.readLine input))
        _ (.readLine input)
        _ (.readLine input)
        _ (.readLine input)
        no-data (get-nodata (.readLine input))]
    {:width ncols :height nrows :no-data no-data
     :data (read-data input) :color color}))

(defn get-color
  [accessor data]
  (vec (map accessor data)))

(defn get-reds
  [data]
  (get-color #(nth (:color %) 0) data))

(defn get-greens
  [data]
  (get-color #(nth (:color %) 1) data))

(defn get-blues
  [data]
  (get-color #(nth (:color %) 2) data))

(defn get-no-data
  [data]
  (map :no-data data))

(defn usage
  [spec]
  (let [usage-message "Usage: datavis <param-filename> <output-filename>\n\n%s"]
    (case spec
      :params (format usage-message "No param-filename or output-filename specified.")
      :output-file (format usage-message "No output-filename specified."))))

(defn error
  [message]
  (.println *err* message)
  (System/exit 1))

(defn -main
  [& args]
  (let [[params output-file] args
        _ (cond (nil? params) (error (usage :params))
                (nil? output-file) (error (usage :output-file)))
        files (map #(read-data-file (:filename %) (:color %))
                                 (p/get-parameter-files params))]

    (if (not= 1 (count (into #{} (map :width files))))
      (error "Data-files don't all have the same width."))
    (if (not= 1 (count (into #{} (map :height files))))
      (error "Data-files don't all have the same height."))

    (with-open [output (io/output-stream output-file)]
        (let [w (:width (first files))
              h (:height (first files))
              img (BufferedImage. w h BufferedImage/TYPE_4BYTE_ABGR)
              pixel-renderer (make-pixel-renderer img
                                                  (get-reds files)
                                                  (get-greens files)
                                                  (get-blues files)
                                                  (get-no-data files))]

          (render-data files pixel-renderer 0 0)
          (ImageIO/write img "PNG" output)))))