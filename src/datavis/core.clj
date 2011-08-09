(ns datavis.core
  (:import [java.awt Transparency]
           [java.awt.image
            DataBuffer
            BufferedImage
            WritableRaster
            ComponentColorModel]
           [java.awt.color ColorSpace]
           [javax.imageio ImageIO])
  (:require [clojure.java [io :as io]]
            [clojure [string :as str]])
  (:gen-class))

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
  (+ (bit-shift-left (bit-and (int (* a 255)) 0xff) 24)
     (bit-shift-left (bit-and (int (* r 255)) 0xff) 16)
     (bit-shift-left (bit-and (int (* g 255)) 0xff) 8)
     (bit-and (int (* b 255)) 0xff)))

(defn make-pixel-renderer
  [^BufferedImage raster red green blue no-data]
  (let [red (double red)
        green (double green)
        blue (double blue)
        no-data (double no-data)]
    (fn [data x y]
      (let [data (double data)
            x (int x)
            y (int y)]
        (if (= data no-data)
          (.setRGB raster x y (make-pixel [0.0 0.0 0.0 0.0]))
          (.setRGB raster x y (make-pixel [1.0
                                           (* data red)
                                           (* data green)
                                           (* data blue)])))))))

(defn render-data
  ([data width height pixel-renderer x y]
     (cond
      (and (< y (dec height)) (< x width) (nil? data))
      (throw (Exception. (format "Out of data: (%d,%d)" x y)))
      
      (= y height)
      (recur data width height pixel-renderer (inc x) 0)

      (< x width)
      (do (pixel-renderer (first data) x y)
          (recur (next data) width height pixel-renderer x (inc y))))))

(defn parse-double
  [s]
  (Double. s))

(defn read-lines
  [input]
  (lazy-seq
   (if-let [line (.readLine input)]
     (cons line (read-lines input))
     ())))

(defn read-data
  [input]
  (map parse-double
       (remove #(= "" %)
               (mapcat #(str/split % #"\s")
                       (read-lines input)))))

(defn -main
  [input-file output-file red green blue]
   (let [input (io/make-reader input-file {:encoding "UTF-8"})]
      (with-open [output (io/output-stream output-file)]
        (let [ncols (get-ncols (.readLine input))
              nrows (get-nrows (.readLine input))
              _ (.readLine input)
              _ (.readLine input)
              _ (.readLine input)
              no-data (get-nodata (.readLine input))
              img (BufferedImage. ncols nrows BufferedImage/TYPE_4BYTE_ABGR)
              pixel-renderer (make-pixel-renderer
                              img (Double. red) (Double. green) (Double. blue) no-data)]

          (render-data (read-data input) ncols nrows pixel-renderer 0 0)
          (ImageIO/write img "PNG" output)))))

