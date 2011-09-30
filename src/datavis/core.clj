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
  [[d1 d2 d3 :as data]]
  (let [s (reduce + data)]
    (if (zero? s)
      [0 0 0]
      [(/ d1 s) (/ d2 s) (/ d3 s)])))

(defn clean-data
  [data no-data]
  (map #(if (= % no-data) 0 %) data))

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



(defn render-data
  [file1 file2 file3 pixel-renderer x y]
  {:pre [(= (:width file1) (:width file2) (:width file3))
         (= (:height file1) (:height file2) (:height file3))]}
  (loop [data1 (:data file1)
         data2 (:data file2)
         data3 (:data file3)
         width (:width file1)
         height (:height file1)
         x 0 y 0]
    (cond
     (and (< y (dec height)) (< x width)
          (or (nil? data1) (nil? data2) (nil? data3)))
     (throw (Exception. (format "Out of data: (%d,%d)" x y)))
   
     (= x width)
     (recur data1 data2 data3 width height 0 (inc y))
   
     (< y height)
     (do (pixel-renderer [(first data1) (first data2) (first data3)] x y)
         (recur (next data1) (next data2) (next data3) width height (inc x) y)))))

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

(defn read-data-file
  [filename color]
  (let [input (io/make-reader filename {:encoding "UTF-8"})
        ncols (get-ncols (.readLine input))
        nrows (get-nrows (.readLine input))
        _ (.readLine input)
        _ (.readLine input)
        _ (.readLine input)
        no-data (get-nodata (.readLine input))]
    {:width ncols :height nrows :no-data no-data
     :data (read-data input) :color color}))

(defn get-color
  [accessor data-set-1 data-set-2 data-set-3]
  (#(vector (accessor %1) (accessor %2) (accessor %3))
   data-set-1 data-set-2 data-set-3))

(defn get-reds
  [data-set-1 data-set-2 data-set-3]
  (get-color #(nth (:color %) 0) data-set-1 data-set-2 data-set-3))

(defn get-greens
  [data-set-1 data-set-2 data-set-3]
  (get-color #(nth (:color %) 1) data-set-1 data-set-2 data-set-3))

(defn get-blues
  [data-set-1 data-set-2 data-set-3]
  (get-color #(nth (:color %) 2) data-set-1 data-set-2 data-set-3))

(defn get-no-data
  [data-set-1 data-set-2 data-set-3]
  (if (not (= (:no-data data-set-1)
              (:no-data data-set-2)
              (:no-data data-set-3)))
    (throw (Exception. "Data-files don't all have the same \"no-data\" value.")))
  (:no-data data-set-1))

(defn render-file
  [input-file red green blue]
  (let [input (io/make-reader input-file {:encoding "UTF-8"})]
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
      img)))

(defn -main
  [input1 r1 g1 b1 input2 r2 g2 b2 input3 r3 g3 b3 output-file]
  (let [file1 (read-data-file input1 (vec (for [x [r1 g1 b1]] (Integer. x))))
        file2 (read-data-file input2 (vec (for [x [r2 g2 b2]] (Integer. x))))
        file3 (read-data-file input3 (vec (for [x [r3 g3 b3]] (Integer. x))))]

    (if (not (= (:width file1) (:width file2) (:width file3)))
      (throw (Exception. "Data-files don't all have the same width.")))
    (if (not (= (:height file1) (:height file2) (:height file3)))
      (throw (Exception. "Data-files don't all have the same height.")))

    (with-open [output (io/output-stream output-file)]
        (let [w (:width file1)
              h (:height file2)
              img (BufferedImage. w h BufferedImage/TYPE_4BYTE_ABGR)
              pixel-renderer (make-pixel-renderer img
                                                  (get-reds file1 file2 file3)
                                                  (get-greens file1 file2 file3)
                                                  (get-blues file1 file2 file3)
                                                  (get-no-data file1 file2 file3))]

          (render-data file1 file2 file3 pixel-renderer 0 0)
          (ImageIO/write img "PNG" output)))))

