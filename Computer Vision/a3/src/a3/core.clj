(ns a3.core
  (:gen-class)
  (:import [java.awt.image BufferedImage])
  (:import [javax.imageio ImageIO])
  (:import [java.io File])
  (use [clojure.string :only (split)]))

; COMP316-18A ASSIGNMENT THREE COMPUTER VISION
; SACHA RAMAN | 1288048

; The read-image, save-image, get-width and get-height functions is not my code.
; My set-rgb and get-intensity function is based on the get-rgb function
; Iterating through all pixels in an image using dotimes is also not my code.
; They are from the functions provided to us in the Clojure Image Processing document.

;---------------------------------------------------------------------------------------------------------------------------------

(defn read-image
	"Read in an image into BufferedImages"
	[filename]
	(let [file (File. filename)]
		(ImageIO/read file)
	)
)

;---------------------------------------------------------------------------------------------------------------------------------

(defn save-image
   "Saves an image to files"
	[image extension filename]
	(let [file (File. filename)]
	   (ImageIO/write image extension file)
	)
)

;---------------------------------------------------------------------------------------------------------------------------------

(defn get-width
  "Gets the width of an image"
  [image]
  (.getWidth image)
)

;---------------------------------------------------------------------------------------------------------------------------------

(defn get-height
  "Get the height of an image"
  [image]
  (.getHeight image)
)

;---------------------------------------------------------------------------------------------------------------------------------

(defn get-intensity_
  "Get the 8bit intensity (gray) value"
  [image x y]
  (let [rgb (.getRGB image x y)
        ; since each r g b value is identical, we only need to get one of these
        intensity (bit-and rgb 0xFF)]
    intensity
   )
)

; memoize the get-intensity_ function
(def get-intensity (memoize get-intensity_))

;---------------------------------------------------------------------------------------------------------------------------------

(defn set-rgb_
  "set rgb to the input value"
  [image x y value]
  (let [red (bit-shift-left value 16)
  		  green (bit-shift-left value 8)
		  blue value]
		(.setRGB image x y (bit-or red green blue))
	)
)

; memoize the set-rgb_ function
(def set-rgb (memoize set-rgb_))

;---------------------------------------------------------------------------------------------------------------------------------

(defn binning
	"put a value into the appropriate bin"
	[value bins pixels]
	(int (* value (/ bins pixels)))
)

;---------------------------------------------------------------------------------------------------------------------------------

; In this version, it gets the appropriate kirsch filter by rotating the orginal kirsh filter using a loop
; I have decided not to use this version as the other version does the exact same thing in less time
(defn get-kirsch_
  "gets the neighboring intensity values and applies the specified kirsch filter to them"
  [image x1 y1 i]
  ;iterate through each of the surrounding pixels and get each of their intensities
  (let [intensities (vec (for [x [(- x1 1) x1 (+ x1 1)] y [(- y1 1) y1 (+ y1 1)]] (get-intensity image x y)))
        ;get the specified kirsch-filter by rotating H0 by the number of times specified by i
        k (loop [x i kirsch-filter [-1 -2 -1 0 1 2 1 0]]
                     (if (< x 1)
                       kirsch-filter
                       ;rotate the kirsch filter by 1
                       (recur (- x 1) (conj (vec (rest kirsch-filter)) (first kirsch-filter)))))
        ;put the filter in the same order as the vector of intensities
        kirsch-filter [(get k 0) (get k 1) (get k 2) (get k 7) 0 (get k 3) (get k 6) (get k 5) (get k 4)]
        ;multiply the two vectors
        kirsch-val (+ (int (reduce + (vec (map * intensities kirsch-filter)))) 127)]
    ;return kirsch value - if the kirsch value is <0 then return 0, if >255 return 255 (clamping)
    (cond (< kirsch-val 0) 0 (> kirsch-val 255) 255 :else kirsch-val))
)

;---------------------------------------------------------------------------------------------------------------------------------

(defn rotation
  "gets the specified rotation of the kirsch filter"
  [i]
  (get [[-1 -2 -1 0 0 0 1 2 1][-2 -1 0 -1 0 1 0 1 2][-1 0 1 -2 0 2 -1 0 1][0 1 2 -1 0 1 -2 -1 0]
        [1 2 1 0 0 0 -1 -2 -1][2 1 0 1 0 -1 0 -1 -2][1 0 -1 2 0 -2 1 0 -1][0 -1 -2 1 0 -1 2 1 0]] i)
)

;---------------------------------------------------------------------------------------------------------------------------------

(defn get-kirsch
  "gets the neighboring intensity values and applies the specified kirsch filter to them"
  [image x1 y1 i]
  ;iterate through each of the surrounding pixels and get each of their intensities
  (let [intensities (vec (for [x [(- x1 1) x1 (+ x1 1)] y [(- y1 1) y1 (+ y1 1)]] (get-intensity image x y)))
        ;get the kirsch filter specified for the pixel at x1 y1
        kirsch-val (+ (int (reduce + (vec (map * intensities (rotation i))))) 127)]
    ;return kirsch value - if the kirsch value is <0 then return 0, if >255 return 255 (clamping)
    (cond (< kirsch-val 0) 0 (> kirsch-val 255) 255 :else kirsch-val))
)

;---------------------------------------------------------------------------------------------------------------------------------

(defn kirsch
  "applies one of the eight kirsch filters to a gray-scale image"
  [file i]
  ;read int the image and get its width and heigh
  (let [image (read-image file)
        new-image (read-image file)
        image-width (get-width image)
        image-height (get-height image)
        filename (get (split file #"\.") 0)
        filetype (get (split file #"\.") 1)]
    ;go through each pixel
    (dotimes [x image-width]
      (dotimes [y image-height]
        ;offset the start x and y by 1 so we dont exceed boundaries
        (let [x1 (+ x 1)
              y1 (+ y 1)
              ;also take off the width and height so we dont exceed boundaries
              w1 (- image-width 1)
              h1 (- image-height 1)]
          ;give the image a black border
          (if (or (= x 0) (= y 0) (= x w1) (= y h1))
            (set-rgb new-image x y 0)
          )
          ;if we are within these new bounds
          (if (and (< x1 w1)
                   (< y1 h1))
            ;call the kirsch filter and set this as the new color value for the pixel
            (set-rgb new-image x1 y1 (get-kirsch image x1 y1 i))
          )
          )))
    ;save the image
    (save-image new-image filetype (str filename "_kirsch"))
  )
)

;---------------------------------------------------------------------------------------------------------------------------------

(defn normalize-hist
  "takes a histogram and returns a normalized histogram"
  [hist]
  (let [values (count hist)
        hist-sum (reduce + hist)]
    (vec (map #(double (/ % hist-sum)) hist))
  )
)

;---------------------------------------------------------------------------------------------------------------------------------

(defn edge-magnitude-hist
  "returns a normalised frequency histogram with 8 bins containing edge magnitude"
  [file]
  ;read in the image and get its width and heigh
  (let [image (read-image file)
        image-width (get-width image)
        image-height (get-height image)
        ;for every pixel in the image, get the 8 kirsch filters and then get the max of those
        max-k (vec (for [x (range 1 (- image-width 1)) y (range 1 (- image-height 1))]
					(apply max (vec (map #(get-kirsch image x y %) (range 8))))))
        ;for each max kirsch value, find its designated bin. Then for each of the 8 bins, count up
        ;how many are in the respective bin and display as a vector of 8 bins
		    bin (vec (map (fn[x] (count (filter #(= % x) (map #(binning % 8 256) max-k)))) (range 8)))]
    ;normalize the histogram
    (normalize-hist bin)
  )
)

;---------------------------------------------------------------------------------------------------------------------------------

(defn edge-direction-hist
  "which returns a normalised direction histogram with 8 bins containing edge direction"
  [file]
  ; read int the image and get its width and heigh
  (let [image (read-image file)
        image-width (get-width image)
        image-height (get-height image)
        ; for every pixel in the image, get the 8 kirsch filters and then get the index of the max of those
        index-max-k (vec (for [x (range 1 (- image-width 1)) y (range 1 (- image-height 1))]
					(.indexOf (vec (map #(get-kirsch image x y %) (range 8))) (apply max (vec (map #(get-kirsch_ image x y %) (range 8)))) ) ))
        ; how many are in the respective bin and display as a vector of 8 bins
		    bin (vec (map (fn[x] (count (filter #(= % x) (map #(binning % 8 8) index-max-k)))) (range 8)))]
    ; normalize the histogram
    (normalize-hist bin)
  )
)

;---------------------------------------------------------------------------------------------------------------------------------

(defn intensity-hist
  "gets 8 bin intensity histogram for a grayscale image"
  [file]
  ; get the image and its width and heigh
  (let [image (read-image file)
        image-width (get-width image)
        image-height (get-height image)
        ; iterate over each pixel
        intensity (vec (for [x (range image-width) y (range image-height)] (get-intensity image x y)))
        ; how many are in the respective bin and display as a vector of 8 bins
        bin (vec (map (fn[x] (count (filter #(= % x) (map #(binning % 8 256) intensity)))) (range 8)))]
    ; normalize the histogram
    (normalize-hist bin)
  )
)

;---------------------------------------------------------------------------------------------------------------------------------

(defn image-descriptor
  "returns a descriptor for a given image containing the
  edge-direction, magnitude and intensity historgrams"
  [file]
  (let [edge (edge-direction-hist file)
        magnitude (edge-magnitude-hist file)
        intensity (intensity-hist file)
        descriptor (into [] (concat edge magnitude intensity))]
    (normalize-hist descriptor)
  )
)

;---------------------------------------------------------------------------------------------------------------------------------

(defn image-similarity
  "takes two image files and returns a value between 0.0 and 1.0
  indicating how similar two images are using the image descriptor"
  [file1 file2]
  (let [d1 (image-descriptor file1)
        d2 (image-descriptor file2)
        ; compare the two vectors and get the min of each value
        compare-d1-d2 (vec (map #(min %1 %2) d1 d2))]
    ; add them all up to get a values between 0 - 1
    (reduce + compare-d1-d2)
  )
)

;---------------------------------------------------------------------------------------------------------------------------------

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
