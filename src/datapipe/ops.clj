(ns datapipe.ops
  (require [clojure.java.io :as io]
           [clojure.string :as s]))

; Filter operations
(defn sample
  "Keep only a number of elements, selected randomly."
  [percentage coll]
  (filter (fn [x] (< (Math/random) percentage)) coll))


(defn select
  "Select a set of keys."
  [key selector coll]
  (let [pred
        (cond (string? selector) #(= (% key))
              (sequential? selector) #(some (set [(% key)]) selector)
              (instance? java.util.regex.Pattern selector) #(re-matches selector (str (% key)))
              )]
    (filter pred coll)))

(defn lookup
  "Keep only the given column."
  [key coll]
  (map (fn [row] {key (get row key)}) coll))

; Keep only the ones where (key coll) = val.
; Inputs are strings, so values should also be strings.
(defn equals [key val coll]
  (filter #(= (% key) val) coll))

(defn only-letters
  "Filter the keys with only letters."
  [coll]
  (filter (fn [row]
            (let [val (get row "key")]
              (and
               (not (nil? val))
               (re-matches #"\w" (get row "key")))))
          coll))

(defn freq-map
  "Count the occurences of a value for the given key and return a map with key=count."
  ([key coll]
  (reduce
     (fn [cnts row]
         (let [v (get row key)]
           (assoc cnts v (inc (get cnts v 0)))))
   {}
   coll))
  ([k1 k2 coll]
    (reduce
     (fn [cnts row]
         (let [kv1 (get row k1) kv2 (get row k2)]
           (assoc cnts [kv1 kv2] (inc (get cnts [kv1 kv2] 0)))))
   {}
   coll)))

(defn freqs
  "Count the occurences of a value for the given key and return a list with key,count."
  ([key coll]
  (let [fm (freq-map key coll)]
    (map (fn [[kv amt]] {key kv :amount amt}) fm)))
  ([k1 k2 coll]
   (let [fm (freq-map k1 k2 coll)]
     (map (fn [[[kv1 kv2] amt]] {k1 kv1 k2 kv2 :amount amt}) fm))))


(defn parse-float
  ([s] (parse-float s 0.0))
  ([s default]
   (if (number? s)
     s
     (try
       (Float/valueOf s)
       (catch Exception e default)))))

(defn scale
  "Reduce the size of the input keys. This assumes the keys are numbers."
  [key scale coll]
  (map (fn [row]
         (let [v (parse-float (get row key ""))
               new-v (Math/round (* v (float scale)))]
           (assoc row key (str new-v)))) coll))

(defn- geo-distance
  "Calculate the Euclidian distance between two points."
  [x1 y1 x2 y2]
  (Math/sqrt
   (+
    (Math/pow (- x2 x1) 2)
    (Math/pow (- y2 y1) 2))))

(defn distance
  "Calculate the total euclidian distance between x/y coordinates.
  This assumes the file has columns x and y which contain numbers."
  [coll]
  [(reduce (fn [pt1 pt2]
          (let [x1 (parse-float (pt1 :x))
                y1 (parse-float (pt1 :y))
                x2 (parse-float (pt2 :x))
                y2 (parse-float (pt2 :y))
                dist (get pt1 :distance 0)]
            {:x x2 :y y2 :distance (+ dist (geo-distance x1 y1 x2 y2))})) coll)])


;(def pts [{:x 0 :y 0} {:x 10 :y 10} {:x 20 :y 20}])
;(distance pts)


