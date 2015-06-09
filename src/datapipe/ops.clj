(ns datapipe.ops
  (require [clojure.java.io :as io]
           [clojure.string :as s]))

(set! *warn-on-reflection* true)

(defn- average-by-group
  [key group]
  (let [vals (map (comp #(Float/parseFloat (str %)) key) group)]
    (/ (reduce + vals) (count group))))

(defn- grouping-fn
  [grouper]
  (if (keyword? grouper)
    grouper
    #(hash (map % grouper))))

(defn- group-keys
  [grouper group]
  (if (keyword? grouper)
    {grouper ((first group) grouper)}
    (zipmap grouper (map (first group) grouper))))

(defn average
  "Calculate average values by grouping on a column. Column key can be a list.
  Example: average :age :name"
  [key grouper coll]
  (let [groups (group-by (grouping-fn grouper) coll)]
    (map (fn [[group-key group]]
           (let [avg (average-by-group key group)]
             (merge (group-keys grouper group) {key avg}))) groups)))

(defn sample
  "Reduce the size of the file by randomly selecting lines from the file.
  The percentage is a value between 0 and 1. At 0, no rows are kept.
  At 1, all rows are kept. At 0.5, about 50% of all rows are kept.
  Example: sample 0.01"
  [percentage coll]
  (filter (fn [x] (< (Math/random) percentage)) coll))


(defn select
  "Select the rows where the selector matches.
  Example: select :key \"A\"
  Example: select :key [\"a\" \"b\" \"c\"]
  Example (select only word characters): select :key #\"\\w\""
  [key selector coll]
  (let [pred
        (cond (string? selector)
              #(= (str (% key)) selector)
              (sequential? selector)
              #(some (set [(% key)]) selector)
              (instance? java.util.regex.Pattern selector)
              #(re-matches selector (str (% key)))
              )]
    (filter pred coll)))

(defn lookup
  "Keep only the given column.
  Example: lookup :time"
  [key coll]
  (map (fn [row] {key (get row key)}) coll))

(defn equals
  "Keep only the rows where the key in the collection matches the value.
  Example: equals :key \"A\""
  [key val coll]
  (filter #(= (% key) val) coll))

(defn- freq-map
  "Count the occurences of a value for the given key
  and return a map with key=count."
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
  "Count the number of time a value in the given column occurs.
  You can also specify two columns; in this case both values have to match.
  Example: freqs :x :y"
  ([key coll]
  (let [fm (freq-map key coll)]
    (map (fn [[kv amt]] {key kv :amount amt}) fm)))
  ([k1 k2 coll]
   (let [fm (freq-map k1 k2 coll)]
     (map (fn [[[kv1 kv2] amt]] {k1 kv1 k2 kv2 :amount amt}) fm))))


(defn- parse-float
  "Convert a string to a floating point number.
  If the string is not a number, return 0, or a default value
  of your choosing.
  If the value is already a number, it is returned as-is."
  ([^String s] (parse-float s 0.0))
  ([^String s default]
   (if (number? s)
     s
     (try
       (Float/valueOf s)
       (catch Exception e default)))))

(defn scale
  "Multiply the values in the given column.
  The column needs to contain numbers.
  Numbers are rounded to the nearest integer.
  Example: scale :x 0.1"
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
            {:x x2 :y y2
             :distance (+ dist (geo-distance x1 y1 x2 y2))})) coll)])

(defn add-column
  "Add a new column with the name col-name.
  For each row, call the function on the values for the key.
  Example: add-column :time :week time-week
  Example: add-column :x :plus-one inc"
  [key col-name f coll]
  (pmap #(assoc % col-name (f (get % key))) coll))


(defn words
  "Try to find words.
  Example: words :key"
  [key coll]
  (let [groups (partition-by (fn [row] (not (nil? (re-matches #"\w" (get row key ""))))) coll)]
    (for [g groups]
      (assoc (first g) :word  (apply str (map #(get % key) g))))))

;; Time

(def ^{:private true} df (java.text.SimpleDateFormat. "yyyy-MM-dd HH:mm:ss.SSS"))

(defn- to-calendar ^java.util.Calendar [s]
  (let [^java.text.SimpleDateFormat format df]
    (try
      (doto (java.util.Calendar/getInstance)
        (.setTime (.parse format s)))
      (catch Exception e nil))))

(defn time-year
  "Get the year of the given date.
  This is useful when combined with add-column.
  Example: add-column :time :year time-year"
  [date]
  (let [cal (to-calendar date)]
    (if-not (nil? cal)
      (.get cal java.util.Calendar/YEAR)
      0)))

(defn time-month
  "Get the month of the given date.
  This is useful when combined with add-column.
  Example: add-column :time :month time-month"
  [date]
  (let [cal (to-calendar date)]
    (if-not (nil? cal)
      ; .getMonth starts from 0, which is stupid.
      (inc (.get cal java.util.Calendar/MONTH))
      0)))

(defn time-date
  "Get the day of the month of the given date.
  This is useful when combined with add-column.
  Example: add-column :time :date time-date"
  [date]
  (let [cal (to-calendar date)]
    (if-not (nil? cal)
      (.get cal java.util.Calendar/DATE)
      0)))

(defn time-dow
  "Get the day of the week for the given date.
  This is useful when combined with add-column.
  Example: add-column :time :dow time-dow"
  [date]
  (let [cal (to-calendar date)]
    (if-not (nil? cal)
      (.get cal java.util.Calendar/DAY_OF_WEEK)
      0)))

(defn time-doy
  "Get the day of the year for the given date.
  This is useful when combined with add-column.
  Example: add-column :time :doy time-doy"
  [date]
  (let [cal (to-calendar date)]
    (if-not (nil? cal)
      (.get cal java.util.Calendar/DAY_OF_YEAR)
      0)))

(defn time-hour
  "Get the hour for the given date.
  This uses the 24-hour clock.
  This is useful when combined with add-column.
  Example: add-column :time :hour time-hour"
  [date]
  (let [cal (to-calendar date)]
    (if-not (nil? cal)
      (.get cal java.util.Calendar/HOUR_OF_DAY)
      0)))

(defn time-minute
  "Get the minute for the given date.
  This is useful when combined with add-column.
  Example: add-column :time :minute time-minute"
  [date]
  (let [cal (to-calendar date)]
    (if-not (nil? cal)
      (.get cal java.util.Calendar/MINUTE)
      0)))

(defn time-second
  "Get the second for the given date.
  This is useful when combined with add-column.
  Example: add-column :time :second time-second"
  [date]
  (let [cal (to-calendar date)]
    (if-not (nil? cal)
      (.get cal java.util.Calendar/SECOND)
      0)))

(defn time-week
  "Get the week number for the given date.
  This is useful when combined with add-column.
  Example: add-column :time :week time-week"
  [date]
  (let [cal (to-calendar date)]
    (if-not (nil? cal)
      (.get cal java.util.Calendar/WEEK_OF_YEAR)
      0)))
