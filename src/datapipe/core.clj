(ns datapipe.core
  (require [clojure.java.io :as io]
           [clojure.string :as s]))

; Filter operations
(defn sample
  "Keep only a number of elements, selected randomly."
  [percentage coll]
  (filter (fn [x] (< (Math/random) percentage)) coll))

; Lookup is harder since we join-csv-row takes the same headers in.
; We could change this if join-csv-row looked at the first row for the
; header, but this would require holding on to the head?
(defn lookup
  "Keep only the given column."
  [key coll]
  (map (fn [row] {key (get row key)}) coll))

; Keep only the ones where x = given x value.
; You could expand this where the value should fit a certain bounding box.
; Some bounding boxes on the screen are relevant, e.g. the menu bar.
(defn x-pos [x coll]
  (filter  #(= x (% "x")) coll))


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

(defn freqs
  "Count the occurences of a value for the given key."
  [key coll]
  ; Wrap this in a list of one element so we can use it for join-csv-rows.
  [(reduce
     (fn [cnts row]
         (let [v (get row key)]
           (assoc cnts v (inc (get cnts v 0)))))
   {}
   coll)])


; File filter operation

(defn split-csv-rows [headers coll]
  (map #(zipmap headers (s/split % #",")) coll))

(defn join-csv-rows [rows]
  (let [headers (vec (keys (first rows)))]
    (conj (map (fn [row]
                 (let [vals (map row headers)]
                   (s/join "," vals))) rows) (s/join "," headers))))

(defn pipe
  "Filter the input file using the op filter and write to an output file."
  [op in-file out-file]
  (with-open [r (io/reader in-file)
              w (io/writer out-file)]
    (let [headers (s/split (first (line-seq r)) #",")]
      (doseq [line (join-csv-rows (op (split-csv-rows headers (line-seq r))))]
        (.write w (str line "\n"))))))


(defn stats
  "Go over the input file and keep statistics."
  [f val in-file]
  (with-open [r (io/reader in-file)]
    (let [headers (s/split (first (line-seq r)) #",")]
      (reduce f val  (split-csv-rows headers (line-seq r))))))

(defn rcomp
  "Reverse compose. First operation is done first."
  [& fs]
  (apply comp (reverse fs)))

;(pipe (comp (partial sample 0.01) (partial lookup "x")) "/Users/fdb/Desktop/mouse.csv" "/Users/fdb/Desktop/m.csv")


(pipe (rcomp only-letters (partial freqs "key")) "/Users/fdb/Desktop/keys.csv" "/Users/fdb/Desktop/k.csv")
;(comment
;(pipe
; (comp
;  (partial row-eq "key" (Character/toString (char 13)))
;  (partial random-sample 0.9)
;  )
; "keys.csv" "k.csv")


;(def freqs (stats (fn [cnts row]
;         (let [key (get row "key")]
;           (assoc cnts key (inc (get cnts key 0)))))
;       {}
;       "keys.csv"))

;(reverse (sort-by #(get % 1) (vec freqs))))
