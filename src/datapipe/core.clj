(ns datapipe.core
  (require [clojure.java.io :as io]
           [clojure.string :as s]))

; File filter operation


(defn csv-header
  "Convert a list of strings/keywords to a CSV header."
  [headers]
  (s/join "," (map name headers)))

(defn split-csv-rows [headers coll]
  (map #(zipmap headers (s/split % #",")) coll))

(defn join-csv-rows [rows]
  (let [headers (vec (keys (first rows)))]
    (conj (map (fn [row]
                 (let [vals (map row headers)]
                   (s/join "," vals))) rows) (csv-header headers))))

(defn pipe
  "Filter the input file using the op filter and write to an output file."
  [op in-file out-file]
  (with-open [r (io/reader in-file)
              w (io/writer out-file)]
    (let [headers (map keyword (s/split (first (line-seq r)) #","))]
      (doseq [line (join-csv-rows (op (split-csv-rows headers (line-seq r))))]
        (.write w (str line "\n"))))))

(defn preview-file
  "Show a preview of the file by reading the first n lines."
  ([f] (preview-file f 100))
  ([f n]
   (with-open [r (io/reader f)]
     (let [headers (vec (s/split (first (line-seq r)) #","))
           rows (vec (take n (split-csv-rows headers (line-seq r))))]
       [headers rows]))))


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


;(pipe (rcomp only-letters (partial freqs "key")) "/Users/fdb/Desktop/keys.csv" "/Users/fdb/Desktop/k.csv")


;(freqs :key [{:key "a"} {:key "b"} {:key "c"} {:key "a"} {:key "a"} {:key "b"}])

(comment
(pipe (rcomp
       (partial datapipe.ops/sample 0.01)
       (partial datapipe.ops/scale :x 0.1)
       (partial datapipe.ops/scale :y 0.1)
       (partial datapipe.ops/freqs :x :y)
       )
      "/Users/fdb/Desktop/mouse.csv" "/Users/fdb/Desktop/m.csv")
)


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
