(ns datapipe.gui
  (:require [seesaw.forms :as forms]
            [seesaw.border :as border]
            [clojure.string :as s]
            [datapipe.core :as pipe]
            [datapipe.ops :as ops])
  (:use seesaw.core
        seesaw.chooser))

(native!)

; Input file
(def input-file-label (label "<EMPTY>"))
(def input-file-button (button :text "Input File"))
(def input-file-panel (border-panel :west input-file-button :center input-file-label))

(def input-file (atom nil))

(defn set-input-file [f]
  (reset! input-file (.getAbsolutePath f))
  (config! input-file-label :text (.getName f)))

(defn select-input-file [_]
  (choose-file :type :open :success-fn (fn [fc file] (set-input-file file))))

(listen input-file-button :action select-input-file)


; Output file
(def output-file-label (label "<EMPTY>"))
(def output-file-button (button :text "Output File"))
(def output-file-panel (border-panel :west output-file-button :center output-file-label))

(def output-file (atom nil))


(defn set-output-file [f]
  (reset! output-file (.getAbsolutePath f))
  (config! output-file-label :text (.getName f)))

(defn select-output-file [_]
  (choose-file :type :save :success-fn (fn [fc file] (set-output-file file))))

(listen output-file-button :action select-output-file)

; Source pane
(def initial-source
   "sample 0.001")


(def source-pane (text :id :source :text initial-source :multi-line? true :font "MONOSPACED-PLAIN-14"))

(def source-scroll (scrollable source-pane))


(defn useless?
  "Return if this line of source is useless. That is, it is empty or is a comment."
  [s]
  (let [ts (.trim s)]
    (or (empty? ts)
        (.startsWith ts ";")
        (.startsWith ts "//")
        (.startsWith ts "#"))))

(defn wrap-source-with
  "Wrap each line of source with the prefix and suffix.
  Skips useless lines."
  [prefix suffix source]
  (let [lines (s/split source #"\n")
        source-lines (filter (complement useless?) lines)
        wrapped-lines (map #(str prefix % suffix) source-lines)]
    (s/join "\n" wrapped-lines)))


(defn prepare-source [source]
  (let [wrapped-source (wrap-source-with "  (partial " ")" source)]
    (str "(use 'datapipe.ops)\n(datapipe.core/pipe (datapipe.core/rcomp \n"
         wrapped-source
         "\n) \"" @input-file "\" \"" @output-file "\")")))


(defn eval-source [source]
  (load-string (prepare-source source)))

; Error pane
(def error-pane (text :id :err :text "" :multi-line? true :font "MONOSPACED-PLAIN-11" :editable? false))
(def error-scroll (scrollable error-pane))


(defn error-message [e]
  (cond (nil? e) "nil"
        (string? e) e
        (instance? Throwable e)
        (str
         (.getMessage e)
         "\n\n"
         (s/join "\n" (map str (.getStackTrace e))))
        :else (str e)))

(defn set-error [e]
  (config! error-pane :text (error-message e)))

; Ref panel

(defn gen-ref []
  (s/join "\n\n"
  (for [[name v] (sort (ns-publics 'datapipe.ops))]
    (str name "\n"
         (first (:arglists (meta v))) "\n"
         (-> v meta :doc)))))

(def ref-panel (text :text (gen-ref) :multi-line? true :editable? false))
(def ref-scroll (scrollable ref-panel))

; Table
(def data-table (table))
(def data-table-scroll (scrollable data-table))


(defn data-table-set-file [f]
  (let [[headers rows] (pipe/preview-file f)]
    (config! data-table :model [:columns headers :rows rows])))

; Run button
(defn do-run [_]
  (let [source (value source-pane)
        start-time (System/currentTimeMillis)]
    (try
      (do
        (eval-source source)
        (set-error (format "Done in %.2f seconds." (float (/ (- (System/currentTimeMillis) start-time) 1000))))
        (data-table-set-file @output-file))
      (catch Exception e (set-error e)))))

(def run-button (button :text "Run"))
(listen run-button :action do-run)

(defonce f (frame :title "DataPipe"))

(defn -main []
  (config! f :content (top-bottom-split
                       (border-panel
                        :border (border/empty-border :thickness 10)
                        :north (vertical-panel :items [input-file-panel output-file-panel])
                        :center (left-right-split
                                 (top-bottom-split
                                  source-scroll
                                  error-scroll
                                  :divider-location 0.8 :border nil)
                                 ref-scroll
                                 :divider-location 0.5 :border nil)
                        :south run-button)
                       data-table-scroll :divider-location 0.9 :border nil)
           :size [1000 :by 700])
  (show! f))

; (-main)
