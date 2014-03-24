(ns datapipe.gui
  (:require [seesaw.forms :as forms]
            [seesaw.border :as border]
            [clojure.string :as s]
            [datapipe.core :as pipe])
  (:use seesaw.core
       seesaw.chooser)
  (:gen-class))


(native!)
(defonce f (frame :title "DataPipe"))

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
    (str "(use 'datapipe.core)\n(datapipe.core/pipe (datapipe.core/rcomp \n"
         wrapped-source
         "\n) \"" @input-file "\" \"" @output-file "\")")))


(defn eval-source [source]
  (load-string (prepare-source source)))

; Error pane
(def error-pane (text :id :err :text "" :multi-line? true :font "MONOSPACED-PLAIN-11" :editable? false))
(def error-scroll (scrollable error-pane))


(defn error-message [e]
  (if (nil? e)
    "OK"
  (str
   (.getMessage e)
   "\n\n"
   (s/join "\n" (map str (.getStackTrace e))))))

(defn set-error [e]
  (config! error-pane :text (error-message e)))

; Run button
(defn do-run [_]
  (let [source (value (select f [:#source]))]
    (try
      (do (eval-source source)
        (set-error nil))
      (catch Exception e (set-error e)))))

(def run-button (button :text "Run"))
(listen run-button :action do-run)


(config! f :content
         (border-panel
          :border (border/empty-border :thickness 10)
          :north (vertical-panel :items [input-file-panel output-file-panel])
          :center (top-bottom-split source-scroll error-scroll :divider-location 0.7)
          :south run-button))


;(config! f :size  [800 :by 600])
;(-> f show!)

(defn -main []
  (config! f :size  [800 :by 600])
  (-> f show!))


