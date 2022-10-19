(ns importer
  (:require [clojure.set :as cs]
            [clojure.xml :as xml]
            [refdata :as refdata])
  (:import (java.io ByteArrayInputStream)))

;(def interesting-tags #{:Class :Pos :PIC :Classified :Driver})
;
;(defn keep-interesting [result-tag]
;  (filter
;    (fn[subtag]
;      (contains? interesting-tags (get subtag :tag)))
;    result-tag))

(defn content-from
  [tag tags]
  (:content
    (first
      (filter #(= tag (:tag %)) tags))))

(def nutty-bool
  {"YES" true
   "NO" false})

(defn -main
  [& args]
  (let [known-drivers (cs/map-invert (refdata/all-drivers (second *command-line-args*)))
        race-id (nth args 2)
        body (slurp (first *command-line-args*))
        root (xml-seq (xml/parse (ByteArrayInputStream. (.getBytes body))))
        items (map #(% :content) (filter (comp #{:Result} :tag) root))
        ;items-stripped (map #(keep-interesting %) items)

        lines (map
               (fn [tags]
                 (let [
                       clazz (name (get refdata/class-aliases (clojure.string/lower-case (first (content-from :Class tags)))))
                       pos (Integer/parseInt (first(content-from :PIC tags)))
                       points (refdata/pos-to-points pos)
                       classified (get nutty-bool (first(content-from :Classified tags)))
                       driver (known-drivers (first (content-from :Name (content-from :Driver tags))))
                       ]
                   {:class clazz
                    :points points
                    :pos pos
                    :classified classified
                    :driver (if (nil? driver) "????" (name driver))}
                   )
                 )
               items)]
      (doseq [line lines]
        (println (str race-id "," (:class line) "," (:driver line) "," (:pos line) "," (:points line))))))

