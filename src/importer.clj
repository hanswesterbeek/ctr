(ns importer
  (:require [clojure.set :as cs]
            [clojure.xml :as xml]
            [refdata :as refdata]
            [clojure.test :as t])
  (:import (java.io ByteArrayInputStream)))

(defn sanitize-driver
  [name]
  (apply (-> name
             (clojure.string/lower-case)
             (clojure.string/replace " " "")
             )
         (filter #(Character/isLetter %))))

(t/deftest hmm
  (t/testing "foo"
    (t/is (= "hanswesterbeek") (sanitize-driver " Han s Wèsterbeek"))))


(defn drivers-sanitized
  [drivers]
  (into {}
        (map
          (fn [[k v]]
            [ k (sanitize-driver v)])
          drivers)))

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
                       clazz-raw (first (content-from :Class tags))
                       clazz (name (get refdata/class-aliases (clojure.string/lower-case clazz-raw)))
                       pos (Integer/parseInt (first(content-from :PIC tags)))
                       points (refdata/pos-to-points pos)
                       classified (get nutty-bool (first(content-from :Classified tags)))
                       driver-raw (first (content-from :Name (content-from :Driver tags)))
                       driver (or (refdata/driver-aliases driver-raw) (known-drivers driver-raw))
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

