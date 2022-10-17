(ns ctr
  (:require [clojure.data.csv :as csv]
           [clojure.java.io :as io])
  )

(defn csv-data->maps [csv-data]
  (map zipmap
       (->> (first csv-data)
            (map keyword)
            repeat)
       (rest csv-data)))

(def races-to-count-by-num-participations
  [:13 13, :14 13, :15 13, :16 13 :17 13
   :18 14,
   :19 15,
   :20 16, :21 16,
   :22 17,
   :23 18,
   :24 18,
   :25 19,
   :26 20])

(defn scores [driver rclass facts]
  ; returns a list of scores like [25 18 23] in descending order
  (sort >
    (map
      (fn [row]
        (:points row))
      (filter
        #(and
           (= (:driver %) driver)
           (= (:rclass %) rclass)
           )
        facts))))


(defn -main
  []
  ; for every class, gather every driver
  ; for every combination of driver and class, gather their results

  (with-open [reader (io/reader (io/resource "results.csv"))]
    (let [raw (csv-data->maps (csv/read-csv reader))
          facts (map
                  (fn [row]
                     {:points (Integer/parseInt (:points row))
                      :driver (clojure.string/lower-case (:driver row))
                      :rclass (keyword (:rclass row))
                      })
                     raw)
          ;r-classes (map #(keyword %) classes)
          drivers (set (map :driver facts))]

      (doseq [r-class [:boxc]]
        (println (str "Klasse: " r-class))
        (doseq [driver drivers]
          (let
            [driver-scores (scores driver r-class facts)]
            (println (str driver ": " driver-scores))))))))
