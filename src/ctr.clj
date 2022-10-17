(ns ctr
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint])
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

(defn drivers-by-class
  [rclass facts]
  (set
    (map
      #(:driver %)
      (filter
        (fn [row] (= rclass (:rclass row)))
        facts)
      )))

(defn scores
  [driver rclass facts]
  ; returns a list of scores like [25 18 23] in descending order
  (let [
        raw (sort >
                  (map
                    (fn [row]
                      (:points row))
                    (filter
                      #(and
                         (= (:driver %) driver)
                         (= (:rclass %) rclass)
                         )
                      facts)))
        bruto (reduce + raw)
        ]
    {:bruto bruto
     :by-race raw}
    )
  )


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
          r-classes (map #(keyword %) (set (map :rclass facts)))]

      (doseq [r-class r-classes]
        (println (str "Klasse: " r-class ))
        (println "############################")
        (let [
              drivers (drivers-by-class r-class facts)
              ;scores (map #({:driver % :netto (:netto (scores % r-class facts))}) drivers)
              ;sorted (into (sorted-map-by (fn [key1 key2] (compare (key2 scores) (key1 scores)))) scores)
              ]
          (doseq [driver drivers]
            (let
              [driver-scores (scores driver r-class facts)]

              (println (str driver " " (format "%03d" (:bruto driver-scores)) " " (:by-race driver-scores)))
              )))))))

