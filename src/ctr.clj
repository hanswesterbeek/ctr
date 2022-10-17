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

(defn driver-stats
  [driver rclass facts]
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
    {:driver driver
     :bruto bruto
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
        (let [drivers (drivers-by-class r-class facts)
              driver-stats (map #(driver-stats % r-class facts) drivers)
              sorted-by-bruto (sort-by :bruto > driver-stats)]
          (doseq [item sorted-by-bruto]
              (println (str (:driver item) " " (format "%03d" (:bruto item)) " " (:by-race item))))
          (println)
            )))))

