(ns ctr
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]))

(defn csv-data->maps [csv-data]
  (map zipmap
       (->> (first csv-data)
            (map keyword)
            repeat)
       (rest csv-data)))

(def scrap-reference {13 0, 14 1, 15 2, 16 3, 17 4, 18 4, 19 4,
                      20 4, 21 5, 22 5, 23 5, 24 6, 25 6, 26 6})

(defn consistent-race-id
  [original]
  (let [splitted (clojure.string/split original #"-")
        lh (clojure.string/lower-case (get splitted 0))
        rh-as-int (Integer/parseInt (get splitted 1))]
    (str lh "-" (format "%02d" rh-as-int))))

(defn races-to-scrap-by-num-races
  [num-races]
  (let [x (sort < (keys scrap-reference))]
    (cond
      (< num-races (first x)) 0
      (> num-races (last x)) (get num-races scrap-reference))
      :else (get scrap-reference num-races)))

(defn drivers-by-class
  [rclass facts]
  (set
    (map
      #(:driver %)
      (filter
        (fn [row] (= rclass (:rclass row)))
        facts))))

(defn classification
  [race driver rclass facts]
  (let [maybe-position (:position
                         (first
                           (filter
                             #(and
                                (= (:driver %) driver)
                                (= (:rclass %) rclass)
                                (= (:race %) race))
                             facts)))]
  (if (some? maybe-position)
    (format "%2d" maybe-position)
    "  "
    )
  ))

(defn driver-stats
  [driver rclass races facts]
  (let [
        raw (sort >
              (map
                (fn [row]
                  (:points row))
                (filter
                  #(and
                     (= (:driver %) driver)
                     (= (:rclass %) rclass))
                  facts)))
        bruto (reduce + raw)
        p (map #(classification % driver rclass facts) races)
        netto 0]
    {:driver driver
     :bruto bruto
     :by-race raw
     :p p
     :netto netto
     }
    )
  )


(defn -main
  []
  (with-open [reader (io/reader (io/resource "results.csv"))]
    (let [raw (csv-data->maps (csv/read-csv reader))
          facts (map
                  (fn [row]
                     {:points (Integer/parseInt (:points row))
                      :driver (clojure.string/lower-case (:driver row))
                      :rclass (keyword (:rclass row))
                      :position (Integer/parseInt (:position row))
                      :race (consistent-race-id (:race row))
                      })
                     raw)
          races (apply sorted-set (set (map #(get % :race) facts)))
          num-races (count races)
          r-classes (map #(keyword %) (set (map :rclass facts)))
          num-to-scrap (races-to-scrap-by-num-races num-races)]
      (println "==========================================")
      (println "Legenda:")
      (println " - net: punten na schrap")
      (println " - bru: punten voor schrap")
      (println "Races verwerkt:     " num-races)
      (println "Aantal te schrappen: " num-to-scrap)
      (println "==========================================")

      (doseq [r-class r-classes]
        (println (str "Klasse: " (name r-class)))
        (println "==============================================================================================")
        (println "Drvr | Bru | Resultaten")
        (println "----------------------------------------------------------------------------------------------")
        (let [drivers (drivers-by-class r-class facts)
              driver-stats (map #(driver-stats % r-class races facts) drivers)
              sorted-by-bruto (sort-by :bruto > driver-stats)]
          (doseq [item sorted-by-bruto]
            (println (str (:driver item) " | " (format "%3d" (:bruto item)) " | " (clojure.string/join " " (:p item)))))
          (println))))))
