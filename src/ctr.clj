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

(def scrap-reference {
                      13 13, 14 13, 15 13, 16 13 17 13
           18 14,
           19 15,
           20 16, 21 16,
           22 17,
           23 18,
           24 18,
           25 19,
           26 20})

(defn races-to-count-by-num-starts
  [actual-starts]
  (if
    (< actual-starts (first (sort < (keys scrap-reference))))
      actual-starts
      (get scrap-reference actual-starts)))

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
        num-races (count raw)
        num-to-consider (races-to-count-by-num-starts num-races)
        relevant-results (take num-to-consider raw)
        netto (reduce + relevant-results)]
    {:driver driver
     :bruto bruto
     :by-race raw
     :total-num-races num-races
     :num-counting-races num-to-consider
     :relevant-results relevant-results
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
                      })
                     raw)
          r-classes (map #(keyword %) (set (map :rclass facts)))]
      (println "net: punten na schrap")
      (println "bru: punten voor schrap")
      (println " r: totaal aantal gereden races, tr: tellende races voor eindresultaat")


       ;bru tr cr r-results")
      (doseq [r-class r-classes]
        (println (str "Klasse: " r-class ))
        (println "=========================================================================")
        (println "Drvr | Net | Bru | R  | Tr | Relevante resultaten")
        (println "-------------------------------------------------------------------------")
        (let [drivers (drivers-by-class r-class facts)
              driver-stats (map #(driver-stats % r-class facts) drivers)
              sorted-by-netto (sort-by :netto > driver-stats)]
          (doseq [item sorted-by-netto]
            (println (str (:driver item) " | " (format "%03d" (:netto item)) " | " (format "%03d" (:bruto item)) " | "(format "%02d" (:total-num-races item)) " | " (format "%02d" (:num-counting-races item)) " | " (clojure.string/join " " (:relevant-results item)))))
          (println))))))
