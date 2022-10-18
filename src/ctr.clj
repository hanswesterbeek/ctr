(ns ctr
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]))

(defn csv-data->maps [csv-data]
  (map zipmap
       (->> (first csv-data)
            (map keyword)
            repeat)
       (rest csv-data)))

(def num-races-projected 25)

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

(defn pad [n coll val]
  (take n (concat coll (repeat val))))


(defn driver-stats
  [driver rclass races facts num-races-held]
  (let [
        actual (sort >
              (map
                (fn [row]
                  (:points row))
                (filter
                  #(and
                     (= (:driver %) driver)
                     (= (:rclass %) rclass))
                  facts)))
        padded (sort > (pad num-races-held actual 0))
        scrapped-p (drop-last (races-to-scrap-by-num-races num-races-projected) padded)
        scrapped-f (drop-last (races-to-scrap-by-num-races num-races-held) padded)
        p (map #(classification % driver rclass facts) races)]
    {:driver driver
     :bruto (reduce + actual)
     :by-race actual
     :p p
     :proj (reduce + scrapped-p)
     :fini (reduce + scrapped-f)
     }
    )
  )


(defn -main
  [&args]
  (with-open [reader (io/reader (first *command-line-args*))]
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
          num-races-past (count races)
          r-classes (map #(keyword %) (set (map :rclass facts)))
          num-to-scrap (races-to-scrap-by-num-races num-races-past)]
      (println "==========================================")
      (println "Legenda:")
      (println " -  pnt: punten voor schrap")
      (println " - proj: punten na geprojecteerde schrap")
      (println " - fini: punten incl schrap indien seizoen nu eindigt ('finito')")
      (println "Races gehouden:             " (format "%2d" num-races-past))
      (println "Races te schrappen:         " (format "%2d" num-to-scrap))
      (println "Geprojecteerde races:       " (format "%2d" num-races-projected))
      (println "Geprojecteerde schrapraces: " (format "%2d" (races-to-scrap-by-num-races num-races-projected)))
      (println "==========================================")

      (doseq [r-class r-classes]
        (println (str "Klasse: " (name r-class)))
        (println "==============================================================================================")
        (println "Drvr | pnt | proj | fini | Resultaten")
        (println "----------------------------------------------------------------------------------------------")
        (let [drivers (drivers-by-class r-class facts)
              driver-stats (map #(driver-stats % r-class races facts num-races-past) drivers)
              sorted-by-bruto (sort-by :bruto > driver-stats)]
          (doseq [item sorted-by-bruto]
            (println (str (:driver item) " | " (format "%3d" (:bruto item)) " | " (format "%3d" (:proj item)) "  | " (format "%3d" (:fini item)) "  | " (clojure.string/join " " (:p item)))))
          (println))))))
