(ns ctr
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.test :as t]
            [refdata :as ref]
            ))

(defn csv-data->maps [csv-data]
  (map zipmap
       (->> (first csv-data)
            (map keyword)
            repeat)
       (rest csv-data)))

(def all-drivers (ref/all-drivers (second *command-line-args*)))

(def num-races-projected 25)

(defn consistent-race-id
  [original]
  (let [splitted (clojure.string/split original #"-")
        lh (clojure.string/lower-case (get splitted 0))
        rh-as-int (Integer/parseInt (get splitted 1))]
    (str lh "-" (format "%02d" rh-as-int))))

(defn races-to-scrap-by-num-races
  [num-races]
  (get ref/scrap-reference num-races))

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
      "  ")))

(defn pad [n coll val]
  (take n (concat coll (repeat val))))

(def disqualifications [{:class  :gtc
                         :driver :jebr
                         :races  2
                         }
                        {:class  :cayc
                         :driver :nido
                         :races  2
                         }
                        {:class  :cayc
                         :driver :tehh
                         :races  2
                         }
                        {:class  :cayc
                         :driver :fvda
                         :races  1
                         }
                        {:class  :944c
                         :driver :jobr
                         :races  1
                         }
                        ])

(defn num-dsqs-in-class
  [driver class]
  (let [maybe-dsqs (:races (first (filter #(and
                                             (= (:driver %) driver)
                                             (= (:class %) class))
                                          disqualifications)))]
    (if (nil? maybe-dsqs)
      0
      maybe-dsqs)
    )
  )
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
        dsqs (num-dsqs-in-class driver rclass)
        with-dsqs (concat padded (take dsqs (repeat 0)))
        scrapped-f (drop-last (races-to-scrap-by-num-races num-races-held) with-dsqs)
        p (map #(classification % driver rclass facts) races)]
    {:driver  driver
     :bruto   (reduce + actual)
     :by-race with-dsqs
     :p       p
     :fini    (reduce + scrapped-f)
     :dsqs dsqs
     }
    )
  )


(defn -main
  [& args]
  (with-open [reader (io/reader (nth args 0))]
    (let [raw (csv-data->maps (csv/read-csv reader))
          facts (map
                  (fn [row]
                    {:points   (Integer/parseInt (:points row))
                     :driver   (keyword (clojure.string/lower-case (:driver row)))
                     :rclass   (keyword (:rclass row))
                     :position (Integer/parseInt (:position row))
                     :race     (consistent-race-id (:race row))
                     })
                  raw)
          races (apply sorted-set (set (map #(get % :race) facts)))
          num-races-past (count races)
          r-classes (apply sorted-set (map #(keyword %) (set (map :rclass facts))))
          num-to-scrap (races-to-scrap-by-num-races num-races-past)]
      (println "==========================================")
      (println "Legenda:")
      (println " -  pnt: punten voor schrap")
      (println " - proj: punten na geprojecteerde schrap")
      (println " - fini: punten incl schrap indien")
      (println " -  dk:  diskwalificaties")
      (println "Races gehouden:             " (format "%2d" num-races-past))
      (println "Races te schrappen:         " (format "%2d" num-to-scrap))
      (println "Geprojecteerde races:       " (format "%2d" num-races-projected))
      (println "Geprojecteerde schrapraces: " (format "%2d" (races-to-scrap-by-num-races num-races-projected)))
      (println "Diskwalificaties: " disqualifications)
      (println "==========================================")

      (doseq [r-class r-classes]

        (let [drivers (drivers-by-class r-class facts)
              names (vals (select-keys all-drivers drivers))
              max-driver-name-length (apply max (map #(count %) names))
              driver-name-format (str "%-" max-driver-name-length "s")
              driver-stats (map #(driver-stats % r-class races facts num-races-past) drivers)
              ranking (sort-by :fini > driver-stats)]
          (println (str "Klasse: " (name r-class)))
          (println "=========================================================================================================================")
          (println (format driver-name-format (str "Drvr")) "| fini|brut |dk| punten / resultaten")
          (println "-------------------------------------------------------------------------------------------------------------------------")
          (doseq [item ranking]
            (let [formatted-points (clojure.string/join " " (map #(format "%2d" %) (:by-race item)))]
              (println (format driver-name-format (get all-drivers (:driver item))) "|" (format "%3d" (:fini item)) "|" (format "%3d" (:bruto item)) "|" (format "%1d" (:dsqs item))"|" formatted-points)))
          (println)))
      )))

(t/deftest hmm
  (t/testing "dsqs"
    (t/is (= 2 (num-dsqs-in-class :nidr :cayc)))))

(t/deftest lower-limit-for-scraps
  (t/testing "not enough races"
    (t/is (= 0 (races-to-scrap-by-num-races 2)))))
