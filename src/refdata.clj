(ns refdata
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]))

(defn load-edn
  "Load edn from an io/reader source (filename or io/resource)."
  [source]
  (try
    (with-open [r (io/reader source)]
      (edn/read (java.io.PushbackReader. r)))

    (catch java.io.IOException e
      (printf "Couldn't open '%s': %s\n" source (.getMessage e)))
    (catch RuntimeException e
      (printf "Error parsing edn file '%s': %s\n" source (.getMessage e)))))

(defn all-drivers
  [url]
  (load-edn url))

(def scrap-reference {0 0,
                      1 0,
                      2 0,
                      3 0,
                      4 0,
                      5 0,
                      6 0,
                      7 0,
                      8 0,
                      9 0,
                      10 0,
                      11 0,
                      12 0,
                      13 0,
                      14 1,
                      15 2,
                      16 3,
                      17 4,
                      18 4,
                      19 4,
                      20 4,
                      21 5,
                      22 5,
                      23 5,
                      24 6,
                      25 6,
                      26 6})

(def driver-aliases {
                     "Janine/Jack Rozendaal"  :tero
                     "Jan Wim de Koekkoek"    :jwko
                     "Niek Jan Steehouwer"    :njst
                     "Ties van Gooswilligen"  :tego
                     "Gijs can Gooswillingen" :gigo
                     "Cas Havik/Huib Havik"   :teha
                     "Job van den Broek",     :jobr
                     "Marc van der Meulen" :mame
                     "Willem van Waes" :wiwa })

(def eligible-finish-statuses #{"DID NOT FINISH" "FINISHED"})

(def class-aliases
  {
   "cc" :cayc
   "cay" :cayc
   "bx" :boxc
   "bc" :boxc
   "944" :944c
   "rs" :rsc
   "gt" :gtc
   "gt4" :gt4c})

(def class-names
  {
   :cayc "Cayman Cup"
   :boxc "Boxster Cup"
   :944c "944 Cup"
   :rsc "RS Cup"
   :gtc "GT Cup"
   :gt4c "GT4 Cup"})

(def points-reference ; very tempting to compute this but subject to too much change
  {1 25
   2 23
   3 21
   4 19
   5 18
   6 17
   7 16
   8 15
   9 14
   10 13
   11 12
   12 11
   13 10
   14 9
   15 8
   16 7
   17 6
   18 5
   19 6
   20 5
   21 4
   22 3
   23 2
   24 1})

(defn pos-to-points [pos]
  (if (< pos 24)
    (get points-reference pos)
    0))
