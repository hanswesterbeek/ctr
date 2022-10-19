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

(def scrap-reference {13 0, 14 1, 15 2, 16 3, 17 4, 18 4, 19 4,
                      20 4, 21 5, 22 5, 23 5, 24 6, 25 6, 26 6})
