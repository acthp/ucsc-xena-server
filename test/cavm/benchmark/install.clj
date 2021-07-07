(ns cavm.benchmark.install
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as s])
  (:require [clj-http.client :as client])
  (:require [cgdata.core :as cgdata])
  (:require [cavm.loader :as loader])
  (:require [cavm.readers :as readers])
  (:require [cavm.h2 :as h2])
  (:require cavm.cgdata) ; needed for side-effect of registering methods
  (:require [cavm.benchmark.data :as benchmark-data]))

(defn- mkdir [dir]
  (.mkdirs (io/file dir))
  (when (not (.exists (io/file dir)))
    (str "Unable to create directory: " dir)))

(def ppkey (System/getenv "XENA_KEY"))
(defn ppenc [s]
  (let [k (take (count s) (cycle (map int ppkey)))]
    (s/join (map (fn [c0 c1] (char (bit-xor c0 c1))) (map int s) k))))

; encode a private host like so
; (println (s/join (map #(format "\\%03o" (int %)) (ppenc host-string))))

(defn download [url file]
  (spit file (:body (client/get url))))

(def ^:private detectors
  [cgdata/detect-cgdata
   cgdata/detect-tsv])

; Just download, for now.
(defn install [dir]
  ; Create directory
  (when-let [error (mkdir (io/file dir "files"))]
    (binding [*out* *err*]
      (println error)
      (System/exit 1)))

  (let [docroot (io/file dir "files")
        detector (apply readers/detector docroot detectors)
        xenadb (h2/create-xenadb (io/file dir "database"))]
    (doseq [ds (map #(if (:pp (meta %)) (update-in % [:host] ppenc) %) benchmark-data/datasets)]
      (println "Downloading" (:host ds) (:remote-id ds))
      (download (format "%s/download/%s" (:host ds) (:remote-id ds)) (io/file docroot (:id ds)))
      (download (format "%s/download/%s.json" (:host ds) (:remote-id ds)) (io/file docroot (str (:id ds) ".json")))
      (println "Loading" (:id ds))
      (loader/loader xenadb detector docroot (io/file docroot (:id ds))))))

  ; query remotes to get sha1
  ; there may be multiple files, e.g. for clinical. In that case we will
  ; get two rows.
  ;{:select [:source.name :source.hash]
  ; :from [:dataset]
  ; :join [:dataset_source [:= :dataset.id :dataset_id]
  ;        :source [:= :source.id :source_id]]
  ; :where [:like :dataset.name "%copynumber_donor"]}
