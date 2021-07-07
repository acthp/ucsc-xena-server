(ns cavm.benchmark.core
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as s])
  (:require [clojure.edn :as edn])
  (:require [clojure.tools.cli :refer [parse-opts]])
  (:require [cavm.benchmark.data :as benchmark-data])
  (:require [cavm.benchmark.install :refer [install]])
  (:require [cavm.h2 :as h2])
  (:require [cavm.db :as cdb]))

; This should be fine for long-running tasks. As tasks become shorter
; hotspot effects become important, and a tool like criterium is required.
(defmacro timeit [expr]
  `(let [start# (. System (nanoTime))]
     ~expr
     (/ (double (- (. System (nanoTime)) start#)) 1000000.0)))

(def dataset-map
  (into {} (map (fn [ds] [(:key ds) ds]) benchmark-data/datasets)))

; XXX should probably validate the return values to some degree, so
; that we can see nothing is broken. Or do that in unit tests?
(defn probemap-probe [xena {id :id} probes]
  (timeit (cdb/column-query xena
                            {:select ["name" "position"]
                             :from [id]
                             :where [:in "name" probes]})))

(defn cnv-gene [xena {id :id} genes]
  (timeit (cdb/column-query xena
                            {:select ["position" "sampleID" "value"]
                             :from [id]
                             :where [:in "position" (mapv (juxt :chr :start :end) genes)]})))

(defn probe-fetch [xena {id :id} probes samples]
  (timeit (cdb/fetch xena
                     [{:table id
                       :columns probes
                       :samples samples}])))

; Candidate rewrite of probe fetch. Note that it is much slower. Should
; profile this & see if we can match the performance of the current query, above.
(defn probe-fetch-xena [xena {id :id} probes samples]
  (timeit (cdb/column-query xena
                            {:select probes
                             :from [id]
                             :where [:in "sampleID" samples]})))

(defn code-fetch [xena {id :id} field]
  (timeit (cdb/run-query xena
                         {:select [:P.name [#sql/call [:group_concat :value :order :ordering :separator #sql/call [:chr 9]] :code]]
                          :from [[{:select [:field.id :field.name]
                                   :from [:field]
                                   :join [{:table [[[:name :varchar [field]]] :T]} [:= :T.name :field.name]]
                                   :where [:= :dataset_id {:select [:id]
                                                           :from [:dataset]
                                                           :where [:= :name id]}]} :P]]
                          :left-join [:code [:= :P.id :field_id]]
                          :group-by [:P.id]})))

; Candidate rewrite of code fetch. Note that for all-samples query we
; don't really care about ordering, and we need to do a 'distinct' of the
; union of all the sampleID fields. Something to keep in mind if we do
; xena-query support for 'distinct'.
(defn code-fetch2 [xena {id :id} field]
  (timeit (cdb/run-query xena
                         {:select [:value]
                          :order-by [:ordering]
                          :from [:code]
                          :where [:= :field_id {:select [:field.id]
                                                :from [:field]
                                                :where [:and [:= :name field]
                                                        [:= :dataset_id {:select [:id]
                                                                         :from [:dataset]
                                                                         :where [:= :name id]}]]}]})))
; This is fastest.
(defn code-fetch3 [xena {id :id} field]
  (timeit (cdb/run-query xena
                         {:select [:value]
                          :order-by [:ordering]
                          :from [:code]
                          :join [:field [:= :field_id :field.id] :dataset [:= :dataset_id :dataset.id]]
                          :where [:and [:= :field.name field] [:= :dataset.name id]]})))

(defn sort-genes [genes]
  (sort (fn [x y]
          (compare [(:chr x) (:start x)]
                   [(:chr y) (:start y)])) genes))

(def dbfile
  (str (io/file (System/getProperty "user.home") "xena-benchmark/database")))

(defn datafile [id]
  (str (io/file (System/getProperty "user.home") "xena-benchmark/files" id)))

(defn report [msg t]
  (format "%60s\t%f msec" msg t))


(defn line-count [file]
  (with-open [rdr (clojure.java.io/reader file)]
    (count (line-seq rdr))))

(defn rnd-lines [lines size n]
  (let [select (set (take n (distinct (repeatedly #(rand-int size)))))]
    (take n (filter identity
                    (map-indexed (fn [idx itm] (when (select idx) itm))
                                 lines)))))

; cut fields from tsv, similar to unix 'cut'
(defn cut [fields lines]
  (map (fn [line]
         (let [cols (s/split line #"\t")]
           (mapv #(cols %) fields)))
       lines))

; get probes from probemap or matrix
(defn get-probes [{id :id} c]
  (let [file (datafile id)
        N (line-count file)]
    (with-open [rdr (clojure.java.io/reader file)]
      (doall (flatten (cut [0] (rnd-lines (drop 1 (line-seq rdr)) (dec N) c)))))))

; get samples from matrix
(defn get-samples [{id :id} c]
  (let [file (datafile id)]
    (with-open [rdr (clojure.java.io/reader file)]
      (let [samples (rest (s/split (first (line-seq rdr)) #"\t"))
            N (count samples)]
       (doall (rnd-lines samples N c))))))

(defn gene-line [[chr start end gene]]
  {:chr chr
   :start (Integer/parseInt (s/trim start))
   :end (Integer/parseInt (s/trim end))
   :gene gene})

; get genes from refseq, by position.
; Note this will fail if the column order is different.
(defn get-genes [{id :id} c]
  (let [file (datafile id)
        N (line-count file)]
    (with-open [rdr (clojure.java.io/reader file)]
      (doall (map gene-line
                  (cut [2 4 5 12] (rnd-lines (drop 1 (line-seq rdr)) (dec N) c)))))))

(def tests
  [^:probemap
   {:desc "1 probe from probemap of 1.4M rows"
    :id :probemap-large-one
    :params (fn []
             [(get-probes (dataset-map :probemap-large) 1)])
    :body (fn [xena probes]
            (probemap-probe xena (dataset-map :probemap-large) probes))}
   ^:probemap
   {:desc "2 probe from probemap of 1.4M rows"
    :id :probemap-large-two
    :params (fn []
             [(get-probes (dataset-map :probemap-large) 2)])
    :body (fn [xena probes]
            (probemap-probe xena (dataset-map :probemap-large) probes))}
   ^:cnv
   {:desc "3 gene positions from CNV of 970k rows"
    :id :cnv-large-three-gene
    :params (fn []
             [(get-genes (dataset-map :refgene-hg19) 3)])
    :body (fn [xena genes]
            (cnv-gene xena (dataset-map :cnv-large) genes))}
   ^:cnv
   {:desc "30 gene positions from CNV of 970k rows"
    :id :cnv-large-thirty-gene
    :params (fn []
             [(get-genes (dataset-map :refgene-hg19) 30)])
    :body (fn [xena genes]
            (cnv-gene xena (dataset-map :cnv-large) genes))}
   ^:cnv
   {:desc "300 gene positions from CNV of 970k rows"
    :id :cnv-large-three-hundred-gene
    :params (fn []
             [(get-genes (dataset-map :refgene-hg19) 300)])
    :body (fn [xena genes]
            (cnv-gene xena (dataset-map :cnv-large) genes))}
   ^:matrix
   {:desc "30 probes * 500 samples from matrix of 17k rows"
    :id :matrix-medium-thirty-probe
    :params (fn []
             [(get-probes (dataset-map :matrix-medium) 30)
              (get-samples (dataset-map :matrix-medium) 500)])
    :body (fn [xena probes samples]
            (probe-fetch xena (dataset-map :matrix-medium) probes samples))}
   ^:matrix
   {:desc "300 probes * 500 samples from matrix of 17k rows"
    :id :matrix-medium-three-hundred-probe
    :params (fn []
             [(get-probes (dataset-map :matrix-medium) 300)
              (get-samples (dataset-map :matrix-medium) 500)])
    :body (fn [xena probes samples]
            (probe-fetch xena (dataset-map :matrix-medium) probes samples))}

   ^:matrix
   {:desc "300 probes * 500 samples from matrix of 17k rows, xena-query"
    :id :matrix-medium-three-hundred-probe-xena
    :params (fn []
             [(get-probes (dataset-map :matrix-medium) 300)
              (get-samples (dataset-map :matrix-medium) 500)])
    :body (fn [xena probes samples]
            (probe-fetch-xena xena (dataset-map :matrix-medium) probes samples))}

   ^:codes
   {:desc "500 codes"
    :id :codes-medium
    :params (fn [] [])
    :body (fn [xena]
            (code-fetch xena (dataset-map :matrix-medium) "sampleID"))}

   ^:codes
   {:desc "500 codes, #2"
    :id :codes-medium2
    :params (fn [] [])
    :body (fn [xena]
            (code-fetch2 xena (dataset-map :matrix-medium) "sampleID"))}
   ^:codes
   {:desc "500 codes, #3"
    :id :codes-medium3
    :params (fn [] [])
    :body (fn [xena]
            (code-fetch3 xena (dataset-map :matrix-medium) "sampleID"))}])


(def test-map
  (into {} (map #(-> [(:id %) %]) tests)))

(defn print-results [results]
  (doseq [{:keys [id result]} results]
    (println (report (:desc (test-map id)) result))))

(defn- mkdir [dir]
  (.mkdirs (io/file dir))
  (when (not (.exists (io/file dir)))
    (str "Unable to create directory: " dir)))

(def ^:private argspec
  [["-o" "--output FILE" "Output file" :default "bench.edn"]
   ["-h" "--help" "Show help"]
   ["-l" "--load" "Load benchmark params from cache in .benchmark"]
   ["-r" "--input FILE" "Read and print results from FILE"]
   ["-i" "--include TAG" "Run benchmarks with TAG (can be used multiple times)" :default [] :assoc-fn (fn [m k v] (update-in m [k] conj v))]])

(defn run [& args]
  (let [{:keys [options arguments summary errors]} (parse-opts args argspec)
        includes (map keyword (:include options))
        test-filter (if (empty? includes)
                      (fn [_] true)
                      #(some (fn [pat] (or (= (:id %) pat) ((meta %) pat)))
                             includes))]
    (cond
      errors (binding [*out* *err*]
               (println (s/join "\n" errors)))
      (:help options) (println summary)
      (= arguments ["install"]) (install
                                  (io/file (System/getProperty "user.home")
                                           "xena-benchmark"))
      (:input options) (print-results (edn/read-string (slurp (:input options))))
      :else (let [xena (h2/create-xenadb dbfile)
                  results (atom [])]
              (try
                (when-let [error (mkdir (io/file "." ".benchmark"))]
                  (binding [*out* *err*]
                    (println error)
                    (System/exit 1)))
                (doseq [{:keys [id desc params body]} (filter test-filter tests)]
                  (let [params (if (:load options)
                                 (edn/read-string
                                   (slurp (io/file "." ".benchmark" (name id))))
                                 (params))
                        result (apply body xena params)]
                    (when (not (:load options))
                      (spit (io/file "." ".benchmark" (name id)) (pr-str params)))
                    (swap! results conj {:id id :result result})
                    (println (report desc result))))
                (spit (:output options) (pr-str @results))
                (finally
                  (cdb/close xena)))))))


(comment
  (run "benchmark" "-i" "cnv")
  (run "benchmark" "-i" "matrix")
  (run "benchmark" "-i" "matrix-medium-three-hundred-probe-xena")
  (run "benchmark" "-i" "codes")
  (run "benchmark")
  (.printStackTrace *e))


(defn -main [& args]
  (try
    (apply run args)
    (finally
      (shutdown-agents))))
