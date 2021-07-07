(ns cavm.db-test
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as s])
  (:require [cavm.h2 :as h2])
  (:require [cavm.db :as cdb])
  (:require [cavm.readers :as cr])
  (:require [cgdata.core :as cgdata])
  (:require [cavm.loader :refer [loader]])
  (:require [cavm.cgdata]) ; register reader methods
  (:require [honeysql.types :as hsqltypes])
  (:require [clojure.test :as ct])
  (:require [clojure.test.check :as tc])
  (:require [clojure.test.check.generators :as gen])
  (:require [clojure.test.check.clojure-test :as tcct :refer [defspec]])
  (:require [clojure.test.check.properties :as prop]))

; XXX fix nearly-equal-matrix to also log failures
(def ^:dynamic *verbose* false)

(defmacro is= [a b]
  `(let [a# ~a
         b# ~b
         r# (= a# b#)]
    (when (and (not r#) *verbose*)
      (println "expected (=" ~(str a) ~(str b) ")")
      (print "actual (not (=")
      (clojure.pprint/write a#)
      (print " ")
      (clojure.pprint/write b#)
      (println "))"))
    r#))

(def ^:dynamic *eps* 0.0001)

(defn- nearly-equal [a b]
  (and (= (count a) (count b))
       (reduce (fn [acc pair]
                 (or (let [[x y] pair]
                       (and
                         (Double/isNaN x)
                         (Double/isNaN y)))
                     (and acc (< (java.lang.Math/abs (apply - pair)) *eps*))))
               true
               (map vector a b))))

(defn- nearly-equal-matrix [a b]
  (and (= (count a) (count b))
       (every? (fn [[a1 b1]] (nearly-equal a1 b1)) (map vector a b))))

(def gen-mostly-ints
  (gen/frequency [[9 gen/int] [1 (gen/return Double/NaN)]]))

; The conditional is necessary to smoothly handle small numbers.
; Otherwise we only do squares, like 1,4,9. Above the threshold we
; are not perfectly accurate. This can't be avoided.
(defn gen-vector-of-size
  "Generate vector of given size, working-around gen/vector
  stackoverflow problem."
  [generator n]
  (if (< n 300) ; picked 300 at random, to avoid stack overflow
    (gen/vector generator n)
    (let [root-n (java.lang.Math/sqrt n)]
      (gen/fmap #(into [] (apply concat %)) (gen/vector (gen/vector generator root-n) root-n)))))

(let [xm 99 ; max 'size' in test.check.generators
      s 10] ; threshold 'size' below which we use slope 1
  (defn scale-size
    "Return a function for scaling a size parameter up to max-size, using a
    piecewise linear mapping that preserves the first s sizes. This allows us
    to shrink to the first s sizes, while covering the range up to max-size."
    [max-size]
    (let [slope (/ (- max-size s) (- xm s))]
      (fn [size]
        (if (< size s)
          size
          (+ s (* (- size s) slope)))))))

(defn scale-gen
  "Return a generator that will scale to max-size, via scale-size."
  [generator max-size]
  (gen/sized (comp #(gen/resize % generator) (scale-size max-size))))

(defn gen-matrix [x y]
  (gen/bind
    (gen-vector-of-size (gen-vector-of-size gen-mostly-ints x) y)
    (fn [m]
      (gen/return m))))

(defn gen-max [generator max-size]
  (gen/sized
    (fn [size]
      (gen/resize (min size max-size) generator))))

(def ascii-quote (int \"))

(defn gen-name [name-max]
  (gen-max (gen/such-that #(and (not-empty (s/trim %)) (= -1 (.indexOf ^String % ascii-quote))) gen/string-ascii)
           name-max))

(def ^:dynamic *name-max* 30)
(def gen-names
  "Generate a list of names which are non-empty after trimming."
  (gen/sized (fn [size]
              (gen/vector
                (gen-name *name-max*)
                1 (max 1 size)))))

(def gen-distinct-names
  "Generate a list of names which are non-empty after trimming, and
  are distinct after trimming."
  (gen/such-that
    #(= (count %) (count (set (map s/trim %))))
    gen-names))

(def gen-tsv
  (gen/bind
    (gen/tuple gen-names gen-distinct-names)
    (fn [[samples probes]]
      (gen/hash-map
        :probes (gen/return probes)
        :samples (gen/return samples)
        :matrix (gen-matrix (count samples) (count probes))))))

(def gen-tsv-distinct
  (gen/bind
    (gen/tuple gen-distinct-names gen-distinct-names)
    (fn [[samples probes]]
      (gen/hash-map
        :probes (gen/return probes)
        :samples (gen/return samples)
        :matrix (gen-matrix (count samples) (count probes))))))

(defn- fields-from-tsv [{:keys [probes samples matrix]}]
  (cons {:rows (range (count samples))
         :field "sampleID"
         :valueType "category"
         :feature {:state samples
                   :order (zipmap samples (range))}}
        (map (fn [id rows] {:rows rows
                            :valueType "float"
                            :feature {}
                            :field id}) probes matrix)))

(defmacro db-fixture [db & body]
  `(let [~db (h2/create-xenadb "test" {:subprotocol "h2:mem"})]
     (try
       ~@body
       (finally (cdb/close ~db)))))

; XXX should generate random file name
(defmacro db-disk-fixture [db & body]
  `(let [~db (h2/create-xenadb "test" {:subprotocol "h2"})]
     (try
       ~@body
       (finally (do
                  (cdb/close ~db)
                  (doseq [f# ["test.h2.db" "test.lock.db" "test.trace.db"]]
                    (io/delete-file f# true)))))))

(def ^:dynamic *test-runs* 40)

(defn check-matrix [db id tsv]
  (let [{:keys [probes samples matrix]} tsv
        exp (cdb/run-query db {:select [:name :status :rows] :from [:dataset]})
        q-samples (cdb/run-query db
                                 {:select [[:value :name]]
                                  :from [:field]
                                  :where [:= :name "sampleID"]
                                  :left-join [:code [:= :field.id :field_id]]
                                  :order-by [:ordering]})
        q-probes (cdb/run-query db {:select [:*] :from [:field]})
        q-data (cdb/fetch db [{:table id
                               :columns probes
                               :samples samples}])]
    (and (is= exp [{:name id :status "loaded" :rows (count samples)}])
         (is= (map :name q-samples) samples)
         (is= (map :name q-probes) (cons "sampleID" probes))
         (nearly-equal-matrix
            matrix
            (vec (map #(into [] %) (map ((first q-data) :data) probes)))))))

(defn genomic-matrix-memory-run
  "Run a generated genomic-matrix-loader test case. Useful for repeated failed
  cases. Run with *verbose* to print detailed test failures"
  [tsv id]
  (db-disk-fixture
    db
    (let [fields (fields-from-tsv tsv)]
      (cdb/write-matrix
        db id
        [{:name id
          :time (org.joda.time.DateTime. 2014 1 1 0 0 0 0)
          :hash "1234"}]
        {} (fn [] fields) nil false)
      (check-matrix db id tsv))))

(defspec genomic-matrix-memory
  *test-runs*
  (prop/for-all
    [tsv gen-tsv-distinct
     id (gen/such-that not-empty gen/string-ascii)]
    (genomic-matrix-memory-run tsv id)))

(defn matrix1 [db]
  (ct/testing "tsv matrix from memory"
    (cdb/write-matrix
      db
      "id1"
      [{:name "id1" :time (org.joda.time.DateTime. 2014 1 1 0 0 0 0) :hash "1234"}]
      {}
      (fn [] [{:rows [0 1]
               :field "sampleID"
               :valueType "category"
               :feature {:state ["sample1" "sample2"]
                         :order {"sample1" 0 "sample2" 1}}}
              {:field "position"
               :valueType "position"
               :rows [{:chrom "chr1"
                       :chromStart 1200
                       :chromEnd 1300
                       :strand "-"}
                      {:chrom "chr2"
                       :chromStart 300
                       :chromEnd 1200
                       :strand "+"}]}
              {:field "genes"
               :valueType "genes"
               :row-val identity
               :rows [["FOXM1" "CUL2"]
                      ["ACK" "BLAH"]]}
              {:rows [1.1 1.2] :field "probe1"}
              {:rows [2.1 2.2] :field "probe2"}])
      nil
      false)

    (let [exp (cdb/run-query db {:select [:name :status :rows] :from [:dataset]})
          samples (cdb/run-query db
                                 {:select [[:value :name]]
                                  :from [:field]
                                  :where [:= :name "sampleID"]
                                  :left-join [:code [:= :field.id :field_id]]
                                  :order-by [:ordering]})
          probes (cdb/run-query db {:select [:*] :from [:field]})
          positions (cdb/run-query db {:select [:*] :from [:field_position]})
          genes (cdb/run-query db {:select [:*] :from [:field_gene]})
          data (cdb/fetch db [{:table "id1"
                               :columns ["probe1" "probe2"]
                               :samples ["sample2" "sample1"]}])
          data2 (cdb/fetch db [{:table "id1"
                                :columns ["probe1" "probe2"]
                                :samples ["sample1" "sample3"]}])]
      (ct/is (= exp [{:name "id1" :status "loaded" :rows 2}]))
      (ct/is (= samples
                [{:name "sample1"}
                 {:name "sample2"}]))
      (ct/is (= probes
                [{:name "sampleID" :id 1 :dataset_id 1}
                 {:name "position" :id 2 :dataset_id 1}
                 {:name "genes"    :id 3 :dataset_id 1}
                 {:name "probe1"   :id 4 :dataset_id 1}
                 {:name "probe2"   :id 5 :dataset_id 1}]))
      (ct/is (= positions
                [{:field_id 2
                  :row 0
                  :chrom "chr1"
                  :chromstart 1200
                  :chromend 1300
                  :bin 4681
                  :strand "-"}
                 {:field_id 2
                  :row 1
                  :chrom "chr2"
                  :chromstart 300
                  :chromend 1200
                  :bin 4681
                  :strand "+"}]))
      (ct/is (= genes
                [{:field_id 3 :row 0 :gene "FOXM1"}
                 {:field_id 3 :row 0 :gene "CUL2"}
                 {:field_id 3 :row 1 :gene "ACK"}
                 {:field_id 3 :row 1 :gene "BLAH"}]))
      (let [[probe1 probe2]
            (vec (map #(into [] %) (map ((first data) :data) ["probe1" "probe2"])))]
        (ct/is (nearly-equal probe1 [1.2 1.1]))
        (ct/is (nearly-equal probe2 [2.2 2.1])))
      (let [[probe1 probe2]
            (vec (map #(into [] %) (map ((first data2) :data) ["probe1" "probe2"])))]
        (ct/is (nearly-equal probe1 [1.1 Double/NaN]))
        (ct/is (nearly-equal probe2 [2.1 Double/NaN]))))))

(def docroot "test/cavm/test_inputs")

(def detector
  (cr/detector
    docroot
    cgdata/detect-cgdata
    cgdata/detect-tsv))

(defn detect-matrix [db]
  (ct/testing "detect tsv matrix"
    (let [{file-type :file-type} (detector "test/cavm/test_inputs/matrix")]
      (ct/is (= file-type :cgdata.core/tsv)))))

(defn str-nan [f]
  (if (and (number? f) (Double/isNaN f)) "" (str f)))

(def ^:dynamic *delete-tmp-files* true)

(defmacro with-file
  "Execute body, ensuring that file is always deleted before returning."
  [bindings & body]
  (if (= (count bindings) 0)
    `(do ~@body)
    `(let ~(subvec bindings 0 2)
       (try
         (with-file ~(subvec bindings 2) ~@body)
         (finally (when *delete-tmp-files*
                    (io/delete-file (io/file ~(bindings 0)) true)))))))

(defn write-clinical-meta [file]
  (spit (io/file file)
        "{\"type\": \"clinicalMatrix\"}"))

(defn transpose [m]
  (apply map vector m))

(defn write-genomic-matrix
  "Write a genomic matrix to a file."
  [file {:keys [matrix probes samples]} clinical meta-file]
  (let [header (cons "sampleID" samples)
        trans (if clinical transpose identity)
        rows (map (fn [probe row] (cons probe row)) probes matrix)]
    (when clinical
      (write-clinical-meta meta-file))
    (spit (io/file file)
          (s/join "\n"
                  (map #(s/join "\t" (map str-nan %)) (trans (cons header rows)))))))

(defn trim-tsv
  "Rewrite tsv with probes and samples trimmed of leading and trailing
  whitespace. Useful for comparing with tsv read from files, which we
  similarly trim."
  [{:keys [probes samples] :as tsv}]
  (assoc tsv
         :probes (map s/trim probes)
         :samples (map s/trim samples)))

; XXX duplicating loader code. This is, perhaps, more "obviously correct",
; but it would be nice not to duplicate the loader code. Perhaps generate
; a tsv w/o dups, then splice in a few dups? Would have to see if that
; shrinks.
(defn drop-dup-samples [{:keys [samples matrix] :as tsv}]
  (let [dist (distinct samples)
        m (transpose
            (map
              (into {} (rseq (mapv #(-> [%1 %2]) samples (transpose matrix))))
              dist))]
    (assoc tsv :samples dist :matrix m)))

(defn genomic-matrix-loader-run
  "Run a generated genomic-matrix-loader test case. Useful for repeated failed
  cases. Run with *verbose* to print detailed test failures"
  [tsv id clinical]
  (with-file [file (str (io/file docroot "generated" id))
              meta-file (str file ".json")]
    (write-genomic-matrix file tsv clinical meta-file)
    (db-disk-fixture
      db
      (loader db detector docroot file)
      (check-matrix db (str (io/file "generated" id)) (drop-dup-samples (trim-tsv tsv))))))

(defspec genomic-matrix-loader
  *test-runs*
  (prop/for-all
    [tsv gen-tsv
     id (gen/such-that not-empty gen/string-alpha-numeric)
     clinical gen/boolean]
    (genomic-matrix-loader-run tsv id clinical)))

(defn matrix2 [db]
  (ct/testing "tsv matrix from file"
    (loader db detector docroot "test/cavm/test_inputs/matrix") ; odd that loader & detector both require docroot
    (let [exp (cdb/run-query db {:select [:name] :from [:dataset]})
          samples (cdb/run-query db
                                 {:select [[:value :name]]
                                  :from [:field]
                                  :where [:= :name "sampleID"]
                                  :left-join [:code [:= :field.id :field_id]]
                                  :order-by [:ordering]})
          probes (cdb/run-query db {:select [:*] :from [:field]})
          data (cdb/fetch db [{:table "matrix"
                               :columns ["probe1" "probe2"]
                               :samples ["sample2" "sample1"]}])]
      (ct/is (= exp [{:name "matrix"}]))
      (ct/is (= samples
                [{:name "sample1"}
                 {:name "sample2"}
                 {:name "sample3"}
                 {:name "sample4"}]))
      (ct/is (= probes
                [{:name "sampleID" :id 1 :dataset_id 1}
                 {:name "probe1" :id 2 :dataset_id 1}
                 {:name "probe2" :id 3 :dataset_id 1}
                 {:name "probe3" :id 4 :dataset_id 1}
                 {:name "probe4" :id 5 :dataset_id 1}
                 {:name "probe5" :id 6 :dataset_id 1}]))
      (let [[probe1 probe2]
            (vec (map #(into [] %) (map ((first data) :data) ["probe1" "probe2"])))]
        (ct/is (nearly-equal probe1 [2.1 1.1]))
        (ct/is (nearly-equal probe2 [2.2 1.2]))))))

(defn matrix2-reload [db]
  (ct/testing "reload tsv matrix from file"
    (loader db detector docroot "test/cavm/test_inputs/matrix")
    (loader db detector docroot "test/cavm/test_inputs/matrix" true)
    (let [exp (cdb/run-query db {:select [:name] :from [:dataset]})
          samples (cdb/run-query db
                                 {:select [[:value :name]]
                                  :from [:field]
                                  :where [:= :name "sampleID"]
                                  :left-join [:code [:= :field.id :field_id]]
                                  :order-by [:ordering]})
          probes (cdb/run-query db {:select [:*] :from [:field]})
          data (cdb/fetch db [{:table "matrix"
                               :columns ["probe1" "probe2"]
                               :samples ["sample2" "sample1"]}])]
      (ct/is (= exp [{:name "matrix"}]))
      (ct/is (= samples
                [{:name "sample1"}
                 {:name "sample2"}
                 {:name "sample3"}
                 {:name "sample4"}]))
      (ct/is (= probes
                [{:name "sampleID" :id 7 :dataset_id 1}
                 {:name "probe1" :id 8 :dataset_id 1}
                 {:name "probe2" :id 9 :dataset_id 1}
                 {:name "probe3" :id 10 :dataset_id 1}
                 {:name "probe4" :id 11 :dataset_id 1}
                 {:name "probe5" :id 12 :dataset_id 1}]))
      (let [[probe1 probe2]
            (vec (map #(into [] %) (map ((first data) :data) ["probe1" "probe2"])))]
        (ct/is (nearly-equal probe1 [2.1 1.1]))
        (ct/is (nearly-equal probe2 [2.2 1.2]))))))

(defn matrix-dup [db]
  (ct/testing "tsv matrix from file with duplicate probe"
    (loader db detector docroot "test/cavm/test_inputs/matrix2") ; odd that loader & detector both require docroot
    (let [exp (cdb/run-query db {:select [:name] :from [:dataset]})
          samples (cdb/run-query db
                                 {:select [[:value :name]]
                                  :from [:field]
                                  :where [:= :name "sampleID"]
                                  :left-join [:code [:= :field.id :field_id]]
                                  :order-by [:ordering]})
          probes (cdb/run-query db {:select [:*] :from [:field]})
          data (cdb/fetch db [{:table "matrix2"
                               :columns ["probe1" "probe2"]
                               :samples ["sample2" "sample1"]}])]
      (ct/is (= exp [{:name "matrix2"}]))
      (ct/is (= samples
                [{:name "sample1"}
                 {:name "sample2"}
                 {:name "sample3"}
                 {:name "sample4"}]))
      (ct/is (= probes
                [{:name "sampleID" :id 1 :dataset_id 1}
                 {:name "probe1" :id 2 :dataset_id 1}
                 {:name "probe2" :id 3 :dataset_id 1}
                 {:name "probe2 (2)" :id 4 :dataset_id 1}
                 {:name "probe4" :id 5 :dataset_id 1}
                 {:name "probe5" :id 6 :dataset_id 1}]))
      (let [[probe1 probe2]
            (vec (map #(into [] %) (map ((first data) :data) ["probe1" "probe2"])))]
        (ct/is (nearly-equal probe1 [2.1 1.1]))
        (ct/is (nearly-equal probe2 [2.2 1.2]))))))

(defn matrix-bad-probe [db]
  (ct/testing "tsv matrix from file with too long probe"
    (loader db detector docroot "test/cavm/test_inputs/matrix3") ; odd that loader & detector both require docroot
    (let [exp (cdb/run-query db {:select [:name] :from [:dataset]})
          samples (cdb/run-query db
                                 {:select [[:value :name]]
                                  :from [:field]
                                  :where [:= :name "sampleID"]
                                  :left-join [:code [:= :field.id :field_id]]
                                  :order-by [:ordering]})
          probes (cdb/run-query db {:select [:*] :from [:field]})
          data (cdb/fetch db [{:table "matrix3"
                               :columns ["probe1" "probe2"]
                               :samples ["sample2" "sample1"]}])]
      (ct/is (= exp [{:name "matrix3"}]))
      (ct/is (= samples
                [{:name "sample1"}
                 {:name "sample2"}
                 {:name "sample3"}
                 {:name "sample4"}]))
      (ct/is (= probes
                [{:name "sampleID" :id 1 :dataset_id 1}
                 {:name "probe1" :id 2 :dataset_id 1}
                 {:name "probe2" :id 3 :dataset_id 1}
                 {:name "probe3aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" :id 4 :dataset_id 1} ; truncated to 250 chars
                 {:name "probe4" :id 5 :dataset_id 1}
                 {:name "probe5" :id 6 :dataset_id 1}]))
      (let [[probe1 probe2]
            (vec (map #(into [] %) (map ((first data) :data) ["probe1" "probe2"])))]
        (ct/is (nearly-equal probe1 [2.1 1.1]))
        (ct/is (nearly-equal probe2 [2.2 1.2]))))))

(defn detect-cgdata-genomic [db]
  (ct/testing "detect cgdata genomic"
    (let [{file-type :file-type} (detector "test/cavm/test_inputs/cgdata_matrix")]
      (ct/is (= file-type :cgdata.core/genomic)))))

(defn matrix3 [db]
  (ct/testing "cgdata genomic matrix"
    (loader db detector docroot "test/cavm/test_inputs/cgdata_matrix")
    (let [exp (cdb/run-query db {:select [:name] :from [:dataset]})
          samples (cdb/run-query db
                                 {:select [[:value :name]]
                                  :from [:field]
                                  :where [:= :name "sampleID"]
                                  :left-join [:code [:= :field.id :field_id]]
                                  :order-by [:ordering]})
          probes (cdb/run-query db {:select [:*] :from [:field]})]
      (ct/is (= exp [{:name "cgdata_matrix"}]))
      (ct/is (= samples
                [{:name "sample1"}
                 {:name "sample2"}
                 {:name "sample3"}
                 {:name "sample4"}]))
      (ct/is (= probes
                [{:name "sampleID" :id 1 :dataset_id 1}
                 {:name "probe1" :id 2 :dataset_id 1}
                 {:name "probe2" :id 3 :dataset_id 1}
                 {:name "probe3" :id 4 :dataset_id 1}
                 {:name "probe4" :id 5 :dataset_id 1}
                 {:name "probe5" :id 6 :dataset_id 1}])))))

(defn detect-cgdata-probemap [db]
  (ct/testing "detect cgdata probemap"
    (let [{file-type :file-type} (detector "test/cavm/test_inputs/probes")]
      (ct/is (= file-type :cgdata.core/probemap)))))

(defn probemap1 [db]
  (ct/testing "cgdata probemap"
    (loader db detector docroot "test/cavm/test_inputs/probes")
    (let [probemap (cdb/run-query db {:select [:name] :from [:dataset]})
          fields (cdb/run-query db {:select [:*] :from [:field]})
          probes (cdb/run-query db {:select [:value] :from [:code]})
          positions (cdb/run-query db {:select [:*] :from [:field_position]})
          genes (cdb/run-query db {:select [:*] :from [:field_gene]})
          probe-field (:id (first (cdb/run-query db {:select [:id] :from [:field] :where [:= :name "name"]})))
          data ((cdb/column-query db {:select ["name"] :from ["probes"]}) "name")]

      (ct/is (= probemap [{:name "probes"}]))
      (ct/is (= (set probes)
                (set [{:value "probe1"}
                      {:value "probe2"}
                      {:value "probe3"}
                      {:value "probe4"}
                      {:value "probe5"}
                      {:value "probe6"}
                      {:value "probe7"}
                      {:value "probe8"}
                      {:value "probe9"}])))
      (ct/is (= (set genes)
                (set
                  [{:field_id 3 :row 0 :gene "GENEA"}
                   {:field_id 3 :row 2 :gene "GENEA"}
                   {:field_id 3 :row 4 :gene "GENEB"}
                   {:field_id 3 :row 6 :gene "GENEB"}
                   {:field_id 3 :row 6 :gene "GENEC"}
                   {:field_id 3 :row 7 :gene "GENED"}
                   {:field_id 3 :row 8 :gene "GENEE"}])))
      (ct/is (= data ["probe1" "probe5" "probe2" "probe6" "probe3" "probe7" "probe4" "probe8" "probe9"])))))

(defn detect-cgdata-clinical [db]
  (ct/testing "detect cgdata clinical"
    (let [{file-type :file-type} (detector "test/cavm/test_inputs/clinical_matrix")]
      (ct/is (= file-type :cgdata.core/clinical)))))

(defn clinical1 [db]
  (ct/testing "cgdata clinical matrix"
    (loader db detector docroot "test/cavm/test_inputs/clinical_matrix")
    (let [exp (cdb/run-query db {:select [:name :type] :from [:dataset]})
          samples (cdb/run-query db
                                 {:select [[:value :name]]
                                  :from [:field]
                                  :where [:= :name "sampleID"]
                                  :left-join [:code [:= :field.id :field_id]]
                                  :order-by [:ordering]})
          probes (cdb/run-query db {:select [:*] :from [:field]})]
      (ct/is (= exp [{:name "clinical_matrix" :type "clinicalMatrix"}]))
      (ct/is (= samples
                [{:name "sample1"}
                 {:name "sample2"}
                 {:name "sample3"}
                 {:name "sample4"}
                 {:name "sample5"}]))
      (ct/is (= probes
                [{:name "sampleID" :id 1 :dataset_id 1}
                 {:name "probe1" :id 2 :dataset_id 1}
                 {:name "probe2" :id 3 :dataset_id 1}
                 {:name "probe3" :id 4 :dataset_id 1}
                 {:name "probe4" :id 5 :dataset_id 1}])))))

(defn clinical2 [db]
  (ct/testing "cgdata clinical matrix2, clinical features"
    (loader db detector docroot "test/cavm/test_inputs/clinical_matrix2")
    (let [exp (cdb/run-query db {:select [:name :type] :from [:dataset]})
          samples (cdb/run-query db
                                 {:select [[:value :name]]
                                  :from [:field]
                                  :where [:= :name "sampleID"]
                                  :left-join [:code [:= :field.id :field_id]]
                                  :order-by [:ordering]})
          probes (cdb/run-query db {:select [:*] :from [:field]})
          features (cdb/run-query db {:select [:longtitle :field_id] :from [:feature]})]
      (ct/is (= exp [{:name "clinical_matrix2" :type "clinicalMatrix"}]))
      (ct/is (= samples
                [{:name "sample1"}
                 {:name "sample2"}
                 {:name "sample3"}
                 {:name "sample4"}
                 {:name "sample5"}]))
      (ct/is (= probes
                [{:name "sampleID" :id 1 :dataset_id 1}
                 {:name "probe1" :id 2 :dataset_id 1}
                 {:name "probe2" :id 3 :dataset_id 1}
                 {:name "probe3" :id 4 :dataset_id 1}
                 {:name "probe4" :id 5 :dataset_id 1}]))
      (ct/is (= features
                [{:longtitle nil :field_id 1} ; sample name
                 {:longtitle "A primary probe" :field_id 2}
                 {:longtitle "A secondary probe" :field_id 3}])))))

(defn clinical3 [db]
  (ct/testing "cgdata clinical matrix3, numeric sample id"
    (loader db detector docroot "test/cavm/test_inputs/clinical_matrix3")
    (let [exp (cdb/run-query db {:select [:name :type] :from [:dataset]})
          samples (cdb/run-query db
                                 {:select [[:value :name]]
                                  :from [:field]
                                  :where [:= :name "sampleID"]
                                  :left-join [:code [:= :field.id :field_id]]
                                  :order-by [:ordering]})
          probes (cdb/run-query db {:select [:*] :from [:field]})
          features (cdb/run-query db {:select [:longtitle :field_id] :from [:feature]})]
      (ct/is (= exp [{:name "clinical_matrix3" :type "clinicalMatrix"}]))
      (ct/is (= samples
                [{:name "1"}
                 {:name "2"}
                 {:name "3"}
                 {:name "4"}
                 {:name "5"}]))
      (ct/is (= probes
                [{:name "sampleID" :id 1 :dataset_id 1}
                 {:name "probe1" :id 2 :dataset_id 1}
                 {:name "probe2" :id 3 :dataset_id 1}
                 {:name "probe3" :id 4 :dataset_id 1}
                 {:name "probe4" :id 5 :dataset_id 1}]))
      (ct/is (= features
                [{:longtitle nil :field_id 1} ; sample name
                 {:longtitle "A primary probe" :field_id 2}
                 {:longtitle "A secondary probe" :field_id 3}])))))

(defn detect-cgdata-mutation [db]
  (ct/testing "detect cgdata mutation"
    (let [{file-type :file-type} (detector "test/cavm/test_inputs/mutation")]
      (ct/is (= file-type :cgdata.core/mutation)))))

(defn get-categorical [db table field samples]
  (let [field-id (->> {:select [:field.id]
                       :from [:dataset]
                       :left-join [:field [:= :dataset.id :dataset_id]]
                       :where [:and
                               [:= :field.name field]
                               [:= :dataset.name table]]}
                      (cdb/run-query db)
                      (first)
                      (:id))
        codes (->> {:select [:ordering :value]
                    :from [:field]
                    :left-join [:code [:= :field.id :field_id]]
                    :where [:= :field_id field-id]}
                   (cdb/run-query db)
                   (#(for [{:keys [ordering value]} %] [ordering value]))
                   (into {}))]
    (->> [{:table table
           :columns [field]
           :samples samples}]
         (cdb/fetch db)
         (first)
         (#(% :data))
         (#(% field))
         (into [])
         (map (comp codes int)))))

(defn get-float [db table field samples]
  (let [field-id (->> {:select [:field.id]
                       :from [:dataset]
                       :left-join [:field [:= :dataset.id :dataset_id]]
                       :where [:and
                               [:= :field.name field]
                               [:= :dataset.name table]]}
                      (cdb/run-query db)
                      (first)
                      (:id))]
    (->> [{:table table
           :columns [field]
           :samples samples}]
         (cdb/fetch db)
         (first)
         (#(% :data))
         (#(% field))
         (into []))))

(defn mutation1 [db]
  (ct/testing "cgdata mutation"
    (loader db detector docroot "test/cavm/test_inputs/mutation")
    (let [effect (get-categorical db "mutation" "effect" ["sample1" "sample2"])
          reference (get-categorical db "mutation" "ref" ["sample1" "sample2"])
          alt (get-categorical db "mutation" "alt" ["sample1" "sample2"])
          amino-acid (get-categorical db "mutation" "amino-acid" ["sample1" "sample2"])
          user-field (get-categorical db "mutation" "user-field" ["sample1" "sample2"])
          user-field2 (get-float db "mutation" "user-field2" ["sample1" "sample2"])
          dataset (first (cdb/run-query db {:select [:*] :from [:dataset]}))]

      (ct/is (= effect ["frameshift_variant"
                        "missense_variant"
                        "missense_variant"]))
      (ct/is (= reference ["G" "G" "C"]))
      (ct/is (= alt ["A" "T" "T"]))
      (ct/is (= amino-acid ["R151W" "F1384L" "R1011K"]))
      (ct/is (= user-field ["blue" "red" "green"]))
      (ct/is (= user-field2 (map float [2.2 1.1 3.3])))
      (ct/is (= (select-keys dataset [:name :datasubtype])
                {:name "mutation" :datasubtype "somatic mutation"})))))

(defn mutation2 [db]
  (ct/testing "cgdata mutation"
    (loader db detector docroot "test/cavm/test_inputs/mutation2")
    (let [effect (get-categorical db "mutation2" "effect" ["TCGA-F2-6879-01"])
          reference (get-categorical db "mutation2" "ref" ["TCGA-F2-6879-01"])
          alt (get-categorical db "mutation2" "alt" ["TCGA-F2-6879-01"])
          amino-acid (get-categorical db "mutation2" "amino-acid" ["TCGA-F2-6879-01"])]

      (ct/is (= effect ["Splice_Site"
                        "Missense_Mutation"
                        "RNA"
                        "Missense_Mutation"
                        "Missense_Mutation"
                        "Missense_Mutation"
                        "RNA"
                        "Silent"
                        "Missense_Mutation"]))
      (ct/is (= reference ["C" "A" "A" "G" "T" "C" "T" "C" "G"]))
      (ct/is (= alt ["T" "T" "G" "A" "C" "A" "C" "T" "A"])))))

(defn segmented [db]
  (ct/testing "tsv segmented from file"
    (loader db detector docroot "test/cavm/test_inputs/segmented") ; odd that loader & detector both require docroot
    (let [exp (cdb/run-query db {:select [:name] :from [:dataset]})
          samples (cdb/run-query db
                                 {:select [[:value :name]]
                                  :from [:field]
                                  :where [:= :name "sampleID"]
                                  :left-join [:code [:= :field.id :field_id]]
                                  :order-by [:ordering]})
          probes (cdb/run-query db {:select [:*] :from [:field]})
          data (cdb/column-query db {:select ["position" "sampleID"] :from ["segmented"]})]

      (ct/is (= exp [{:name "segmented"}]))
      (ct/is (= samples
                [{:name "sample1"}
                 {:name "sample2"}
                 {:name "sample3"}
                 {:name "sample4"}]))
      (ct/is (= probes
                [{:name "position" :id 1 :dataset_id 1}
                 {:name "sampleID" :id 2 :dataset_id 1}
                 {:name "value" :id 3 :dataset_id 1}]))
      (ct/is (= data
                {"sampleID" ["sample1" "sample1" "sample2" "sample3" "sample4"]
                 "position" [
                             {:strand  ".", :chromend 654, :chromstart 321, :chrom  "chr3"}
                             {:strand  ".", :chromend 456, :chromstart 123, :chrom  "chr5"}
                             {:strand  ".", :chromend 21000000, :chromstart 10000000, :chrom  "chr7"}
                             {:strand  ".", :chromend 22222, :chromstart 11111, :chrom  "chrX"}
                             {:strand  ".", :chromend 13000000, :chromstart 12000000, :chrom  "chrX"}]})))))

(defn gene-pred1 [db]
  (ct/testing "gene predition file"
    (loader db detector docroot "test/cavm/test_inputs/refGene")
    (let [chrom-field (:id (first (cdb/run-query db
                                                 {:select [:id]
                                                  :from [:field]
                                                  :where [:= :name "chrom"]})))

          data (cdb/run-query
                 db
                 {:select [:gene :chrom]
                  :from [:field-gene]
                  :join [:field-position [:= :field-gene.row :field-position.row]]})]

      (ct/is (= (set data)
                #{{:gene "MTVR2" :chrom "chr17"}
                  {:gene "LOC100506860" :chrom "chr7"}
                  {:gene "CHMP1B" :chrom "chr18"}
                  {:gene "LOC441204" :chrom "chr7"}
                  {:gene "TCOF1" :chrom "chr5"}
                  {:gene "NSRP1" :chrom "chr17"}
                  {:gene "SPPL3" :chrom "chr12"}
                  {:gene "OPA3" :chrom "chr19"}
                  {:gene "OPA1" :chrom "chr3"}})))))

; XXX test that cgdata defaults to genomicMatrix if not specified

; clojure.test fixtures don't work with nested tests, so we
; have to invoke fixtures ourselves.
(defn run-tests [fixture]
  (doseq [t [matrix1 detect-matrix matrix2 detect-cgdata-genomic matrix3
             matrix2-reload
             detect-cgdata-probemap probemap1
             detect-cgdata-clinical clinical1
             detect-cgdata-mutation mutation1 mutation2
             gene-pred1 matrix-dup matrix-bad-probe
             clinical2 clinical3 segmented]]
    (fixture t)))

(ct/deftest test-h2
  (run-tests
    (fn [f]
      (let [db (h2/create-xenadb "test" {:subprotocol "h2:mem"})]
        (try (f db) ; 'finally' ensures our teardown always runs
          (finally (cdb/close db)))))))

;
;
;

(defn db-in-memory-test-run
  "Run an in-memory db test case."
  [fields testfn]
  (db-fixture
    db
    (cdb/write-matrix
      db "foo"
      [{:name "foo"
        :time (org.joda.time.DateTime. 2014 1 1 0 0 0 0)
        :hash "1234"}]
      {} (fn [] fields) nil false)
    (testfn db)))

(defn gene-field [name rows]
  {:field name
   :valueType "genes"
   :row-val identity
   :rows rows})

(defn category-field [name values]
  (let [not-nil (filter #(not= % "") values)
        order (zipmap not-nil (range))
        nil-order (assoc order "" Double/NaN)]
    {:rows (map nil-order values)
     :field name
     :valueType "category"
     :feature {:state values
               :order order}}))

(ct/deftest gene-test
  (let [field0 (gene-field "genes"
                           [["a"]
                            ["a" "b"]
                            ["c"]
                            ["d" "e"]])

        field1 (category-field "name"
                               ["probe0"
                                ""
                                "probe2"
                                "probe3"])]
    (db-in-memory-test-run
      [field0 field1]
      (fn [db]
        (ct/is (= (cdb/column-query db
                    {:select ["genes"]
                     :from ["foo"]
                     :where [:in :any "genes" ["a"]]})
                  {"genes" [["a"] ["a" "b"]]}))
        (ct/is (= (cdb/column-query db
                    {:select ["genes"]
                     :from ["foo"]
                     :where [:in :any "genes" ["a" "z"]]})
                  {"genes" [["a"] ["a" "b"]]}))
        (ct/is (= (cdb/column-query db
                    {:select ["name"]
                     :from ["foo"]
                     :where [:in :any "genes" ["z"]]})
                  {"name" []}))
        (ct/is (= (cdb/column-query db
                    {:select ["name"]
                     :from ["foo"]
                     :where [:in :any "genes" ["a"]]})
                  {"name" ["probe0" nil]}))))))


;
; Some experiments with generators for gene fields.
;

; Generate list of gene names.
(def gen-genes
  (gen/such-that not-empty (gen/vector (gen/not-empty gen/string-alpha-numeric))))

; Generate list of genes, and list of rows of genes selected from it, e.g.
; [["foxm1" "tp53" "her2"] [["foxm1"] ["tp53"] ["foxm1" "tp53"]]]
(def gen-gene-rows
  (gen/bind gen-genes
            (fn [genes] (gen/tuple (gen/return genes)
                                   (gen/vector (gen/vector (gen/one-of (map gen/return genes))))))))

; Generate field name + rows of genes.
(def gen-gene-field
  (gen/fmap #(apply gene-field (% 1)) (gen/tuple gen/string-alpha-numeric gen-gene-rows)))

;(def sql-prop
;  (prop/for-all [field gen-gene-rows]
;                (db-in-memory-test-run [field] (fn [db] (ct/is (= 1 1))))))

;(tc/quick-check 100
;                sql-prop)
