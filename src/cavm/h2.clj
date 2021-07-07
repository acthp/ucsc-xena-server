(ns
  ^{:author "Brian Craft"
    :doc "Xena H2 database backend"}
  cavm.h2
  (:require [clojure.java.jdbc.deprecated :as jdbcd])
  (:require [clojure.java.jdbc :as jdbc])
  (:require [cavm.jdbc])
  (:require [org.clojars.smee.binary.core :as binary])
  (:require [clojure.java.io :as io])
  (:require [honeysql.core :as hsql])
  (:require [honeysql.format :as hsqlfmt])
  (:require [honeysql.types :as hsqltypes])
  (:require [cavm.json :as json])
  (:require [cavm.binner :refer [calc-bin overlapping-bins]])
  (:use [clj-time.format :only (formatter unparse)])
  (:use [cavm.hashable :only (ahashable)])
  (:require [cavm.db :refer [XenaDb]])
  (:require [clojure.tools.logging :refer [spy trace info warn]])
  (:require [taoensso.timbre.profiling :refer [profile p]])
  (:require [clojure.core.cache :as cache])
  (:require [cavm.statement :refer [sql-stmt sql-stmt-result cached-statement]])
  (:require [cavm.conn-pool :refer [pool]])
  (:require [cavm.lazy-utils :refer [consume-vec lazy-mapcat]])
  (:require [clojure.core.match :refer [match]])
  (:require [cavm.sqleval :refer [evaluate]])
  (:require [clojure.data.int-map :as i])
  (:import [org.h2.jdbc JdbcBatchUpdateException])
  (:import [com.mchange.v2.c3p0 ComboPooledDataSource]))

(def h2-log-level
  (into {} (map vector [:off :error :info :debug :slf4j] (range))))

;
; Note that "bin" in this file is like a "segement" in the column store
; literature: a column is split into a series of segments (bins) before
; writing to disk (saving as blob in h2).
;

(def ^:dynamic ^:private *tmp-dir* (System/getProperty "java.io.tmpdir"))

(defn set-tmp-dir! [dir]
  (alter-var-root #'*tmp-dir* (constantly dir)))

; Coerce this sql fn name to a keyword so we can reference it.
(def ^:private KEY-ID (keyword "scope_identity()"))

(def ^:private float-size 4)
(def ^:private bin-size 1000)
(def ^:private score-size (* float-size bin-size))
; Add 30 for gzip header
;( def score-size (+ 30 (* float-size bin-size)))

; row count for batch operations (e.g. insert/delete)
(def ^:private batch-size 1000)

;
; Utility functions
;

(defn- chunked-pmap [f coll]
  (->> coll
       (partition-all 250)
       (pmap (fn [chunk] (doall (map f chunk))))
       (apply concat)))

(defn- memoize-key
  "Memoize with the given function of the arguments"
  [kf f]
  (let [mem (atom {})]
    (fn [& args]
      (let [k (kf args)]
        (if-let [e (find @mem k)]
          (val e)
          (let [ret (apply f args)]
            (swap! mem assoc k ret)
            ret))))))

;
; Table models
;

(declare score-decode)

(defn- cvt-scores [{scores :scores :as v}]
  (if scores
    (assoc v :scores (float-array (score-decode scores)))
    v))

;
; Table definitions.
;

(def ^:private max-probe-length 250)
(def ^:private probe-version-length 5) ; to disambiguate probes by appending " (dd)"

; Note H2 has one int type: signed, 4 byte. Max is approximately 2 billion.
(def ^:private field-table
  ["CREATE SEQUENCE IF NOT EXISTS FIELD_IDS CACHE 2000"
   (format "CREATE TABLE IF NOT EXISTS `field` (
           `id` INT NOT NULL DEFAULT NEXT VALUE FOR FIELD_IDS PRIMARY KEY,
           `dataset_id` INT NOT NULL,
           `name` VARCHAR(%d),
           UNIQUE(`dataset_id`, `name`),
           FOREIGN KEY (`dataset_id`) REFERENCES `dataset` (`id`) ON DELETE CASCADE)"
           (+ max-probe-length probe-version-length))])

(def ^:private field-score-table
  [(format  "CREATE TABLE IF NOT EXISTS `field_score` (
            `field_id` INT NOT NULL,
            `i` INT NOT NULL,
            `scores` VARBINARY(%d) NOT NULL,
            UNIQUE (`field_id`, `i`),
            FOREIGN KEY (`field_id`) REFERENCES `field` (`id`) ON DELETE CASCADE)" score-size)])

(def ^:private cohorts-table
  ["CREATE TABLE IF NOT EXISTS `cohorts` (
   `id` INT NOT NULL AUTO_INCREMENT PRIMARY KEY,
   `name` VARCHAR(2000) NOT NULL UNIQUE)"])

; XXX What should max file name length be?
; XXX Unique columns? Indexes?
(def ^:private source-table
  ["CREATE TABLE IF NOT EXISTS `source` (
   `id` INT NOT NULL AUTO_INCREMENT PRIMARY KEY,
   `name` VARCHAR(2000) NOT NULL,
   `time` TIMESTAMP NOT NULL,
   `hash` VARCHAR(40) NOT NULL)"])

(def ^:private dataset-table
  ["CREATE TABLE IF NOT EXISTS `dataset` (
   `id` INT NOT NULL AUTO_INCREMENT PRIMARY KEY,
   `name` VARCHAR(1000) NOT NULL UNIQUE,
   `probeMap` VARCHAR(255),
   `shortTitle` VARCHAR(255),
   `longTitle` VARCHAR(255),
   `groupTitle` VARCHAR(255),
   `platform` VARCHAR(255),
   `cohort` VARCHAR(1000),
   `security` VARCHAR(255),
   `rows` INT,
   `status` VARCHAR(20),
   `text` VARCHAR (65535),
   `type` VARCHAR (255),
   `dataSubType` VARCHAR (255))"])

(def ^:private dataset-columns
  #{:name
    :probeMap
    :shortTitle
    :longTitle
    :groupTitle
    :platform
    :cohort
    :security
    :dataSubType
    :type
    :text})

(def ^:private dataset-defaults
  (into {} (map #(vector % nil) dataset-columns)))

(def ^:private dataset-source-table
  ["CREATE TABLE IF NOT EXISTS `dataset_source` (
   `dataset_id` INT NOT NULL,
   `source_id` INT NOT NULL,
   FOREIGN KEY (dataset_id) REFERENCES `dataset` (`id`) ON DELETE CASCADE,
   FOREIGN KEY (source_id) REFERENCES `source` (`id`) ON DELETE CASCADE)"])

(def ^:private dataset-meta
  {:defaults dataset-defaults
   :columns dataset-columns})

; XXX CASCADE might perform badly. Might need to do this incrementally in application code.
(def ^:private field-position-table
  ["CREATE TABLE IF NOT EXISTS `field_position` (
   `field_id` INT NOT NULL,
   `row` INT NOT NULL,
   `bin` INT,
   `chrom` VARCHAR(255) NOT NULL,
   `chromStart` INT NOT NULL,
   `chromEnd` INT NOT NULL,
   `strand` CHAR(1),
   FOREIGN KEY (`field_id`) REFERENCES `field` (`id`) ON DELETE CASCADE)"
   "CREATE INDEX IF NOT EXISTS chrom_bin ON field_position (`field_id`, `chrom`, `bin`)"
   "CREATE INDEX IF NOT EXISTS position_row ON field_position (`field_id`, `row`)"])

(def ^:private field-gene-table
  ["CREATE TABLE IF NOT EXISTS `field_gene` (
   `field_id` INT NOT NULL,
   `row` INT NOT NULL,
   `gene` VARCHAR_IGNORECASE(255) NOT NULL,
    FOREIGN KEY (`field_id`) REFERENCES `field` (`id`) ON DELETE CASCADE)"
   "CREATE INDEX IF NOT EXISTS field_gene ON `field_gene` (`field_id`, `gene`)"
   "CREATE INDEX IF NOT EXISTS gene_row ON `field_gene` (`field_id`, `row`)"])

;
; feature tables
;

(def ^:private feature-table
  ["CREATE SEQUENCE IF NOT EXISTS FEATURE_IDS CACHE 2000"
   "CREATE TABLE IF NOT EXISTS `feature` (
   `id` INT NOT NULL DEFAULT NEXT VALUE FOR FEATURE_IDS PRIMARY KEY,
   `field_id` INT(11) NOT NULL,
   `shortTitle` VARCHAR(255),
   `longTitle` VARCHAR(255),
   `priority` DOUBLE DEFAULT NULL,
   `valueType` VARCHAR(255) NOT NULL,
   `visibility` VARCHAR(255),
   FOREIGN KEY (`field_id`) REFERENCES `field` (`id`) ON DELETE CASCADE)"])

(def ^:private feature-columns
  #{:shortTitle
    :longTitle
    :priority
    :valueType
    :visibility})

(def ^:private feature-defaults
  (into {} (map #(vector % nil) feature-columns)))


(def ^:private feature-meta
  {:defaults feature-defaults
   :columns feature-columns})

; Order is important to avoid creating duplicate indexes. A foreign
; key constraint creates an index if one doesn't exist, so create our
; index before adding the constraint.
; XXX varchar size is pushing it. Do we need another type for clob-valued
; fields?
(def ^:private code-table
  ["CREATE TABLE IF NOT EXISTS `code` (
   `id` INT(11) NOT NULL AUTO_INCREMENT PRIMARY KEY,
   `field_id` INT(11) NOT NULL,
   `ordering` INT(10) unsigned NOT NULL,
   `value` VARCHAR(16384) NOT NULL,
   UNIQUE (`field_id`, `ordering`),
   FOREIGN KEY (`field_id`) REFERENCES `field` (`id`) ON DELETE CASCADE)"])

(defn- normalize-meta [m-ent metadata]
  (-> metadata
      (clojure.walk/keywordize-keys)
      (#(merge (:defaults m-ent) %))
      (select-keys (:columns m-ent))))

(defn- json-text [md]
  (assoc md :text (json/write-str md :escape-slash false)))

(defn- load-probe-meta
  [feature-seq field-id {:keys [order] :as feat}]
  (let [feature-id (feature-seq)
        fmeta (merge (normalize-meta feature-meta feat)
                     {:field_id field-id :id feature-id})]
    (cons [:insert-feature (assoc fmeta :id feature-id)]
          (for [[value ordering] order]
            [:insert-code {:field_id field-id
                           :ordering ordering
                           :value value}]))))
;
;
;

(defn do-command-while-updates [cmd]
  (while
    (> (first (jdbcd/do-commands cmd)) 0)))

(defn delete-rows-by-field-cmd [table field]
  (format "DELETE FROM `%s` WHERE `field_id` = %d LIMIT %d"
                table field batch-size))

(defn- clear-fields [exp]
  (jdbcd/with-query-results
    field
    ["SELECT `id` FROM `field` WHERE `dataset_id` = ?" exp]
    (doseq [{id :id} field]
      (doseq [table ["code" "feature" "field_gene" "field_position" "field_score"]]
        (do-command-while-updates
          (delete-rows-by-field-cmd table id)))))
  (do-command-while-updates
    (format "DELETE FROM `field` WHERE `dataset_id` = %d LIMIT %d"
            exp batch-size)))

; Deletes an experiment. We break this up into a series
; of commits so other threads are not blocked, and we
; don't time-out the db thread pool.
; XXX Might want to add a "deleting" status to the dataset table?
(defn- clear-by-exp [exp]
  (clear-fields exp)
  (jdbcd/delete-rows :field ["`dataset_id` = ?" exp]))

; Update meta entity record.
(defn- merge-m-ent [ename metadata]
  (let [normmeta (normalize-meta dataset-meta (json-text (assoc metadata
                                                                "name" ename)))
        {id :id} (jdbcd/with-query-results
                   row
                   [(str "SELECT `id` FROM `dataset` WHERE `name` = ?") ename]
                   (first row))]
    (if id
      (do
        (jdbcd/update-values :dataset ["`id` = ?" id] normmeta)
        id)
      (-> (jdbcd/insert-record :dataset normmeta) KEY-ID))))

;
; Hex
;

(def ^:private hex-chars (char-array "0123456789ABCDEF"))

(defn- bytes->hex [^bytes ba]
  (let [c (count ba)
        out ^chars (char-array (* 2 c))]
    (loop [i 0]
      (when (< i c)
        (aset out (* 2 i)
              (aget ^chars hex-chars
                    (bit-shift-right
                      (bit-and 0xF0 (aget ba i))
                      4)))
        (aset out (+ 1 (* 2 i))
              (aget ^chars hex-chars
                    (bit-and 0x0F (aget ba i))))
        (recur (inc i))))
    (String. out)))

;
; Binary buffer manipulation.
;

(defn- sort-float-bytes
  "Sorts a byte array by float byte order. This makes the bytes very slightly
  more compressable."
  [^bytes in]
  (let [c (count in)
        fc (/ c 4)
        out (byte-array c)]
    (loop [i 0
           b1 0
           b2 fc
           b3 (* fc 2)
           b4 (* fc 3)]
      (when (< i c)
        (aset out b1 (aget in i))
        (aset out b2 (aget in (+ i 1)))
        (aset out b3 (aget in (+ i 2)))
        (aset out b4 (aget in (+ i 3)))
        (recur (+ i 4) (inc b1) (inc b2) (inc b3) (inc b4))))
    out))

; Pick elements of float array, by index.
(defn- apick-float [^floats a idxs]
 (let [ia (float-array idxs)]
  (amap ia i ret (aget a i))))

(defn- ashuffle-float [mappings ^floats out ^floats in]
   (doseq [[i-in i-out] mappings]
     (aset out i-out (aget in i-in)))
   out)

(defn- bytes-to-floats [^bytes barr]
  (let [bb (java.nio.ByteBuffer/allocate (alength barr))
        fb (.asFloatBuffer bb)
        out (float-array (quot (alength barr) 4))]
    (.put bb barr)
    (.get fb out)
    out))

(defn- floats-to-bytes [^floats farr]
  (let [bb (java.nio.ByteBuffer/allocate (* (alength farr) 4))
        fb (.asFloatBuffer bb)]
    (.put fb farr)
    (.array bb)))

;
; Score encoders
;

(def ^:private codec-length
  (memoize (fn [len]
             (binary/repeated :float-le :length len))))

(defn- codec [blob]
  (codec-length (/ (count blob) float-size)))

(defn score-decode [blob]
  (bytes-to-floats blob))

(defn score-encode-orig [slist]
  (floats-to-bytes (float-array slist)))

(defn- score-encodez
  "Experimental compressing score encoder."
  [slist]
  (let [baos (java.io.ByteArrayOutputStream.)
        gzos (java.util.zip.GZIPOutputStream. baos)]
    (binary/encode (codec-length (count slist)) gzos slist)
    (.finish gzos)
    (.toByteArray baos)))

(defn- score-sort-encodez
  "Experimental compressing score encoder that sorts by byte order before compressing."
  [slist]
  (let [fbytes (score-encode-orig slist)
        ^bytes sorted (sort-float-bytes fbytes)
        baos (java.io.ByteArrayOutputStream.)
        gzos (java.util.zip.GZIPOutputStream. baos)]
    (.write gzos sorted)
    (.finish gzos)
    (.toByteArray baos)))

(defn- score-decodez [blob]
  (let [bais (java.io.ByteArrayInputStream. blob)
        gzis (java.util.zip.GZIPInputStream. bais)]
    (binary/decode (binary/repeated :float-le :length 10) gzis)))

(def score-encode score-encode-orig)

(defn- encode-row [encode row]
  (mapv encode (partition-all bin-size row)))

(defn- encode-fields [encoder matrix-fn]
  (chunked-pmap #(assoc % :data (encode-row encoder (:scores %)))
                (:fields (matrix-fn))))

;
; sequence utilities


; This is a stateful iterator. It may not be idiomatic in clojure, but it's a
; quick way to get sequential ids deep in a call stack. Maybe we should instead
; return a thunk that generates a seq, then zip with the data?

(defn- seq-iterator
  "Create iterator for a seq"
  [s]
  (let [a (atom s)]
    (fn []
      (let [s @a]
        (reset! a (next s))
        (first s)))))

; Generates primary keys using range, w/o accessing the db for each next value.
; This works in h2 when doing a csvread when the sequence has been created
; automatically by h2 with the column: h2 will update the sequence while reading
; the rows. It doesn't work if the sequence is create separately from the column.
; In that case h2 will not update the sequence after a csvread. We have to
; create the sequence separately in order to set the cache parameter.

(comment (defn- sequence-seq [seqname]
          (let [[{val :i}] (exec-raw (format "SELECT CURRVAL('%s') AS I" seqname) :results)]
            (range (+ val 1) Double/POSITIVE_INFINITY 1))))

(comment
  (defn- sequence-seq
    "Retrieve primary keys in batches by querying the db"
    [seqname]
    (let [cmd (format "SELECT NEXT VALUE FOR %s AS i FROM SYSTEM_RANGE(1, 100)" seqname)]
      (apply concat
             (repeatedly
               (fn [] (-> cmd str
                          (exec-raw :results)
                          (#(map :i %)))))))))

;
; Main loader routines. Here we build a sequence of vectors describing rows to be
; inserted, e.g.
; [:insert-score {:id 1 :scores #<byte[] [B@12345>}]
;

(defmulti ^:private load-field
  (fn [dataset-id field-id column]
    (-> column :valueType)))

(defmethod load-field :default [dataset-id field-id {:keys [field rows]}]
  (let [data (encode-row score-encode (force rows))]
    (conj (mapcat (fn [[block i]]
                    [[:insert-field-score {:field_id field-id
                                           :i i
                                           :scores block}]])
                  (map vector (consume-vec data) (range)))
          [:insert-field {:id field-id :dataset_id dataset-id :name field}])))

; This should be lazy to avoid simultaneously instantiating all the rows as
; individual objects.
(defmethod load-field "position" [dataset-id field-id {:keys [field rows]}]
  (conj (for [[position row] (map vector (force rows) (range))]
          [:insert-position (assoc position
                                   :field_id field-id
                                   :row row
                                   :bin (calc-bin
                                          (:chromStart position)
                                          (:chromEnd position)))])
        [:insert-field {:id field-id :dataset_id dataset-id :name field}]))

(defmethod load-field "genes" [dataset-id field-id {:keys [field rows row-val]}]
  (conj (mapcat (fn [[genes row]]
                  (for [gene (row-val genes)]
                    [:insert-gene {:field_id field-id :row row :gene gene}]))
                (map vector (consume-vec (force rows)) (range)))
        [:insert-field {:id field-id :dataset_id dataset-id :name field}]))

(defn- inferred-type
  "Replace the given type with the inferred type"
  [fmeta row]
  (assoc fmeta "valueType" (:valueType row)))

(defn- load-field-feature [feature-seq field-seq dataset-id field]
  (let [field-id (field-seq)]
    (concat (load-field dataset-id field-id field)
            (when-let [feature (force (:feature field))]
              (load-probe-meta feature-seq field-id (inferred-type feature field))))))
;
;
;
; Table writer that updates tables as rows are read.
;
;

(defn- quote-ent [x]
  (str \` (name x) \`))

(defn- insert-stmt ^cavm.statement.PStatement [table fields]
   (let [field-str (clojure.string/join ", " (map quote-ent fields))
         val-str (clojure.string/join ", " (repeat (count fields), "?"))
         stmt-str (format "INSERT INTO %s(%s) VALUES (%s)"
                      (quote-ent table) field-str val-str)]
     (sql-stmt (jdbcd/find-connection) stmt-str fields)))

(defn- sequence-stmt ^cavm.statement.PStatement [db-seq]
  (sql-stmt-result (jdbcd/find-connection)
                   (format "SELECT NEXT VALUE FOR %s AS i" db-seq)))

(defn- run-delays [m]
  (into {} (for [[k v] m] [k (force v)])))

(def ^:private max-insert-retry 20)

(defn trunc [^String s n]
  (subs s 0 (min (.length s) n)))

(defn- numbered-suffix [n]
  (if (== 1 n) "" (str " (" n ")")))

(defn- table-writer-default [dir dataset-id matrix-fn]
  (with-open [code-stmt (insert-stmt :code [:value :id :ordering :field_id])
              position-stmt (insert-stmt :field_position
                                         [:field_id :row :bin :chrom :chromStart
                                          :chromEnd :strand])
              gene-stmt (insert-stmt :field_gene [:field_id :row :gene])
              feature-stmt (insert-stmt :feature [:id :field_id :shortTitle
                                                  :longTitle :priority
                                                  :valueType :visibility])
              field-stmt (insert-stmt :field [:id :dataset_id :name])
              field-score-stmt (insert-stmt :field_score [:field_id :scores :i])
              field-seq-stmt (sequence-stmt "FIELD_IDS")
              feature-seq-stmt (sequence-stmt "FEATURE_IDS")]
    ; XXX change these -seq functions to do queries returning vectors &
    ; simplify the destructuring. Or better, stop asking the db for ids and
    ; generate them ourselves. Getting ids is the slowest bit, even with a large
    ; cache.
    (let [feature-seq #(delay (-> (feature-seq-stmt []) first :i))
          field-seq #(delay (-> (field-seq-stmt []) first :i))
          ; XXX try memoization again?
          ;        score-id-fn (memoize-key (comp ahashable first) scores-seq)

          writer {:insert-field field-stmt
                  :insert-field-score field-score-stmt
                  :insert-position position-stmt
                  :insert-feature feature-stmt
                  :insert-code code-stmt
                  :insert-gene gene-stmt}
          row-count (atom 0)
          data (matrix-fn)
          warnings (atom (meta data))
          last-log (atom 0)
          inserts (lazy-mapcat #(do
                                  (swap! row-count max (count (force (:rows %))))
                                  (load-field-feature
                                    feature-seq
                                    field-seq
                                    dataset-id
                                    %))
                               data)]

      (doseq [insert-batch (partition-all batch-size inserts)]
        (jdbcd/transaction
          (doseq [[insert-type values] insert-batch]
            (when (= :insert-field insert-type)
              (let [t (System/currentTimeMillis)]
                (when (> (- t @last-log) 10000)
                  (trace "writing" (:name values))
                  (reset! last-log t))))
            (let [values (run-delays values)
                  values (if (and (= :insert-field insert-type)
                                  (> (.length ^String (:name values)) max-probe-length))
                           (do
                             (swap! warnings update-in ["too-long-probes"] conj (:name values))
                             (warn "Too-long probe" (:name values))
                             (update-in values [:name] trunc max-probe-length))
                           values)
                  write (fn write [i] ; on field inserts we retry on certain failures, with sanitized probe ids.
                          (let [values (if (= :insert-field insert-type)
                                         (update-in values [:name] str (numbered-suffix i))
                                         values)]
                            ; On primary key violations, try to rename probes. If we can't find a usable
                            ; name after max-insert-retry attempts, let the dataset error out. It's probably
                            ; a severe error, like wrong column being used as probe id.
                            (try
                              ((writer insert-type) values)
                              (catch JdbcBatchUpdateException ex
                                (cond
                                  (and (= :insert-field insert-type)
                                       ; h2 concats the english message after
                                       ; the localized message, so using
                                       ; .contains.
                                       (.contains (.getMessage ex) "Unique index")
                                       (< i max-insert-retry))
                                  (do
                                    (swap! warnings update-in ["duplicate-probes"] conj (:name values))
                                    (warn "Duplicate probe" (:name values))
                                    (write (inc i)))
                                  :else (throw ex))))))]
              (write 1)))))
      {:rows @row-count
       :warnings @warnings})))
;
;
; Table writer that updates one table at a time by writing to temporary files.
;
; Currently not functioning. Needs update for new schema.

(defn- insert-field-out [^java.io.BufferedWriter out seqfn dataset-id name]
  (let [field-id (seqfn)]
    (.write out (str field-id "\t" dataset-id "\t" name "\n"))
    field-id))

(defn- insert-scores-out [^java.io.BufferedWriter out seqfn slist]
  (let [score-id (seqfn)]
    (.write out (str score-id "\t" (:hex slist) "\n"))
    score-id))

(defn- insert-field-score-out [^java.io.BufferedWriter out field-id i score-id]
  (.write out (str field-id "\t" i "\t" score-id "\n")))

(def ^:private table-writer #'table-writer-default)

(let [fmtr (formatter "yyyy-MM-dd hh:mm:ss")]
  (defn- format-timestamp ^String [timestamp]
    (unparse fmtr timestamp)))

(defn- fmt-time [file-meta]
  (assoc file-meta :time
         (. java.sql.Timestamp valueOf (format-timestamp (:time file-meta)))))

; XXX test this
; XXX  and call it somewhere.
(defn- clean-sources []
  (jdbcd/delete-rows :source
                     ["`id` NOT IN (SELECT DISTINCT `source_id` FROM `dataset_source)"]))

(defn- load-related-sources [table id-key id files]
  (jdbcd/delete-rows table [(str (name id-key) " = ?") id])
  (let [sources (map #(-> (jdbcd/insert-record :source %) KEY-ID) files)]
    (doseq [source-id sources]
      (jdbcd/insert-record table {id-key id :source_id source-id}))))

(defn- related-sources [id]
  (jdbcd/with-query-results rows
    ["SELECT `source`.`name`, `hash`, `time`
     FROM `dataset`
     JOIN `dataset_source` ON `dataset_id` = `dataset`.`id`
     JOIN `source` on `source_id` = `source`.`id`
     WHERE `dataset`.`id` = ?" id]
    (vec rows)))

(defn- record-exception [dataset-id ex]
  (jdbcd/with-query-results rows
    ["SELECT `text` FROM `dataset` WHERE `dataset`.`id` = ?" dataset-id]
    (let [[{text :text :or {text "{}"}}] rows
          metadata (json/read-str text)]
      (jdbcd/update-values :dataset ["`id` = ?" dataset-id] {:text
                                                             (json/write-str
                                                               (assoc metadata "error" (str ex))
                                                               :escape-slash false)}))))

(defn- with-load-status* [dataset-id func]
  (try
    (jdbcd/update-values :dataset ["`id` = ?" dataset-id] {:status "loading"})
    (func)
    (jdbcd/update-values :dataset ["`id` = ?" dataset-id] {:status "loaded"})
    (catch Exception ex
      (warn ex "Error loading dataset")
      (jdbcd/update-values :dataset ["`id` = ?" dataset-id] {:status "error"})
      (record-exception dataset-id ex))))

(defmacro with-load-status [dataset-id & body]
  `(with-load-status* ~dataset-id (fn [] ~@body))) ; XXX need :^once?

(defn- with-delete-status* [dataset-id func]
  (try
    (jdbcd/update-values :dataset ["`id` = ?" dataset-id] {:status "deleting"})
    (func)
    (catch Exception ex
      (warn ex "Error deleting dataset")
      (jdbcd/update-values :dataset ["`id` = ?" dataset-id] {:status "error"})
      (record-exception dataset-id ex))))

(defmacro with-delete-status [dataset-id & body]
  `(with-delete-status* ~dataset-id (fn [] ~@body))) ; XXX need :^once?

(def max-warnings 10)
(defn- truncate-warnings [warnings]
  (-> warnings
      (update-in ["duplicate-keys" "sampleID"] #(take max-warnings %))
      (update-in ["duplicate-probes"] #(take max-warnings %))))

; jdbcd/transaction will create a closure of our parameters,
; so we can't pass in the matrix seq w/o blowing the heap. We
; pass in a function to return the seq, instead.

; files: the files this matrix was loaded from, e.g. clinical & genomic matrix, plus their timestamps & file hashes
; metadata: the metadata for the matrix (should cgdata mix in the clinical metadata?)
; matrix-fn: function to return seq of data rows
; features: field metadata. Need a better name for this.

(defn- load-dataset
  "Load matrix file and metadata. Skips the data load if file hashes are unchanged,
  and 'force' is false. Metadata is always updated."
  ([mname files metadata matrix-fn features]
   (load-dataset mname files metadata matrix-fn features false))

  ([mname files metadata matrix-fn features force]
   (profile
     :trace
     :dataset-load
     (let [dataset-id (jdbcd/transaction (merge-m-ent mname metadata))
           files (map fmt-time files)]
       (with-load-status dataset-id
         (when (or force
                   (not (= (set files) (set (related-sources dataset-id)))))
           (p :dataset-clear
              (clear-by-exp dataset-id))
           (p :dataset-sources
              (load-related-sources
                :dataset_source :dataset_id dataset-id files))
           (p :dataset-table
              (let [{:keys [rows warnings]} (table-writer *tmp-dir* dataset-id matrix-fn)]
                (jdbcd/transaction
                  (jdbcd/update-values :dataset ["`id` = ?" dataset-id] {:rows rows})
                  (when warnings
                    (merge-m-ent mname (assoc metadata :loader (truncate-warnings warnings)))))))))))))

(defn- dataset-by-name [dname & kprops]
  (let [props (clojure.string/join "," (map name kprops))]
    (jdbcd/with-query-results rows
      [(format "SELECT %s
               FROM `dataset`
               WHERE `name` = ?" props) (str dname)]
      (-> rows first))))

(def dataset-id-by-name (comp :id #(dataset-by-name % :id)))

(defn delete-dataset [dataset]
  (let [dataset-id (dataset-id-by-name dataset)]
    (if dataset-id
      (with-delete-status dataset-id
        (clear-by-exp dataset-id)
        (jdbcd/do-commands
          (format "DELETE FROM `dataset` WHERE `id` = %d" dataset-id)))
      (info "Did not delete unknown dataset" dataset))))

(defn create-db [file & [{:keys [classname subprotocol make-pool?]
                          :or {classname "org.h2.Driver"
                               subprotocol "h2"
                               make-pool? true}}]]

  (let [spec {:classname classname
              :subprotocol subprotocol
              :subname file
              :conn-customizer "conn_customizer"
              :make-pool? make-pool?}]
       (if make-pool? (delay (pool spec)) (delay (-> spec)))))

; XXX should sanitize the query
(defn- run-query [q]
  (jdbcd/with-query-results rows (spy :trace (hsql/format q))
    (vec rows)))

;
; Code queries
;

(def ^:private codes-for-field-query
  (cached-statement
    "SELECT `ordering`, `value`
    FROM `field`
    JOIN `code` ON `field_id` = `field`.`id`
    WHERE  `name` = ? AND `dataset_id` = ?"
    true))

(defn- codes-for-field [dataset-id field]
  (->> (codes-for-field-query field dataset-id)
       (map #(mapv % [:value :ordering]))
       (into {})))

;
; All rows queries.
;

; All bins for the given fields.

; XXX inner join here? really? not just a where?
(def ^:private all-rows-query
   (cached-statement
     "SELECT `name`, `i`, `scores` FROM
       (SELECT `name`, `id` FROM
         `field`
         INNER JOIN (TABLE(`name2` VARCHAR = ?) T) ON `name2` = `field`.`name`
       WHERE `dataset_id` = ?) AS P
     LEFT JOIN `field_score` ON `P`.`id` = `field_score`.`field_id`"
     true))

; bins will all be the same size, except the last one. bin size is thus
; max of count of first two bins.
; Loop over bins, copying into pre-allocated output array.
; {"foxm1" [{:name "foxm1" :scores <bytes>} ... ]
(defn concat-field-bins [bins]
  (let [float-bins (mapv #(update-in % [:scores] score-decode) bins)
        sizes (mapv #(count (:scores %)) float-bins)
        bin-size (apply max (take 2 sizes))
        row-count (apply + sizes)
        out (float-array row-count)]
    (doseq [{:keys [scores i]} float-bins]
      (System/arraycopy scores 0 out (* i bin-size) (count scores))) ; XXX mutate "out" in place
    out))

; return float arrays of all rows for the given fields
(defn- all-rows [dataset-id fields]
  (let [field-bins (->> (all-rows-query (to-array fields) dataset-id)
                        (group-by :name))]
    (into {} (for [[k v] field-bins] [k (concat-field-bins v)]))))

;
;
;

(defn bin-offset [i]
  [(quot i bin-size) (rem i bin-size)])

; merge bin number and bin offset for a row
(defn- merge-bin-off [{i :i :as row}]
  (merge row
         {:bin (quot i bin-size)
          :off (rem i bin-size)}))

; Returns mapping from input bin offset to output buffer for a
; given row.
(defn- bin-mapping [{off :off row :order}]
  [off row])


; Gets called once for each bin in the request.
; order is row -> order in output buffer.
; bin is the bin for the given set of rows
; rows is ({:i 12 :bin 1 :off 1}, ... ), where all :bin are the same
(defn- pick-rows-fn [[bin rows]]
  [bin (partial ashuffle-float
                (map bin-mapping rows))])

; Take an ordered list of requested rows, and generate fns to copy
; values from a score bin to an output array. Returns one function
; for each score bin in the request.
;
; rows ({:i 12 :bin 1 :off 1}, ... )
(defn- pick-rows-fns [rows]
  (let [by-bin (group-by :bin rows)]
    (apply hash-map (mapcat pick-rows-fn by-bin))))

; Take map of bin copy fns, list of scores rows, and a map of output arrays,
; copying the scores to the output arrays via the bin copy fns.
(defn- build-score-arrays [rows bfns out]
  (doseq [{i :i scores :scores field :name} rows]
    ((bfns i) (out field) scores))
  out)

(defn- col-arrays [columns n]
  (zipmap columns (repeatedly (partial float-array n Double/NaN))))

; Doing an inner join on a table literal, instead of a big IN clause, so it will hit
; the index.

; XXX performance of the :in clause?
; XXX make a prepared statement
; Instead of building a custom query each time, this can be
; rewritten as a join against a TABLE literal, or as
; IN (SELECT * FROM TABLE(x INT = (?))). Should check performance
; of both & drop the (format) call.
(def ^:private dataset-fields-query
  "SELECT name, i, scores
   FROM (SELECT  `field`.`name`, `field`.`id`
         FROM `field`
         INNER JOIN TABLE(name varchar=?) T ON T.`name`=`field`.`name`
         WHERE (`field`.`dataset_id` = ?)) P
   LEFT JOIN `field_score` ON P.id = `field_score`.`field_id`
   WHERE `field_score`.`i` IN (%s)")

(defn- select-scores-full [dataset-id columns bins]
  (let [q (format dataset-fields-query
                  (clojure.string/join ","
                                       (repeat (count bins) "?")))
        c (to-array (map str columns))
        i bins]
    (jdbcd/with-query-results rows (vec (concat [q c dataset-id] i))
      (vec rows))))

; Returns rows from (dataset-id column-name) matching
; values. Returns a hashmap for each matching  row.
; { :i     The implicit index in the column as stored on disk.
;   ;order The order of the row the result set.
;   :value The value of the row.
; }
(defn- rows-matching [dataset-id column-name values]
  (let [val-set (set values)]
    (->> column-name
         (vector)
         (all-rows dataset-id)                 ; Retrieve the full column
         (vals)
         (first)
                                               ; calculate row index in the column
         (map #(-> {:i %1 :value %2}) (range)) ; [{:i 0 :value 1.2} ...]
         (filter (comp val-set :value))        ; filter rows not matching the request.
         (group-by :value)
         (#(map (fn [g] (get % g [nil])) values)) ; arrange by output order.
         (apply concat)                           ; flatten groups.
         (mapv #(assoc %2 :order %1) (range)))))  ; assoc row output order.

(defn- float-nil [x]
  (when x (float x)))

; Get codes for samples in request
; Get the entire sample column
; Find the rows matching the samples, ordered by sample list
;   Have to scan the sample column, group by sample, and map over the ordering
; Build shuffle functions, per bin, to copy bins to output buffer
; Run scores query
; Copy to output buffer

(defn- genomic-read-req [req]
  (let [{:keys [samples table columns]} req
        dataset-id (dataset-id-by-name table)
        samp->code (codes-for-field dataset-id "sampleID")
        order (mapv (comp float-nil samp->code) samples)     ; codes in request order
        rows (rows-matching dataset-id "sampleID" order)
        rows-to-copy (->> rows
                          (filter :value)
                          (mapv merge-bin-off))

        bins (mapv :bin rows-to-copy)        ; list of bins to pull.
        bfns (pick-rows-fns rows-to-copy)]   ; make fns that map from input bin to
                                             ; output buffer, one fn per input bin.

    (-> (select-scores-full dataset-id columns (distinct bins))
        (#(mapv cvt-scores %))
        (build-score-arrays bfns (col-arrays columns (count rows))))))


;
;
; Methods for providing a sql abstraction over the column store.
;
;


(def ^:private field-ids-query
  (cached-statement
    "SELECT `id`, `name` FROM `field`
     WHERE `dataset_id` = ? AND `name` IN (SELECT * FROM TABLE(x VARCHAR = (?)))"
    true))

(defn- fetch-field-ids [dataset-id names]
  (when-let [ids (field-ids-query dataset-id names)]
    (map (juxt :name :id) ids)))

;

(def ^:private codes-for-values-query
  (cached-statement
    "SELECT `ordering`, `value`
    FROM `code`
    JOIN TABLE(x INT = (?)) V on `x` = `ordering`
    WHERE  `field_id` = ?"
    true))

(defn- codes-for-values [field-id values]
  (when-let [codes (codes-for-values-query values field-id)]
    (map (juxt :ordering :value) codes)))

;

(def ^:private field-bins-query
  (cached-statement
    "SELECT `scores`, `i` FROM `field_score`
     WHERE `field_id` = ? AND `i` IN (SELECT * FROM TABLE(x INT = (?)))"
    true))

(defn- fetch-bins [field-id bins]
  (when-let [scores (field-bins-query field-id bins)]
    (map #(update-in % [:scores] bytes-to-floats) scores)))

;

(def ^:private has-codes-query
  (cached-statement
    "SELECT EXISTS(SELECT `ordering` FROM `code` WHERE `field_id` = ?) hascodes"
    true))

(defn has-codes? [field-id]
  (-> (has-codes-query field-id) first :hascodes))

;

(def ^:private has-genes-query
  (cached-statement
    "SELECT EXISTS(SELECT `gene` FROM `field_gene` WHERE `field_id` = ?) hasgenes"
    true))

(defn has-genes? [field-id]
  (-> (has-genes-query field-id) first :hasgenes))

;

(def ^:private has-position-query
  (cached-statement
    "SELECT EXISTS(SELECT `bin` FROM `field_position` WHERE `field_id` = ?) hasposition"
    true))

(defn has-position? [field-id]
  (-> (has-position-query field-id) first :hasposition))

;

(defn update-codes-cache [codes field-id new-bins cache]
  (match [codes]
         [nil] (update-codes-cache (if (has-codes? field-id)
                                     [:yes {}]
                                     [:no])
                                   field-id new-bins cache)
         [[:no]] codes
         [[:yes code-cache]]
         [:yes
          (let [values (distinct (mapcat cache new-bins))
                missing (filterv #(not (contains? code-cache %)) values)]
            (into code-cache (codes-for-values field-id missing)))]))

(defn update-cache [cache field-id rows]
  (let [bins (distinct (map #(quot % bin-size) rows))
        missing (filterv #(not (contains? cache %)) bins)]
    (if (seq missing)
      (let [new-cache (->> missing
                           (fetch-bins field-id)
                           (map (juxt :i :scores))
                           (into (or cache {})))]
        (update-in new-cache [:codes] update-codes-cache field-id missing new-cache))
      cache)))

;
;
; return fn of rows->vals
;

(defmulti fetch-rows
  (fn [cache rows field-id]
    (cond
      (has-genes? field-id) :gene
      (has-position? field-id) :position
      :else :binned)))

; binned fields

(defmethod fetch-rows :binned [cache rows field-id]
  (swap! cache update-in [field-id] update-cache field-id rows)
  (let [f (@cache field-id)
        getter (match [(:codes f)]
                      ; XXX cast to int here is pretty horrible.
                      ; Really calls for an int array bin in h2.
                      [[:yes vals->codes]] (fn [bin off]
                                             (let [v (aget ^floats (f bin) off)]
                                               (if (. Float isNaN v)
                                                 nil
                                                 (vals->codes (int v)))))
                      [[:no]] (fn [bin off] (get (f bin) off)))]
    (fn [row]
      (apply getter (bin-offset row)))))

(def ^:private field-genes-by-row-query
  (cached-statement
    "SELECT `row`, `gene` FROM `field_gene`
    INNER JOIN TABLE(x VARCHAR = (?)) X ON X.x = `row`
    WHERE `field_id` = ?"
    true))

(defn field-genes-by-row [field-id rows]
  (->> (field-genes-by-row-query rows field-id)
       (group-by :row)
       (map (fn [[k rows]] [k (mapv :gene rows)]))))

; gene fields

(defmethod fetch-rows :gene [cache rows field-id]
  (into {} (field-genes-by-row field-id rows)))

(def ^:private field-position-by-row-query
  (cached-statement
    "SELECT `row`, `chrom`, `chromStart`, `chromEnd`, `strand` FROM `field_position`
    INNER JOIN TABLE(x VARCHAR = (?)) X ON X.x = `row`
    WHERE `field_id` = ?"
    true))

; position fields

(defn field-position-by-row [field-id rows]
  (->> (field-position-by-row-query rows field-id)
      (map (fn [{:keys [row] :as values}] [row (dissoc values :row)]))))

(defmethod fetch-rows :position [cache rows field-id]
  (into {} (field-position-by-row field-id rows)))

;
; Indexed field lookups (gene & position)
;

(defn has-index? [fields field]
  (let [field-id (fields field)]
    (or (has-genes? field-id) (has-position? field-id))))

; gene

; Selecting `gene` as well adds a lot to the query time. Might
; need it if we need to cache gene names.
(def ^:private field-genes-query
  (cached-statement
    "SELECT `row` FROM `field_gene`
    INNER JOIN TABLE(x VARCHAR = (?)) X ON X.x = `gene`
    WHERE `field_id` = ?"
    true))

(defn field-genes [field-id genes]
  (->> (field-genes-query genes field-id)
      (map :row)))

(defn fetch-genes [field-id values]
  (into (i/int-set) (field-genes field-id values)))

; position

(def ^:private field-position-one-query
  (cached-statement
    "SELECT `row` FROM `field_position`
    WHERE `field_id` = ? AND `chrom` = ? AND
    (`bin` >= ? AND `bin` <= ?) AND `chromStart` <= ? AND `chromEnd` >= ?"
    true))

; [:in "position" [["chr17" 100 20000]]]
(defn field-position [field-id values]
  (->>
    (mapcat (fn [[chr start end]]
              (let [istart (int start)
                    iend (int end)
                    bins (overlapping-bins istart iend)]
                (mapcat (fn [[s e]] (field-position-one-query field-id chr s e iend istart)) bins)))
            values)
    (map :row)))

(defn fetch-position [field-id values]
  (into (i/int-set) (field-position field-id values)))

; fetch-indexed doesn't cache, and it's unclear
; if we should cache the indexed fields during 'restrict'.
; h2 also has caches, so it might be better to rely on them,
; simply doing the query again if we need it.

; Otherwise, we might want to inform the cache of what columns are
; required later during 'restrict', or in 'project', so
; we know if they should be cached.

(defn fetch-indexed [fields field values]
  (let [field-id (fields field)]
    (cond
      (has-position? field-id) (fetch-position field-id values)
      (has-genes? field-id) (fetch-genes field-id values))))

;
; sql eval methods
;

; might want to use a reader literal for field names, so we
; find them w/o knowing syntax.
(defn collect-fields [exp]
  (match [exp]
         [[:in field _]] [field]
         [[:in arrmod field _]] [field]
         [[:and & subexps]] (mapcat collect-fields subexps)
         [[:or & subexps]] (mapcat collect-fields subexps)
         [nil] []))

;
; Computing the "set of all rows" to begin a query was taking much too long
; (minutes, for tens of millions of rows). Switching from sorted-set to int-set
; brought this down to tens of seconds, which was still too slow. Here we
; generate the int-set in parallel & merge with i/union, which is fast. This
; brings the time down to a couple seconds for tens of millions of rows. We
; then, additionally, cache the set, and re-use it if possible. We can
; quickly create smaller sets with i/range, so we memoize such that we
; compute a new base set only if the current memoized size is too small.
;
; Note that this means that if we load a very large dataset, then delete it,
; the memory required to represent all the rows of that dataset is not released
; until the server is restarted.
;
; Alternative fixes might be 1) use an agent to always maintain the largest
; "set of all rows" required for the loaded datasets, updating on dataset add/remove;
; 2) using a different representation of contiguous rows, e.g. [start, end]. We
; would need to review the down-stream performance implications of this (i.e. computing
; union and intersection, generating db queries, etc.)

(def block-size 1e6)

(defn int-set-bins [n]
  (let [blocks (+ (quot n block-size) (if (= 0 (rem n block-size)) 0 1))]
    (map (fn [i] [(* i block-size) (min (* (+ i 1) block-size) n)]) (range blocks))))

(defn set-of-all [n]
  (let [blocks (/ n block-size)]
    (reduce #(i/union % %2)
            (pmap (fn [[start end]] (into (i/int-set) (range start end))) (int-set-bins n)))))

(defn mem-min [f]
  (let [mem (atom nil)]
    (fn [n]
      (if (> n (count @mem))
        (let [ret (f n)]
          (reset! mem ret)
          ret)
        (i/range @mem 0 (- n 1))))))

(def set-of-all-cache (mem-min set-of-all))

; XXX Look at memory use of pulling :gene field. If it's not
; interned, it could be expensive.
; XXX Map keywords to strings, for dataset & field names?
; XXX Add some param guards, esp. unknown field.
(defn eval-sql [{[from] :from where :where select :select :as exp}]
  (let [{dataset-id :id N :rows} (dataset-by-name from :id :rows)
        fields (into {} (fetch-field-ids dataset-id
                                         (into (collect-fields where) select)))
        cache (atom {})
        fetch (fn [rows field]
                (if (not-empty rows)
                  (fetch-rows cache rows (fields field))
                  {}))]
   (evaluate (set-of-all-cache N) {:fetch fetch
                                           :fetch-indexed (partial fetch-indexed fields)
                                           :indexed? (partial has-index? fields)} exp)))

;(jdbcd/with-connection @(:db cavm.core/testdb)
;   (eval-sql {:select ["sampleID"] :from ["BRCA1"] :where [:in "sampleID"
;                                                            ["HG00122" "NA07056" "HG01870" "NA18949" "HG02375"
;                                                             "HG00150" "NA18528" "HG02724"]]}))

;(jdbcd/with-connection @(:db cavm.core/testdb)
;   (eval-sql {:select ["alt"] :from ["BRCA1"] :where [:in "position" [["chr17" 10000 100000000]]]}))

;
;
;
;
;
;

; Each req is a map of
;  :table "tablename"
;  :columns '("column1" "column2")
;  :samples '("sample1" "sample2")
; We merge into 'data the columns that we can resolve, like
;  :data { 'column1 [1.1 1.2] 'column2 [2.1 2.2] }
(defn- genomic-source [reqs]
  (map #(update-in % [:data] merge (genomic-read-req %)) reqs))

;
; hand-spun migrations.
;

(def column-len-query
  "SELECT character_maximum_length as len
   FROM  information_schema.columns as c
   WHERE c.table_name = ? AND c.column_name = ?")

(def bad-probemap-query
  "SELECT `dataset`.`name`
   FROM `dataset`
   LEFT JOIN `dataset` as d ON d.`name` = `dataset`.`probemap`
   WHERE `dataset`.`probeMap` IS NOT NULL AND d.`name` IS NULL")

(defn delete-if-probemap-invalid []
  (jdbcd/with-query-results
    bad-datasets
    [bad-probemap-query]
    (doseq [{dataset :name} bad-datasets]
      (delete-dataset dataset))))

; dataset.probemap refers to a dataset.name, but previously
; had different data size, and was not enforced as a foreign
; key. Check for old data size, and run migration if it's old
;
; The migration will delete datasets that have invalid probemap,
; and update the field.
(defn migrate-probemap []
  (jdbcd/with-query-results
    pm-key
    [column-len-query "DATASET" "PROBEMAP"]
    (when (and (seq pm-key) (= (:len (first pm-key)) 255))
      (info "Migrating probeMap field")
      (delete-if-probemap-invalid)
      (jdbcd/do-commands
        "AlTER TABLE `dataset` ALTER COLUMN `probeMap` VARCHAR(1000)"
        "ALTER TABLE `dataset` ADD FOREIGN KEY (`probeMap`) REFERENCES `dataset` (`name`)"))))

(defn- migrate []
  (migrate-probemap))
  ; ...add more here


(defn- create[]
  (jdbcd/transaction
    (apply jdbcd/do-commands cohorts-table)
    (apply jdbcd/do-commands source-table)
    (apply jdbcd/do-commands dataset-table)
    (apply jdbcd/do-commands dataset-source-table)
    (apply jdbcd/do-commands field-table)
    (apply jdbcd/do-commands field-score-table)
    (apply jdbcd/do-commands feature-table)
    (apply jdbcd/do-commands code-table)
    (apply jdbcd/do-commands field-position-table)
    (apply jdbcd/do-commands field-gene-table))
  (migrate))

;
; add TABLE handling for honeysql
;

(defmethod hsqlfmt/format-clause :table [[_ [fields alias]] _]
  (str "TABLE("
       (clojure.string/join ", "
        (map (fn [[name type values]]
               (str (hsqlfmt/to-sql name) " " (hsqlfmt/to-sql type) "="
                    (hsqlfmt/paren-wrap (clojure.string/join ", " (map hsqlfmt/to-sql values))))) fields))
       ") " (hsqlfmt/to-sql alias)))

; Fix GROUP_CONCAT in honeysql
(defn- format-concat
  ([value {:keys [order separator]}]
   (let [oclause (if (nil? order)
                   ""
                   (str " order by " (hsqlfmt/to-sql order)))
         sclause (if (nil? separator)
                   ""
                   (str " separator " (hsqlfmt/to-sql separator)))]
     (str "GROUP_CONCAT(" (hsqlfmt/to-sql value) oclause sclause ")"))))

(defmethod hsqlfmt/fn-handler "group_concat" [_ value & args]
  (format-concat value args))

(defrecord H2Db [db])

(extend-protocol XenaDb H2Db
  (write-matrix [this mname files metadata data-fn features always]
    (jdbcd/with-connection @(:db this)
      (load-dataset mname files metadata data-fn features always)))
  (delete-matrix [this mname]
    (jdbcd/with-connection @(:db this)
      (delete-dataset mname)))
  (run-query [this query]
    (jdbcd/with-connection @(:db this)
      (run-query query)))
  (column-query [this query]
    (jdbcd/with-connection @(:db this)
      (eval-sql query)))
  (fetch [this reqs]
    (jdbcd/with-connection @(:db this)
      (doall (genomic-source reqs))))
  (close [this] ; XXX test for pool, or datasource before running?
    (.close ^ComboPooledDataSource (:datasource @(:db this)) false)))

(defn create-xenadb [& args]
  (let [db (apply create-db args)]
    (jdbcd/with-connection @db (create))
    (H2Db. db)))
