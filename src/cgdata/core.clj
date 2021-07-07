(ns
  ^{:author "Brian Craft"
    :doc "Library for reading cgdata and related file formats."}
  cgdata.core
  (:require [clojure.data.json :as json])
  (:require [clojure.string :as s])
  (:require [clojure.java.io :as io])
  (:require [clojure-csv.core :as csv])
  (:require [me.raynes.fs :as fs])
  (:require clojure.pprint)
  (:require [cavm.fs-utils :refer [normalized-path relativize]]) ; should copy these fns if releasing cgdata stand-alone
  (:require [cavm.chrom-pos :refer [chrom-pos-vec]]) ; move to this directory?
  (:require [cavm.lazy-utils :refer [consume-vec]])
  (:require [clojure.set])
  (:gen-class))

;
; Utility functions
;

(defn map-invert
  "Invert the keys/values of a map"
  [m]
  (into {} (map (fn [[k v]] [v k]) m)))

; apply f to values of map
(defn- fmap [f m]
  (into {} (for [[k v] m] [k (f v)])))

(defn intern-coll
  "Intern values of a collection, returning the original collection
  as a list of indices, and a map from values to the indices. Invert
  the map to perform lookups by index."
  [s]
  (loop [s s
         codes {}
         out (vector-of :int) ; XXX does this work with consume-vec?
         i 0]
    (if-let [v (first s)]
      (if (contains? codes v)
        (recur (rest s) codes (conj out (codes v)) i)
        (recur (rest s) (assoc codes v i) (conj out i) (inc i)))
      {:codes codes :values out})))

(defn range-from [i]
  (range i Double/POSITIVE_INFINITY 1))

(defn strcpy [s]
  (String. ^String s))

(defn- split-no-empty [in pat] ; XXX Does this need to handle quotes?
  (filter #(not (= "" %)) (map #(s/trim %) (s/split in pat))))

(defn intern-csv-coll
  "Intern values of a collection of csv, returning the original collection
  as a list of arrays of indices, and a map from indices to values."
  [s]
  (loop [s s
         codes {}
         out []
         i 0]
    (if-let [line (first s)]
      (let [vs (split-no-empty line #",")
            to-add (map strcpy (filter #(not (contains? codes %)) vs))
            new-codes (into codes (map vector to-add (range-from i)))
            new-vals (long-array (map new-codes vs))]
        (recur (rest s) new-codes (conj out new-vals) (+ i (count to-add))))
      {:codes (map-invert codes) :values out})))

(defn- chunked-pmap [f coll]
  (->> coll
       (partition-all 250)
       (pmap (fn [chunk] (doall (map f chunk))))
       (apply concat)))

(defn- drop-quotes [s]
  (.replace ^String s "\"" ""))

(defn- tabbed [line]
  (mapv #(drop-quotes (s/trim %)) (s/split line #"\t")))

(defn- not-blank? [line]
  (not (re-matches #"\s*" line)))

(defn- comment? [line]
  (re-matches #"\s*#.*" line))

(def ^:private not-comment? (comp not comment?))

;
; cgData metadata
;

(defn- all-json [path]
  (let [files (file-seq (io/file path))
        fnames (map str files)]
    (map #(vector (s/replace % #"\.json$" "") (json/read-str (slurp %)))
         (filter #(.endsWith ^String % ".json") fnames))))

(defn- json-add [acc [file {n "name" t "type" :as metadata}]]
  (if-let [group (and (= t "probeMap") (metadata "group"))]
    (-> acc
        (assoc-in [t (str group "::" (metadata ":assembly"))] file)
        (assoc-in [t n] file))
    (assoc-in acc [t n] file)))

(defn- json-table [json-list]
  (reduce json-add {} json-list))

(defn- dirname [file]
   (s/replace file #"[^/]*$" ""))

(defn- basename [file]
  (s/replace file #".*/([^/]*)$" "$1"))

(defn- make-absolute [^String path]
  (if (.startsWith path "/")
    path
    (str "/" path)))

(defn- normalize-path [cut file path]
  (if (= (dirname file) (dirname path))
    (basename path)
    (make-absolute (subs path cut))))

; either pass through [k v] or map it to a file if
; it matches an object in the table.
(defn- resolve-reference [normalize table file [k v]]
  (if-let [match (and (.startsWith ^String k ":")
                      (or
                        (get-in table [(subs k 1) v])
                        (get-in table [(subs k 1) (str v "::hg18")])))]
    [k (normalize file match)]
    [k v]))

; rewrite table to use files instead of object names
(defn- resolve-references [normalize table file metadata]
  (into {}
        (map #(resolve-reference normalize table file %) metadata)))

(defn- write-json [file json]
  (with-open [writer (io/writer file)]
    (binding [*out* writer]
      (json/pprint json :escape-slash false))))

(defn fix-json
  "Utility for rewriting references in cgdata metadata, so
  they refer to file paths, not abstract identifiers."
  [root]
  (let [files (all-json root)
        table (json-table files)
        normalize (partial normalize-path (count root))]
    (doseq [[file data] files]
      (write-json (str file ".json")
                  (resolve-references normalize table file data)))))

;
; cgData clinicalFeature
;

(defmulti ^:private feature-line
  (fn [acc line] (second line)))

(defmethod ^:private feature-line :default
  [acc [feature attr value]]
  (assoc-in acc [feature attr] value))

(defn- add-state [curr value]
  (conj (vec curr) value))

(defmethod ^:private feature-line "state"
  [acc [feature attr value]]
  (update-in acc [feature attr] add-state value))

(defn- parse-order [order]
  (first (csv/parse-csv order)))

(defmethod ^:private feature-line "stateOrder"
  [acc [feature attr value]]
  (assoc-in acc [feature attr] (parse-order value)))

(defn- feature-map [lines]
  "Read tab-split clincalFeature rows into a map"
  (reduce feature-line {} lines))

(defn- calc-order
  "Take feature order from 'stateOrder', or file order of 'state' rows."
  [feature]
  (if-let [state (or (feature "stateOrder") (feature "state"))]
    (-> feature
        (assoc :order (into {} (map #(vector %1 %2) state (range))))
        (assoc :state state))
    feature))

(defn- feature-file [file]
  (when (and file (.exists (io/as-file file)))
    (->> file
         (slurp)
         (#(s/split % #"\n"))     ; split lines
         (map #(s/split % #"\t")) ; split tabs
         (feature-map)
         (fmap calc-order))))

;
; cgData genomicMatrix
;

(defn- parseFloatNA [str]
  (if (or (= str "NA") (= str "nan") (= str ""))
    Double/NaN
    (Float/parseFloat str)))

(defn- parseChromCSV-1 [str i]
  (->> str
       csv/parse-csv
       first
       (filter #(not (s/blank? %)))
       (mapv #(+ (long (Double/parseDouble (s/trim %))) i))
       (s/join ",")))

(defn- parseChromCSV [str]
  (parseChromCSV-1 str 0))

(defn- parseChromCSV0 [str]
  (parseChromCSV-1 str 1))

; If we have no feature description, we try "float". If that
; fails, we try "category" by passing in a hint. We do not
; allow the hint to override a feature description, since that
; would mask curation errors. multimethod may not be the best
; mechanism for this policy.

(defmulti ^:private data-line
  "(id val val val val) -> seq of parsed values)

  Return from a split probe line a probe name,
  feature definition, data type, and seq of floats"
  (fn [features cols & [hint]]
    (or (get (get features (first cols)) "valueType")
        hint)))

(defn dup-indexes
  "Find indexes of duplicate values in coll."
  [coll]
  (loop [i 0 c coll in #{} dups #{}]
    (if-let [v (first c)]
      (if (in v)
        (recur (inc i) (rest c) in (conj dups i))
        (recur (inc i) (rest c) (conj in v) dups))
      dups)))

(defn drop-indexes
  "Drop elements from coll by index."
  [indxs coll]
  (into [] (keep-indexed (fn [i v] (when (not (indxs i)) v)) coll)))

(defmethod ^:private data-line :default
  [features cols & [hint]]
  (try ; the body must be eager so we stay in the try/except scope
    (let [feature (get features (first cols))]
      {:field (String. ^String (first cols)) ; copy, because split is evil.
       :feature feature
       :valueType "float"
       :rows (mapv parseFloatNA (rest cols))}) ; eager
    (catch NumberFormatException e
      (data-line features cols "category"))))

; update map with value for NA
(defn- nil-val [order]
  (assoc order "" Double/NaN))

(defn- ad-hoc-order
  "Provide default order from data order in file"
  [feature cols]
  (if (:order feature)
    feature
    (let [state (distinct cols) ; XXX drop ""? This adds "" as a state.
          order (into {} (map vector state (range)))] ; XXX handle all values null?
      (assoc feature :state state :order order))))

(defn- throw-on-nil [x msg & args]
  (when-not x
    (throw (IllegalArgumentException.
             ^String (apply format msg args))))
  x)

(defmethod ^:private data-line "category"
  [features cols & [hint]]
  (let [name (first cols)
        feature (get features name)  ; use 'get' to handle nil
        feature (ad-hoc-order feature (rest cols))
        order (nil-val (:order feature))
        msg "Invalid state %s for feature %s"
        vals (map #(throw-on-nil (order %) msg % name)
                  (rest cols))]

    {:field (String. ^String name) ; copy the string. string/split is evil.
     :feature feature
     :valueType "category"
     :rows vals}))

(defmulti ^:private matrix-data
  "Return seq of scores, probes X samples"
  (fn [metadata features lines] (metadata "type")))

(defn- ammend-header
  "Coerce first column name to sampleID"
  [header]
  (assoc header 0 "sampleID"))

(defn fix-vec [v size]
  (let [c (count v)]
    (cond
      (> c size) (subvec v 0 size)
      (< c size) (apply conj v (repeat (- size c) ""))
      :else v)))


(defn matrix-data-parse
  [metadata features lines parse]
  (let [header (ammend-header (parse (first lines)))
        dup-sample-rows (into #{} (map inc (dup-indexes (rest header))))
        dup-samples (not-empty (map header dup-sample-rows))
        drop-rows (partial drop-indexes dup-sample-rows)
        ncols (count header)]
    (with-meta
      (cons (data-line features (drop-rows header) "category") ; coerce sampleID type
            (pmap #(data-line features (drop-rows (fix-vec (parse %) ncols)))
                   (rest lines)))
      (when dup-samples {:duplicate-keys {:sampleID dup-samples}}))))

(defmethod matrix-data :default
  [metadata features lines]
  (matrix-data-parse metadata features lines tabbed))

(defn- transpose
  "Transpose cells of a tsv, using the first row to fix the number of columns."
  [lines]
  (let [pcount (count (first lines))]
    (into [] (for [i (range pcount)]
               (mapv #(get % i "") lines)))))

(defmethod matrix-data "clinicalMatrix"
  [metadata features lines]
  (let [lines (->> lines
                  (map tabbed)
                  (transpose))]
    (matrix-data-parse metadata features lines identity)))

(defn- cgdata-meta [file]
  (let [mfile (io/as-file (str file ".json"))]
    (when (.exists mfile)
      (json/read-str (slurp mfile)))))

(defn- path-from-ref
  "Construct a file path relative to the document root, given a file
  reference and the referring file path. References can be relative to
  the referring file, or absolute (relative to document root)."
  [docroot referrer file]
  (let [sref (fs/split (normalized-path referrer))
        sfile (fs/split file)
        abs (if (= (first sfile) fs/unix-root)
              (apply io/file docroot (rest sfile))
              (apply io/file (conj (vec (drop-last sref)) file)))]
    (relativize docroot abs)))

(defn references
  "Return map of any references in md to their paths relative to the document root."
  [docroot referrer md]
  (let [refs (->> md
                  (keys)
                  (filter #(.startsWith ^String % ":")))]
    (into {} (map vector refs (map
                                #(when-let [path (md %)] ; don't resolve nil
                                   (str (path-from-ref docroot referrer path)))
                                refs)))))

(defn matrix-file
  "Return a map describing a cgData matrix file. This will read
  any assoicated json or clinicalFeature file."
  [file & {docroot :docroot :or {docroot fs/unix-root}}]
  (let [metadata (or (cgdata-meta file) {"name" file})
        refs (references docroot file metadata)
        cf (refs ":clinicalFeature")
        feature (when cf (feature-file (fs/file docroot cf)))]

    {:metadata metadata  ; json metadata
     :refs refs          ; map of json metadata references to paths relative to docroot
     :features feature   ; slurped clinicalFeatures
     :data-fn (fn [in] (matrix-data metadata feature (line-seq ((:reader in)))))}))

(defn- chr-order [position]
  (mapv position [:chrom :chromStart]))

;
; general tsv reader
;

(defn tsv-rows [in]
  (map tabbed (filter #(and (not-blank? %)
                            (not-comment? %))
                      (line-seq ((:reader in))))))

;
; Fields need to return :row as vectors so we can sort them. They could return lazy
; seqs if we wanted to convert them to vectors before sort. It's better if the
; field does it, so we can infer the correct storage type via 'empty' when sorting.
;
(defmulti field-spec (fn [field _] (:type field)))

(defn category-field
  [parse {:keys [name i]} in]
  (let [row-vals (delay
                   (intern-coll (map #(parse (get % i "")) (tsv-rows in))))]
    {:field name
     :valueType "category"
     :feature (delay {:order (:codes @row-vals)})
     :rows (delay (:values @row-vals))}))

(defmethod field-spec :category
  [col in]
  (category-field #(s/trim (strcpy %)) col in))

(defmethod field-spec :float
  [{:keys [name i]} in]
  {:field name
   :valueType "float" ; XXX why strings instead of keywords?
   :rows (delay
           (reduce #(conj %1 (parseFloatNA (get %2 i "")))
                   (vector-of :float)
                   (tsv-rows in)))})

(defmethod field-spec :chromStartCSV
  [{:keys [start-index] :as col} in]
  (let [parse ([parseChromCSV0 parseChromCSV] start-index)]
    (category-field parse col in)))

(defmethod field-spec :gene
  [{:keys [name i]} in]
  (let [row-vals (delay
                   (intern-csv-coll
                     (map #(get % i "") (tsv-rows in))))]
    {:field name
     :valueType "genes"
     :row-val (fn [row] (map (:codes @row-vals) row))
     :rows (delay (:values @row-vals))}))

(defn normalize-chrom [s]
  (-> (s/trim s)
      (s/replace  #"^(?i)(chr)?([0-9]+)$" "chr$2")
      (s/replace  #"^(?i)(chr)?m$" "chrM")
      (s/replace  #"^(?i)(chr)?y$" "chrY")
      (s/replace  #"^(?i)(chr)?x$" "chrX")))

(let [parsers
      {:chrom normalize-chrom
       :chromStart #(long (Double/parseDouble (s/trim %)))
       :chromStart0 #(+ (long (Double/parseDouble (s/trim %))) 1)
       :chromEnd #(long (Double/parseDouble (s/trim %)))
       :strand #(first (s/trim %))}

      ; This is awkward. Override chromStart if start-index is 0.
      get-parser (fn [ftype start-index]
                   (if (and (= start-index 0) (= ftype :chromStart))
                     (parsers :chromStart0)
                     (parsers ftype)))]

  (defmethod field-spec :position
    [{:keys [name columns start-index]} in]       ; XXX rename columns to fields
    (let [tlist (mapv :header columns)  ; vec of field header
          ilist (mapv :i columns)       ; vec of indexes in row
          plist (mapv #(get-parser (:header %) start-index) columns)] ; vec of parsers for fields
      {:field name
       :valueType "position"
       :rows (delay
               (into (chrom-pos-vec)
                     (map #(into {}
                                 (mapv (fn [t i parse] [t (parse (get % i ""))])
                                       tlist ilist plist))
                          (tsv-rows in))))})))

(defn normalize-column-name [patterns col fixed]
  (if fixed
    fixed
    (if-let [[pat canon] (first (filter #(re-matches (first %) col) patterns))]
      canon
      col)))

; Add default pattern
(defn columns-from-header [patterns header fixed-columns]
  (map #(normalize-column-name patterns %1 %2)
       (tabbed header)
       (concat fixed-columns (repeat nil))))

(defn pick-header
  "Pick first non-blank line. Return index and line"
  [lines]
  (first (keep-indexed
           (fn [i line]
             (when (not-blank? line)
               [i line]))
           lines)))

(defn drop-hash
  "Drop leading hash, e.g. in tsv header"
  [s]
  (clojure.string/replace s #"^\s*#" ""))

(def ^:private position-columns #{:chrom :chromStart :chromEnd :strand})

; [{:header :chrom :i 2} {:header :chromStart :i 3}.. ]
; -> [{:header :position :columns ({:header :chrom :i 2}... )}...]
(defn find-position-field
  [columns]
  (let [unique (into {} (for [[k v] (group-by :header columns)] [k (first v)]))
        matches (select-keys unique position-columns)
        match-set (set (vals matches))]
    (if (every? matches [:chrom :chromStart :chromEnd])
      (into [{:type :position :header :position :columns match-set}]
            (filter (comp not match-set) columns))
      columns)))

; This is a really weak attempt to infer types of position columns. We
; need a better vision.
(defn find-position-fields
  "Rewrite a list of column objects having a :header attribute,
  collating chrom position columns into position fields, having
  :chrom :chromStart :chromEnd and optional :strand. :chrom and
  :strand are reused if necessary."
  [columns start-index]
  (let [poscols (select-keys (group-by :header columns) position-columns)                ; columns matching position types.
        limit (apply max (map count (vals poscols)))
        poscols (update-in poscols [:chrom] #(take limit (concat % (repeat (last %)))))  ; repeat last chrom if necessary.
        poscols (update-in poscols [:strand] #(take limit (concat % (repeat (last %))))) ; repeat last strand if necessary.
        positions (filter #(every? (set (map :header %)) [:chrom :chromStart :chromEnd]) ; find groups with the required fields.
                           (apply map vector (vals poscols)))                            ;   collate into groups.
        consumed (set (apply concat positions))]                                         ; columns used by position fields.
    (into (mapv #(-> {:type :position
                      :start-index start-index
                      :header :position
                      :columns (filter identity %)})                                     ; write out position field specs.
                positions)
          (filter (comp not consumed) columns))))                                        ;   append all remaining fields.

; XXX change this so numbering starts at 2?
(defn- numbered-suffix [n]
  (if (== 0 n) "" (str " (" n ")")))

(defn- add-field [fields field]
  (update-in fields [field] (fnil inc 1)))

(defn- assign-unique-names
  ([fields] (assign-unique-names fields {}))
  ([fields counts]
   (when-let [{header :header :as field} (first fields)]
       (cons (assoc field :name (str (name header) (numbered-suffix (counts header 0))))
             (lazy-seq (assign-unique-names (rest fields)
                                            (add-field counts header)))))))

(defn- resort [rows order]
  (let [rs (force rows)]
    (into (empty rs) (map rs order))))

; Modify multireader to drop c lines
(defn- drop-from-reader [mr c]
  (assoc mr :reader (fn []
                     (let [reader ((:reader mr))]
                       (doseq [_ (range c)]
                         (.readLine ^java.io.BufferedReader reader))
                       reader))))

(defn guess-column-type [in i]
  (try
    (do
      (doall (take 20 (map #(parseFloatNA (get % i ""))
                           (tsv-rows in))))
      :float)
    (catch NumberFormatException e
      :category)))


(defn- tsv-data
  "Return the fields of a tsv file."
  [start-index columns fixed-columns column-types in]
  (let [[header-i header-content] (pick-header (line-seq ((:reader in))))
        in (drop-from-reader in (inc header-i))
        header (-> (columns-from-header columns (drop-hash header-content) fixed-columns)
                   (#(for [[c i] (map vector % (range))]
                       {:header c
                        :type (or (column-types c) (guess-column-type in i))
                        :start-index start-index
                        :i i}))
                   (find-position-fields start-index)
                   assign-unique-names)
        fields (mapv #(field-spec % in) header)] ; vector of field-specs, each holding vector of rows in :rows
    (if (= "position" (get-in fields [0 :valueType]))
      (let [pos-rows (force (get-in fields [0 :rows]))
            order (do
                    (sort-by #(chr-order (pos-rows %)) (into (vector-of :int)
                                                             (range (count pos-rows)))))
            sorted-fields (map #(update-in % [:rows] resort order) (consume-vec fields))]
        sorted-fields)
      (consume-vec fields))))
;
; mutationVector
;

(def ^:private mutation-column-types
  {:sampleID :category
   :chrom :category
   :ref :category
   :chromStart :float
   :chromEnd :float
   :position :position
   :genes :gene
   :reference :category
   :alt :category
   :effect :category
   :dna-vaf :float
   :rna-vaf :float
   :amino-acid :category})

(def mutation-columns
  {#"(?i)sample[ _]*(name|id)?" :sampleID
   #"(?i)chr(om)?" :chrom
   #"(?i)start" :chromStart
   #"(?i)end" :chromEnd
   #"(?i)genes?" :genes
   #"(?i)alt(ernate)?" :alt
   #"(?i)ref(erence)?" :ref
   #"(?i)effect" :effect
   #"(?i)dna[-_ ]*v?af" :dna-vaf
   #"(?i)rna[-_ ]*v?af" :rna-vaf
   #"(?i)amino[-_ ]*acid[-_ ]*(change)?" :amino-acid})

(def mutation-required-columns
  #{"sampleID" "position" "ref" "alt"})

; This could be generalized by calling from tsv-data &
; passing in the require columns.
(defn enforce-mutation-fields [fields]
  (let [field-set (set (map :field fields))]
    (when
      (not
        (clojure.set/subset? mutation-required-columns field-set))
      (throw (IllegalArgumentException. ^String
                                        (str "Missing fields "
                                             (s/join " " (map name (clojure.set/difference
                                                                     mutation-required-columns
                                                                     field-set)))))))
    fields))

(defn map-file [file {docroot :docroot :or {docroot fs/unix-root}} data-fn]
  (let [metadata (cgdata-meta file)
        start-index (get metadata "start_index" 1)
        refs (references docroot file metadata)]
    {:metadata metadata
     :refs refs
     :data-fn (data-fn start-index)}))

(defn mutation-file
  "Return a map describing a cgData mutation file. This will read
  any associated json."
  [file & args]
  (map-file
   file args
   (fn [start-index]
     (fn [in]
       (enforce-mutation-fields
        (tsv-data
         start-index
         mutation-columns
         [:sampleID]
         mutation-column-types
         in))))))

;
; genomic-segment
;
(def ^:private genomic-segment-column-types
  {:sampleID :category
   :chrom :category
   :chromStart :float
   :chromEnd :float
   :position :position
   :value :float})

(def genomic-segment-columns
  {#"(?i)sample[ _]*(name|id)?" :sampleID
   #"(?i)chr(om)?" :chrom
   #"(?i)start" :chromStart
   #"(?i)end" :chromEnd
   #"(?i)strand" :strand
   #"(?i)value" :value})

(defn column-map-file [file args columns types]
  (map-file
   file args
   (fn [start-index]
     (fn [in]
       (tsv-data start-index columns [] types in)))))

(defn genomic-segment-file
  "Return a map describing a cgData genomicSegment file. This will read
  any associated json."
  [file & args]
  (column-map-file
   file args
   genomic-segment-columns
   genomic-segment-column-types))
;
; probemap
;
(def ^:private probemap-column-types
  {:name :category
   :chrom :category
   :chromStart :float
   :chromEnd :float
   :position :position
   :genes :gene
   :thickStart :category
   :thickEnd :category
   :blockCount :category
   :blockSizes :category
   :blockStarts :category})

(def probemap-columns
  {#"(?i)chr(om)?" :chrom
   #"(?i)start" :chromStart
   #"(?i)chromStart" :chromStart
   #"(?i)end" :chromEnd
   #"(?i)chromEnd" :chromEnd
   #"(?i)strand" :strand
   #"(?i)genes?" :genes
   #"(?i)name" :name
   #"(?i)id" :name
   #"(?i)thickStart" :thickStart
   #"(?i)thickEnd" :thickEnd
   #"(?i)blockCount" :blockCount
   #"(?i)blockSizes" :blockSizes
   #"(?i)blockStarts" :blockStarts})

(defn probemap-file
  "Return a map describing a cgData probemap file. This will read
  any associated json."
  [file & args]
  (column-map-file
   file args
   probemap-columns
   probemap-column-types))
;
; gene prediction
;

(def ^:private gene-pred-column-types
  {:bin :float
   :name :category
   :strand :category
   :chrom :category
   :chromStart :float
   :chromEnd :float
   :exonCount :float
   :exonStarts :chromStartCSV
   :exonEnds :category
   :score :float
   :name2 :gene
   :cdsStartStat :category
   :cdsEndStat :category
   :exonFrames :category})

; XXX This has all gone slightly off due to the handling of position
; columns. We map common aliases to canonical names, like chromStart, but
; then extract positions by looking for canonical names. Thus we have to use
; the same name, chromStart, repeatedly if we have multiple positions per row.
; Instead, we should keep names like txStart and map them to abstract types
; like chromStart. We should also allow specifying which columns should be
; collected into positions columns, e.g. (chrom, strand, txStart, txEnd),
; and (chrom, strand, cdsStart, cdsEnd). If the user gives us no hints, we
; should scan for chrom, chromStart, chromEnd types, as we do now. It's a
; useful fail-over in the case that we don't know the file type.
(def ^:private gene-pred-columns
  {#"(?i)bin" :bin
   #"(?i)name" :name
   #"(?i)strand" :strand
   #"(?i)chr(om)?" :chrom
   #"(?i)txStart" :chromStart
   #"(?i)txEnd" :chromEnd
   #"(?i)cdsStart" :chromStart
   #"(?i)cdsEnd" :chromEnd
   #"(?i)exonCount" :exonCount
   #"(?i)exonStarts" :exonStarts
   #"(?i)exonEnds" :exonEnds
   #"(?i)score" :score
   #"(?i)name2" :name2
   #"(?i)cdsStartStat" :cdsStartStat
   #"(?i)cdsEndStat" :cdsEndStat
   #"(?i)exonFrames" :exonFrames})

(defn gene-pred-file
  "Return a map describing a genePred(Ext) file. This will read
  any associated json."
  [file & args]
  (column-map-file
   file args
   gene-pred-columns
   gene-pred-column-types))

;
; cgdata file detector
;

(def ^:private types
  {"mutationVector" ::mutation
   "clinicalMatrix" ::clinical
   "clinicalFeature" ::feature
   "genomicMatrix" ::genomic
   "probeMap" ::probemap
   "genomicSegment" ::segment
   "genePredExt" ::gene-pred
   "genePred" ::gene-pred})

(defn detect-cgdata
  "Detect cgdata files by presence of json metadata. If no type
  is given, assume genomicMatrix"
  [file]
  (when-let [cgmeta (cgdata-meta file)]
    (if-let [cgtype (cgmeta "type")]
      (types cgtype)
      ::genomic)))

;
; tsv file detector
; XXX might want to also files named *.tsv, files with # comment
; headers, check that column counts are consistent, etc.
;

(defn- tabs [line]
  (count (filter #(= % \tab) line)))

(defn ^:private is-tsv?
  "Detect tsv. Requires two non-blank lines, each having
  at least one tab."
  [lines]
  (let [head (take 2 (filter #(re-matches #".*\S.*" %) lines))] ; two non-blank lines
    (and
      (> (count head) 1)
      (every? #(> (tabs %) 0) head))))

; XXX Note that this will pick up all kinds of unrelated files,
; including probeMaps without assocated json metadata.
(defn detect-tsv
  "Return ::tsv if the file is tsv, or nil"
  [file]
  (when (with-open [in (io/reader file)]
         (is-tsv? (line-seq in)))
    ::tsv))
