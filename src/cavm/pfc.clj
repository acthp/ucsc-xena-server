(ns
  ^{:author "Brian Craft"
    :doc "front coding"}
  cavm.pfc
  (:import [java.util.concurrent ArrayBlockingQueue])
  (:import [java.nio ByteBufferAsIntBufferL ByteBuffer])
  (:import [java.io ByteArrayOutputStream])
  (:require [cavm.huffman :as huffman]))

;
; vbyte
;

(defn vbyte-decode-buff [^bytes buff8 offset8]
   (loop [^long i offset8 x 0]
     (let [b (aget buff8 i)]
       (if (= 0 (bit-and b 0x80))
         (recur (inc i) (bit-shift-left (bit-or x b) 7))
         [(bit-or x (bit-and b 0x7f)) (inc i)]))))
(defn vbyte0 [^ByteArrayOutputStream acc ^long i]
  (.write acc (bit-or 0x80 i)))

(defn vbyte1 [^ByteArrayOutputStream acc ^long i]
  (.write acc (bit-shift-right i 7))
  (.write acc (bit-or 0x80 (bit-and i 0x7f))))

(defn vbyte2 [^ByteArrayOutputStream acc ^long i]
  (.write acc (bit-and (bit-shift-right i 14) 0x7f))
  (.write acc (bit-and (bit-shift-right i 7) 0x7f))
  (.write acc (bit-or 0x80 (bit-and i 0x7f))))

(defn vbyte3 [^ByteArrayOutputStream acc ^long i]
  (.write acc (bit-and (bit-shift-right i 21) 0x7f))
  (.write acc (bit-and (bit-shift-right i 14) 0x7f))
  (.write acc (bit-and (bit-shift-right i 7) 0x7f))
  (.write acc (bit-or 0x80 (bit-and i 0x7f))))

; Note we don't handle things over 0x10000000

(defn vbyte-encode [^ByteArrayOutputStream acc ^long i]
    (cond
        (< i 0x80) (vbyte0 acc i)
        (< i 0x4000) (vbyte1 acc i)
        (< i 0x200000) (vbyte2 acc i)
        :else (vbyte3 acc i)))

;
; pfc
;

(defn common-prefix
    ([a b] (common-prefix a b 0))
    ([^String a ^String b ^long n]
     (if (and (> (.length a) n)
              (> (.length b) n)
              (= (.charAt a n) (.charAt b n)))
         (recur a b (inc n))
         n)))

(defn diff-rec [^ByteArrayOutputStream acc ^String a ^String b]
  (let [n (common-prefix a b)]
    (vbyte-encode acc n)
    (let [ba (.getBytes (.substring b n))]
      (.write acc ba 0 (alength ba)))
    (.write acc 0)))

(defn compute-inner [strings]
  (let [sv (into [] strings)
        n (- (count sv) 1)
        acc (ByteArrayOutputStream.)]
    (loop [i 0]
      (if (< i n)
        (do
          (diff-rec acc (get sv i) (get sv (inc i)))
          (recur (inc i)))
        [(.toByteArray acc)]))))

(comment
 (defn decompress [[init diffs]]
     (reductions (fn [^String prev [n suffix]] (str (.substring prev 0 n) suffix))
                 init diffs)))

(defn compress-bin [strings]
    {:header (into-array Byte/TYPE (concat (.getBytes ^String (first strings)) [0])) ; XXX eliminate concat. use implicit zero?
     :inner (compute-inner strings)})

(defn compress-pfc [strings bin-size]
    (let [ordered (sort strings)
          bins (partition-all bin-size ordered)
          compressed (map compress-bin bins)
          sizes (map #(apply + (map count %)) compressed)]
        [sizes (into-array Byte/TYPE (apply concat (apply concat compressed)))]))

; Compress string collection with front-coding and huffman coding.
; Strings are split into bins, and each bin front-coded.
; Each bin is thus a full string ("header"), and a list of [prefix length, suffix],
; "inner strings".
; A huffman dictionary is computed over the inner strings (lens & suffixes),
; and each inner string bin is compressed against the huffman dict.
; A hu-tucker dictionary is computed over the headers, to allow
; binary search over the bins.
; The returned buffer is
; [header dictionary]
; [bin dictionary]
; [bin sizes]
; [concatenated bins]

; Is it worth trying to minimize byte copying?
(defn get-bytes [method this & args]
    (let [out (java.io.ByteArrayOutputStream. 1000)]
        (apply method this out args)
        (.toByteArray out)))

(defn partition-all-v [n v]
  (let [c (count v)
        m (quot c n)
        acc (transient [])]
    (loop [i 0]
      (when (< i m)
        (conj! acc (subvec v (* i n) (* (inc i) n)))
        (recur (inc i))))
    (let [r (rem c n)]
      (when (> r 0)
        (conj! acc (subvec v (- c r) c))))
    (persistent! acc)))

;(partition-all-v 3 [1 2 3 4 5 6 7 8 9 10])
;(partition-all-v 3 [1 2 3 4 5 6 7 8 9 10 11])
;(partition-all-v 3 [1 2 3 4 5 6 7 8 9 10 11 12])
;(partition-all-v 3 [1 2 3 4 5 6 7 8 9 10 11 12 13])

(require '[clojure.core.reducers :as r])
(defn compress-htfc [strings bin-size]
  (let [out (java.io.ByteArrayOutputStream. 1000)
        ordered (sort strings)
        bins (partition-all-v bin-size (into [] ordered))
        ; we reduce this multiple times, so compute it as foldable.
        front-coded (r/foldcat (r/map compress-bin bins))

        ht (huffman/make-hu-tucker (map #(:header %) front-coded))
        ; XXX Does get-bytes make sense? Why do we need to do this?
        compressed-headers (map #(get-bytes huffman/write ht (:header %))
                                 front-coded)
        huff (huffman/make-huffman (r/mapcat :inner front-coded))
        compressed (r/foldcat (r/map #(get-bytes huffman/write huff (:inner %))
                               front-coded))
        sizes (map #(+ (count %1) (count %2)) compressed-headers compressed)
        offsets (reductions + 0 (take (dec (count sizes)) sizes))]
    {:length (count ordered)
     :bin-size bin-size
     :header-dict ht
     :inner-dict huff
     :offsets offsets
     :headers compressed-headers
     :inners compressed}))

(defn serialize-htfc [htfc]
  (let [out (java.io.ByteArrayOutputStream. 1000)
        {:keys [length bin-size header-dict inner-dict offsets headers inners]} htfc]
    (huffman/write-int length out)
    (huffman/write-int bin-size out)
    (huffman/write-dictionary header-dict out)
    (huffman/write-dictionary inner-dict out)
    (huffman/write-int (count offsets) out)
    (doseq [s offsets]
      (huffman/write-int s out))
    (doseq [[header inner]  (map #(-> [%1 %2])
                                 headers
                                 inners)]
      (.write out ^bytes header)
      (.write out ^bytes inner))
    (let [total (.size out)] ; align to word boundary
      (dotimes [i (rem (- 4 (rem total 4)) 4)]
        (.write out 0)))
    (.toByteArray out)))


(defn index-of [^bytes arr start v]
  (loop [i start]
      (if (= (aget arr i) v)
          i
          (recur (inc i)))))

(defn buff-to-string [^bytes buff ^long offset]
  (String. (java.util.Arrays/copyOfRange buff offset (long (index-of buff offset 0)))))

(defn next-str [buff i prev]
  (let [[plen nxt] (vbyte-decode-buff buff i)
        ^String s (buff-to-string buff nxt)
        n (str (.substring ^String prev 0 plen) s)]
    [n (+ nxt (.length s))]))

(defn uncompress-inner [header buff N]
  (let [acc (transient [header])]
    (loop [i 0 p 0 s header]
      (if (< i N)
        (let [[s p] (next-str buff p s)]
          (conj! acc s)
          (recur (inc i) (long p) s))
        (persistent! acc)))))

(defn uncompress-bin [dict i]
  (let [out (ByteArrayOutputStream.)
        {:keys [length bin-size bin-count first-bin bin-offsets header-dict inner-dict ^ByteBuffer buff8 ^ByteBufferAsIntBufferL buff32]} dict
        bin (+ (* 4 first-bin) (.get buff32 (long (+ bin-offsets i))))
        headerP (huffman/decode-to header-dict buff8 bin out 0)
        header (buff-to-string (.toByteArray out) 0)
        remainder (rem length bin-size)
        N (cond (= remainder 0) bin-size
                (= i (dec bin-count)) remainder
                :else bin-size)
        upper (if (= i (dec bin-count)) (.capacity buff8)
                (+ (* 4 first-bin) (.get buff32 (long (+ bin-offsets 1 i)))))]

    (.reset out)
    (huffman/decode-range inner-dict buff8 headerP out upper)
    (uncompress-inner header (.toByteArray out) (dec N)))) ; already have 1st string

(defn uncompress-dict [dict]
  (apply concat (for [i (range 0 (:bin-count dict))] (uncompress-bin dict i))))

(defn htfc-offsets [^ByteBuffer buff8]
  (let [buff32 (.asIntBuffer buff8)
        length (.get buff32 0)
        bin-size (.get buff32 1)
        bin-dict-offset (+ 2 (huffman/ht-dict-len buff32 2))
        bin-count-offset (+ bin-dict-offset (huffman/huff-dict-len buff32 bin-dict-offset))
        bin-count (.get buff32 ^long bin-count-offset)
        first-bin (+ bin-count-offset bin-count 1)]
    {:buff8 buff8
     :buff32 buff32
     :length length
     :bin-size bin-size
     :bin-dict-offset bin-dict-offset
     :bin-count-offset bin-count-offset
     :bin-offsets (inc bin-count-offset)
     :bin-count bin-count
     :first-bin first-bin
     :header-dict (huffman/ht-tree buff32 buff8 8)
     :inner-dict (huffman/huff-tree buff32 buff8 bin-dict-offset)}))


;
; sketching some routines for computing the union
;

(defn diff-sorted [a b]
  (let [acc (transient [])]
    (loop [a a b b]
      (if-let [n (first a)]
        (if-let [m (first b)]
          (if (= n m)
            (recur (rest a) (rest b))
            (if (< (compare n m) 0)
              (recur (rest a) b)
              (do
                (conj! acc m)
                (recur a (rest b)))))
          (persistent! acc))
        (into (persistent! acc) b)))))

;(diff-sorted [] [])
;(diff-sorted ["foo"] ["foo"])
;(diff-sorted ["foo"] ["boo" "foo"])
;(diff-sorted ["boo" "foo"] ["boo" "foo"])
;(diff-sorted ["boo" "foo"] ["boo" "foo" "goo"])
;(diff-sorted ["boo" "foo"] ["boo" "foo" "goo" "hoo"])
;(diff-sorted ["boo" "foo"] ["boo" "bff" "foo" "goo" "hoo"])
;(diff-sorted [] ["boo" "bff" "foo" "goo" "hoo"])
;(diff-sorted ["boo" "foo"] [])

(defn merge-sorted [a b]
  (if-let [n (first a)]
    (if-let [m (first b)]
      (if (= n m)
        (lazy-seq (cons n (merge-sorted (rest a) (rest b))))
        (if (< (compare n m) 0)
          (lazy-seq (cons n (merge-sorted (rest a) b)))
          (lazy-seq (cons m (merge-sorted a (rest b))))))
      a)
    b))

;(merge-sorted [] [])
;(merge-sorted ["foo"] ["foo"])
;(merge-sorted ["foo"] ["boo" "foo"])
;(merge-sorted ["boo" "foo"] ["boo" "foo"])
;(merge-sorted ["boo" "foo"] ["boo" "foo" "goo"])
;(merge-sorted ["boo" "foo"] ["boo" "foo" "goo" "hoo"])
;(merge-sorted ["boo" "foo"] ["boo" "bff" "foo" "goo" "hoo"])
;(merge-sorted [] ["boo" "bff" "foo" "goo" "hoo"])
;(merge-sorted ["boo" "foo"] [])

(defn merge-sorted-v [a b]
  (let [acc (transient [])]
    (loop [a a b b]
      (if-let [n (first a)]
        (if-let [m (first b)]
          (if (= n m)
            (do
              (conj! acc n)
              (recur (rest a) (rest b)))
            (if (< (compare n m) 0)
              (do
                (conj! acc n)
                (recur (rest a) b))
              (do
                (conj! acc m)
                (recur a (rest b)))))
         (into (persistent! acc) a))
        (into (persistent! acc) b)))))

;(merge-sorted-v [] [])
;(merge-sorted-v ["foo"] ["foo"])
;(merge-sorted-v ["foo"] ["boo" "foo"])
;(merge-sorted-v ["boo" "foo"] ["boo" "foo"])
;(merge-sorted-v ["boo" "foo"] ["boo" "foo" "goo"])
;(merge-sorted-v ["boo" "foo"] ["boo" "foo" "goo" "hoo"])
;(merge-sorted-v ["boo" "foo"] ["boo" "bff" "foo" "goo" "hoo"])
;(merge-sorted-v [] ["boo" "bff" "foo" "goo" "hoo"])
;(merge-sorted-v ["boo" "foo"] [])
;(merge-sorted-v ["boo" "foo" "zoo"] ["boo" "foo" "goo" "hoo"])
(defn lookup-htfc [htfc i])


(defn find-htfc [htfc s])

(defn merge-htfc [a b])

;
;
;

(comment

  (require '[clojure.data.json :as json])
  (use 'criterium.core)

  ;(def buff
  ;    (compress-pfc strings 32))


  (defn spy [msg x]
    (println msg x)
    x)

  ;let [file "toil.json"]
  (let [file "singlecell.json"]
    (def strings
      (json/read-str (slurp file))))

  (time
    (def buff2
      (serialize-htfc (compress-htfc strings 256))))

  ; round trip
  (let [strings strings
        in (time (serialize-htfc (compress-htfc strings 256)))
        out (time (into [] (uncompress-dict (htfc-offsets (doto (ByteBuffer/wrap in)
                                                           (.order java.nio.ByteOrder/LITTLE_ENDIAN))))))]
    (= (sort strings) out))

  (time
    (do
      (into [] (uncompress-dict (htfc-offsets in)))
      nil))

  (use 'clojure.java.io)
  (with-open [file (java.io.FileOutputStream. "toil.bin")]
    (.write file buff2))

  ; Is there some nio mapped buffer solution?
  (defn file-to-byte-array [filename]
    (let [^java.io.File file (clojure.java.io/as-file filename)
          result (byte-array (.length file))]
      (with-open [in (java.io.DataInputStream. (clojure.java.io/input-stream file))]
        (.readFully in result))
      (doto (java.nio.ByteBuffer/wrap result)
        (.order java.nio.ByteOrder/LITTLE_ENDIAN))))

  (def in (file-to-byte-array "toil.bin"))

; goal: back-off perf fixes if they aren't helping.
; goal: unify ht & huff methods
; goal: make decode methods more uniform wrt 32 vs. 8 bit access
; goal: make better method & property names
; add end-to-end test

  ; shannon
  (let [in (into-array Byte/TYPE (mapcat #(conj (seq (.getBytes %)) (char 0))
                                         strings))
        freqs (huffman/byte-freqs [in])
        len (count in)
        p (map (fn [[sym n]] (/ n len)) freqs)
        ln2 (Math/log 2)]
    (- (reduce + 0 (map #(* (/ (Math/log %) ln2) %) p))))

  (/ 4.6 1.3) ; 3.5 bytes per string, when headers are compressed
  (float (/ (count buff2) (count strings))) ; 5.7 bytes for toil???
  ; 4.6 *bits* shannon entropy???
  (float (/ (count buff2) (apply + (map count strings)))) ; 31% for toil
  ; 18% for singlecell

  ;(spit "singlecell.tsv" (clojure.string/join "\n" strings))
  ; around 128k for toil, 5.8 bytes per string
  ; not compressing the headers. Also, mis-counting sizes
  ; 5M for singlecell, 3.9 bytes per string
  (float (/ 207422 22000))    ; 9.42 bytes per string, toil
  (let [[[sizes symbols] blocks block-sizes headers] buff2
        total

        (+ (count sizes)
           (count symbols)
           (apply + (map count blocks))
           (* 4 (count block-sizes))
           (apply + (map count headers)))]
    [total (float (/ total (count strings)))])

  (time (compress-htfc strings 32))

  ; 1.3M 8,397,304 29%, at bin-size 100. At 32 it's 8.9M
  ;      orig is 28,988,248
  ;      gzip is, what, 7.7M, for 26.8%
  ; There was some off-the-shelf compressor I tried, that was better than gzip.
  ; What was it?
  ; In theory, we could push this closer to 10%?

  ; Should we combine the prefix len & the suffix len in one byte? We're probably
  ; wasting a lot of space there.


  ; toil
  ; 22k strings, 463964
  ; compressed   207422 ugh. This is pathetic. Why is it so bad? There's
  ;                          more entropy in tcga/gtex ids?

  (float (/ 207422 22000))    ; 9.42 bytes per string, toil
  (float (/ 8900000 1300000))) ; 6.8 bytes per string, singlecell
