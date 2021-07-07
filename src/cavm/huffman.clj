(ns
  ^{:author "Brian Craft"
    :doc "huffman coding"}
  cavm.huffman
  (:import [java.nio ByteBufferAsIntBufferL ByteBuffer])
  (:import [java.io ByteArrayOutputStream])
  (:import [java.util.concurrent ArrayBlockingQueue]))

(set! *unchecked-math* true)
(defn combine-freqs
  ([] (long-array 256))
  ([^longs arr0 ^longs arr1]
   (loop [i 0]
     (when (< i 256)
       (aset arr0 i ^long (+ (aget arr0 i) (aget arr1 i)))
       (recur (inc i))))
   arr0))

(defn update-freqs [^longs freqs ^bytes item]
  (let [len (alength item)]
    (loop [i 0]
      (if (< i len)
        (let [c (bit-and 0xff (aget item i))]
          (aset freqs c ^long (inc (aget freqs c)))
          (recur (inc i)))
        freqs))))

; Using primitive arrays is two orders of magnitude faster than a hash-map of counts.
(defn byte-freqs [rdr]
  (into {} (mapcat (fn [i v] (if (> v 0) [[i v]] []))
                   (range 256)
                   (clojure.core.reducers/fold combine-freqs update-freqs rdr))))
(set! *unchecked-math* false)

; These are byte arrays by the time they get here, due to front-coding.
;(byte-freqs (map #(.getBytes ^String %) ["foo" "bar"]))
;(byte-freqs (mapv #(.getBytes ^String %) ["foo" "bar" "baz" "horse" "fly"]))
;(byte-freqs-p (mapv #(.getBytes ^String %) ["foo" "bar" "baz" "horse" "fly"]))

(defn- take-lowest [^ArrayBlockingQueue q0
                    ^ArrayBlockingQueue q1]
    (let [v0 (.peek q0) v1 (.peek q1)]
        (if (or (not v1) (and v0 (>= (:priority v1) (:priority v0))))
            (.take q0)
            (.take q1))))

; compute huffman tree from char freqs
(defn- build-tree [freqs]
    (let [leaves (->> freqs
                      (sort-by second)
                      (map #(zipmap [:symbol :priority] %))
                      (ArrayBlockingQueue. (count freqs) false))
          nodes (ArrayBlockingQueue. (count freqs))]
        (while (> (+ (.size leaves) (.size nodes)) 1)
            (let [a (take-lowest leaves nodes) b (take-lowest leaves nodes)
                  new-node {:priority (+ (:priority a) (:priority b)) :left a :right b}]
                (.add nodes new-node)))
        (.take nodes)))

;
; hu-tucker
;

(defn- compatible-pairs [nodes]
  (let [len (count nodes)]
    (for [i (range len)
          :when (nodes i)
          j (range (inc i) len)
          :when (nodes j)
          :while (or (= j (inc i)) (not (:symbol (nodes (dec j)))))]
      [i j])))

(defn- best-merge [nodes]
  (->> nodes
       compatible-pairs
       (map (fn [pair] {:priority (+ (:priority (nodes (pair 0))) (:priority (nodes (pair 1)))) :pair pair}))
       (apply min-key :priority)))

; compute hu-tucker tree from char freqs, O(n^2)
; There are O(n log n), and O(n) algorithms, but they're quite complex.
(defn- build-ht-tree [freqs]
  (let [leaves (->> freqs
                    (sort-by first)
                    (mapv #(zipmap [:symbol :priority] %)))
        len (count leaves)]
    (loop [i (dec len)
           nodes leaves]
      (if (> i 0)
        (let [to-merge (best-merge nodes)
              pair (:pair to-merge)
              next-nodes (assoc nodes
                                (pair 0) (assoc to-merge :left (nodes (pair 0)) :right (nodes (pair 1)))
                                (pair 1) nil)]
          (recur (dec i) next-nodes))
        (some identity nodes)))))

; would this work for huffman, too? It might simplify things to only have a single
; internal representation of a code. Or would it mess up the compact encoding of
; the dictionary? Speaking of which, is there a compact encoding of the hu-tucker
; dictionary?
(defn- make-ht-codes [tree]
  (loop [acc [] q [[0 0 tree]]]
    (if (seq q)
      (let [[[len code node] & more] q]
       (if-let [symbol (:symbol node)]
         (recur (conj acc {:symbol symbol :length len :code code}) more)
         (recur acc (conj more
                          [(inc len) (bit-shift-left code 1) (:left node)]
                          [(inc len) (bit-or (bit-shift-left code 1) 1) (:right node)]))))
      acc)))

(defn- ht-dictionary [codes]
  (->> codes
   (map #(-> [(:symbol %) %]))
   (into {})))

; little endian
(defn write-int [i ^java.io.ByteArrayOutputStream out]
  (.write out (bit-and 0xff i))
  (.write out (bit-and 0xff (bit-shift-right i 8)))
  (.write out (bit-and 0xff (bit-shift-right i 16)))
  (.write out (bit-and 0xff (bit-shift-right i 24))))

; if we word-align the ints, I suspect we'll be in a better place.
; Should we sort by length, as with huffman?
(defn- ht-coded-dictionary [codes ^java.io.ByteArrayOutputStream out]
  (write-int (count codes) out)
  (doseq [{:keys [code length]} codes]
         (write-int code out)
         (write-int length out))
  (doseq [{:keys [symbol]} codes]
         (.write out ^byte symbol))
  (dotimes [i (rem (- 4 (rem (count codes) 4)) 4)] ; extend to word boundary
    (.write out 0)))

; group huffman tree leaves by length from root
(defn- find-depth
    ([tree] (find-depth [tree] 0 []))
    ([nodes depth acc]
     (if (seq nodes)
         (let [{terminals true inner false} (group-by #(contains? % :symbol) nodes)
               children (mapcat #(-> [(:left %) (:right %)]) inner)]
             (recur children
                    (inc depth)
                    (if terminals (conj acc {:length depth :symbols terminals}) acc)))
         acc)))

; Compute canonical codes for tree leaves. This isn't a simple 'map' call
; because we need the 'code' and 'depth' context for each successive leaf depth.
(defn- make-codes
    ([groups] (make-codes groups 0 (:length (first groups)) (transient [])))
    ([groups code depth0 acc]
     (if-let [group (first groups)]
         (let [{depth1 :length nodes :symbols} group
               nodes (sort-by :symbol nodes)
               start (bit-shift-left code (- depth1 depth0))]
             (recur
                 (rest groups)
                 (+ start (count nodes))
                 depth1
                 (conj! acc
                        (assoc group
                               :length depth1
                               :symbols (map #(assoc %1 :code %2)
                                             nodes
                                             (drop start (range)))))))
         (persistent! acc))))

; Build a hash for encoding the text
(defn dictionary [groups]
    (into {}
          (mapcat (fn [group]
                      (let [{:keys [length symbols]} group]
                          (map #(-> [(:symbol %) (assoc % :length length)]) symbols)))
                  groups)))

(comment
 (defn spy [msg x]
   (println msg x)
   x))

; Return the compactly-encoded canonical dictionary
; len count, lens, bytes
(defn coded-dictionary [groups ^java.io.ByteArrayOutputStream out]
  (let [len-counts (into {} (map #(-> [(% :length) (count (% :symbols))]) groups))
        m (apply max (keys len-counts))]
    (write-int m out)
    (doseq [c (map #(get len-counts % 0) (range 1 (inc m)))]
      (write-int c out))
    (let [symbols (mapcat #(map :symbol (:symbols %)) groups)]
      (doseq [^byte b symbols]
        (.write out b))
      ; XXX this is probably the wrong place for this, since the input
      ; buffer is passed in.
      (dotimes [i (rem (- 4 (rem (count symbols) 4)) 4)] ; extend to word boundary
        (.write out 0)))))

(set! *unchecked-math* true)

(defn map-to-arr [m]
  (let [a (object-array 256)]
    (doseq [[i v] m]
      (aset a i v))
    a))

; Assume codes are at most 57 bits so we can write a full code to a 64 bit integer type,
; and have room for any partial byte (up to 7 bits) remaining from the previous
; code. Not sure how reasonable this assumption is. See the comment section at
; bottom of the file.
; At each iteration
;    shift left & merge into 'out' long
;    if bits in 'out' is over 8, send any full bytes (1-7)
;    shift output to remove sent bytes
(defn encode-bytes [^java.io.ByteArrayOutputStream output dict vec-of-arr]
  (let [^bytes arr (vec-of-arr 0) ; XXX before commit, change this to an arr, not vec of array
        len (alength arr)
        ^objects flat-dict (map-to-arr dict)]
    (loop [i 0   ; input index
           out 0 ; output register, treated as 64 bit
           m 0]  ; number of bits currently in 'out'
      (if (< i len)
        (let [{:keys [code length]} (aget flat-dict (bit-and 0xff (long (aget arr i))))
              out (bit-or out (bit-shift-left code (- 64 m length)))
              m (long (+ m length))]
          (when (>= m 8)
            (.write output (bit-and 0xff (bit-shift-right out 56)))
            (when (>= m 16)
              (.write output (bit-and 0xff (bit-shift-right out 48)))
              (when (>= m 24)
                (.write output (bit-and 0xff (bit-shift-right out 40)))
                (when (>= m 32)
                  (.write output (bit-and 0xff (bit-shift-right out 32)))
                  (when (>= m 40)
                    (.write output (bit-and 0xff (bit-shift-right out 24)))
                    (when (>= m 48)
                      (.write output (bit-and 0xff (bit-shift-right out 16)))
                      (when (>= m 56)
                        (.write output (bit-and 0xff (bit-shift-right out 8)))
                        (when (= m 64)
                          (.write output (bit-and 0xff out))))))))))
          (if (>= m 8)
            (let [shift (* 8 (quot m 8))]
              (recur (inc i) (bit-shift-left out shift) (long (- m shift))))
            (recur (inc i) out m)))
        (when (not= m 0) ; write last partial byte
          (.write output (bit-and 0xff (bit-shift-right out 56))))))))

(set! *unchecked-math* false)

(defprotocol Encoder
  (write-dictionary [this out])
  (write [this out text]))

(defrecord huffman [groups dictionary]
  Encoder
  (write-dictionary [this out] (coded-dictionary groups out))
  (write [this out text] (encode-bytes out dictionary text)))

(defrecord hu-tucker [codes dictionary]
  Encoder
  (write-dictionary [this out] (ht-coded-dictionary codes out))
  (write [this out text] (encode-bytes out dictionary [text])))

(defn make-huffman [items]
  (let [groups (->> items byte-freqs build-tree find-depth make-codes)]
    (->huffman groups (dictionary groups))))

(defn make-hu-tucker [items]
  (let [codes (->> items byte-freqs build-ht-tree make-ht-codes)]
    (->hu-tucker codes (ht-dictionary codes))))

; Really want a decode for the output buff.

(defn- ^String toBinaryStr [n]
    (.replace (format "%8s" (Integer/toBinaryString n)) " " "0"))

(defn display-dictionary [dict]
    (clojure.pprint/print-table
      (map #(-> {:symbol (char (:symbol %))
                 :code (.substring (toBinaryStr (:code %)) (- 8 (:length %)) 8)})
           (sort-by :symbol (vals dict)))))

(defn decode-to [tree ^ByteBuffer buff8 initialP ^ByteArrayOutputStream out stop]
  (loop [^long inP initialP n tree]
    (if (= (:symbol n) stop) ; This is kinda nasty. Weird recursion rule when we match. See below.
      inP
      (let [b (.get buff8 inP)]
        (recur (inc inP)
               (loop [j 0x80 n n]
                 (if (> j 0)
                   (let [n (if (= (bit-and b j) 0) (:right n) (:left n))]
                     (if-let [^byte s (:symbol n)]
                       (do
                         (.write out s)
                         (if (= s stop) ; On stop code,
                           (recur 0 n)  ; force exit of inner loop, and return the stop symbol
                           (recur (bit-shift-right j 1) tree)))
                       (recur (bit-shift-right j 1) n)))
                   n)))))))

(defn decode-range [tree ^ByteBuffer buff8 initialP ^ByteArrayOutputStream out maxP]
  (loop [^long inP initialP n tree]
    (when (< inP maxP)
      (let [b (.get buff8 inP)]
        (recur (inc inP)
               (loop [j 0x80 n n]
                 (if (> j 0)
                   (let [n (if (= (bit-and b j) 0) (:right n) (:left n))]
                     (if-let [^byte s (:symbol n)]
                       (do
                         (.write out s)
                         (recur (bit-shift-right j 1) tree))
                       (recur (bit-shift-right j 1) n)))
                   n)))))))

(defn insert-code [tree code len sym]
  (update-in tree
            (for [i (range (dec len) -1 -1)]
              (if (= 0 (bit-and (bit-shift-left 1 i) code))
                :right
                :left))
            assoc :symbol sym))

; build decoding tree for huffman encoding
(defn huff-tree [^ByteBufferAsIntBufferL buff32 ^ByteBuffer buff8 offset32]
  (let [len (.get buff32 ^long offset32)]
    (loop [i 1 code 0 symbol8 (* 4 (+ offset32 1 len)) tree {}]
      (if (<= i len)
        (let [N (.get buff32 ^long (+ offset32 i))]
          (recur (inc i)
                 (bit-shift-left (+ code N) 1)
                 (+ symbol8 N)
                 (loop [j 0 code code tree tree]
                   (if (< j N)
                     (recur (inc j)
                            (inc code)
                            (insert-code tree
                                         code
                                         i
                                         (.get buff8 ^long (+ symbol8 j))))
                     tree))))
        tree))))

; build decoding tree for hu-tucker encoding
(defn ht-tree [^ByteBufferAsIntBufferL buff32 ^ByteBuffer buff8 offset8]
  (let [offset32 (/ offset8 4)
        len (.get buff32 ^long offset32)
        symbols (* 4 (+ 1 (* 2 len)))]
    (loop [j 0
           tree {}]
      (if (< j len)
        (recur (inc j)
               (insert-code tree
                            (.get buff32 ^long (+ offset32 1 (* 2 j)))
                            (.get buff32 ^long (+ offset32 2 (* 2 j)))
                            (.get buff8 ^long (+ offset8 symbols j))))
        tree))))

(defn ht-dict-len [^ByteBufferAsIntBufferL buff32 ^long offset32]
  (let [len (.get buff32 offset32)]
    (+ 1 (* 2 len) (long (Math/ceil (/ len 4))))))

(defn huff-dict-len [^ByteBufferAsIntBufferL buff32 ^long offset32]
  (let [bits (.get buff32 offset32)
        sum (loop [i 0 sum 0]
              (if (< i bits)
                (recur (inc i) (+ sum (.get buff32 (+ offset32 1 i))))
                sum))]
    (+ 1 bits (long (Math/ceil (/ sum 4))))))

(comment

  ; Checking code length bounds, to make sure we can use a 64 bit data type
  ; for encoding w/o overflow.
  ; Worst case for huffman is fibonacci distribution.

  (do
    ; generate seq of fibonacci
    (defn fibs
      ([] (fibs 1 0))
      ([p q]
       (lazy-seq
         (cons (+ p q) (fibs q (+ p q))))))

    ; compute running sum of a seq
    (defn running-sum
      ([s] (running-sum s 0))
      ([s c]
       (if (seq s)
         (let [c (+ (first s) c)]
           (lazy-seq (cons c (running-sum (rest s) c))))
         '())))

    ; Randomly select byte values according to a fibonacci distribution. Works by
    ; computing the running sum of weights, equivalent to stacking sized elements along
    ; a number line; then picking a random number & finding which element it falls on.
    ; Uses enough symbols to fill Integer/MAX_VALUE, which is 44 possible byte values.
    (defn fibselection []
      (let [weight-sums (vec (take-while #(< % Integer/MAX_VALUE) (running-sum (fibs))))
            idxs (range (count weight-sums))]
        (map (fn [i] (or (first (filter #(<= i (weight-sums %)) idxs)) (count weight-sums)))
             (repeatedly #(rand-int Integer/MAX_VALUE))))))

  ; bump the symbol count up to 88, by randomly selecting a range
  (defn moresyms [] (map #(if (= 0 (rand-int 2)) % (+ 44 %)) (fibselection)))
 ; (take 10 (moresyms))

  ; results look far from 57, which is the max we support in the encode-bytes routine, above.
  (map :length (:groups (make-huffman [(byte-array (take 1000000 (fibselection)))]))) ;1M: 22 bits
  (map :length (:groups (make-huffman [(byte-array (take 10000000 (fibselection)))])));10M: 23 bits
  (map :length (:groups (make-huffman [(byte-array (take 1000000 (moresyms)))]))) ;1M: 19 bits
  (map :length (:groups (make-huffman [(byte-array (take 5000000 (moresyms)))]))) ;1M: 21 bits
  (map :length (:codes (make-hu-tucker [(byte-array (take 1000000 (fibselection)))])));1M: 21 bits

  ;
  ;
  ;

  (display-dictionary (dictionary (:groups (make-huffman [(.getBytes "this is an example of a huffman tree")]))))
  (require '[clj-java-decompiler.core :refer  [decompile disassemble]])
  (require 'no.disassemble)
  (def disa (no.disassemble/disassemble encode-bytes2))
  (def disb (no.disassemble/disassemble encode-bytes2))
  (println disa)
  (= disa disb)
  (let [s (map #(.getBytes ^String %) ["foo" "bar" "baz" "one" "two" "three"])]
       (display-dictionary (dictionary (build s))))
  (let [s (map #(.getBytes ^String %) ["foo" "bar" "baz" "one" "two" "three" "zzzzzzz"])
        huffman (build s)
        dict (dictionary huffman)
        encoded (encode-bytes dict (apply concat s))]

    (display-dictionary dict)
    [(count encoded) (into [] encoded)]))
