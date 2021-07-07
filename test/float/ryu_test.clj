(ns float.ryu-test
  (:require [clojure.test :refer [deftest testing is]])
  (:require [clojure.java.shell :refer [sh]])
  (:require [clojure.test.check :as tc])
  (:require [clojure.test.check.generators :as gen])
  (:require [clojure.test.check.clojure-test :as tcct :refer [defspec]])
  (:require [clojure.test.check.properties :as prop])
  (:import java.lang.Runtime)
  (:import info.adams.ryu.RyuFloat)
  (:import info.adams.ryu.RoundingMode))

(def ^:dynamic *test-runs* 400)

(defn libc-float-fmt [fmt value]
  (-> (sh "printf" fmt value)
      :out
      (clojure.string/replace #"e\+0([0-9])" "e$1")
      (clojure.string/replace #"e\+" "e")
      (clojure.string/replace #"inf" "Infinity")))

(defn libc-float [digits value]
  (libc-float-fmt (str "%." digits \g) value))

(defn ryu-float [digits value]
  (RyuFloat/floatToString (Float. value) RoundingMode/ROUND_EVEN digits))

(deftest basic-test
  (testing "whole ones"
    (let [int-ones (map #(-> (apply str \1 (repeat % \0))) (range 0 20))
          digits (range 1 20)
          combos (for [v int-ones
                       d digits] [v d])]
      (doseq [[v d] combos]
        (let [libc (libc-float d v)
              ryu (ryu-float d v)]
         (is (= (Float. libc) (Float. ryu)) {:value v :digits d})
         (is (<= (count ryu) (count libc)) {:value v :digits d})))))
  (testing "fractional ones"
    (let [int-ones (map #(-> (str "0." (apply str (repeat % \0)) \1)) (range 0 20))
          digits (range 1 20)
          combos (for [v int-ones
                       d digits] [v d])]
      (doseq [[v d] combos]
        (let [libc (libc-float d v)
              ryu (ryu-float d v)]
         (is (= (Float. libc) (Float. ryu)) {:value v :digits d})
         (is (<= (count ryu) (count libc)) {:value v :digits d})))))
  (testing "nines"
    (let [int-ones (map #(-> (str "9999" (apply str (repeat % \0)))) (range 0 16))
          digits (range 1 20)
          combos (for [v int-ones
                       d digits] [v d])]
      (doseq [[v d] combos]
        (let [libc (libc-float d v)
              ryu (ryu-float d v)]
         (is (= (Float. libc) (Float. ryu)) {:value v :digits d})
         (is (<= (count ryu) (count libc)) {:value v :digits d})))))
  (testing "fractional nines"
    (let [int-ones (map #(-> (str "0." (apply str (repeat % \0)) "9999")) (range 0 16))
          digits (range 1 20)
          combos (for [v int-ones
                       d digits] [v d])]
      (doseq [[v d] combos]
        (let [libc (libc-float d v)
              ryu (ryu-float d v)]
          (is (= (Float. libc) (Float. ryu)) {:value v :digits d})
          (is (<= (count ryu) (count libc)) {:value v :digits d})))))
  (testing "zero"
    (let [digits (range 1 4)
          combos (for [v ["0.0" "-0.0"]
                       d digits] [v d])]
      (doseq [[v d] combos]
        (let [libc (libc-float d v)
              ryu (ryu-float d v)]
         (is (= (Float. libc) (Float. ryu)) {:value v :digits d})
         (is (<= (count ryu) (count libc)) {:value v :digits d}))))))


(def sign
  (gen/one-of [(gen/return "") (gen/return "-")]))

(defn digits [n]
  (gen/vector (gen/one-of (map #(gen/return (str %)) (range 0 10))) n))

(def non-zero-digit
  (gen/one-of (map #(gen/return (str %)) (range 1 10))))

(def exp
  (gen/choose -39 38))

(def gen-float
  (gen/fmap (fn [[sign first-digit rest-digits exp]]
              (str sign first-digit \. (apply str rest-digits) \e exp))
            (gen/tuple sign non-zero-digit (digits 9) exp)))

(gen/sample gen-float)

(defspec float-equiv-test
  *test-runs*
  (prop/for-all
    [v-str gen-float]
    (let [digits (range 1 20)]
      (every? (fn [d]
                (let [vf (Float. v-str)     ; Convert to float to get exact bit value.
                      v (format "%.12e" vf) ; Back to high-precision string for handoff.
                      libc (libc-float d v)
                      ryu (ryu-float d v)]
;                  (println libc ryu v d) ; dumps the libc vs. ryu output
                  (and (= (Float. ^String libc) (Float. ^String ryu))
                       (<= (count ryu) (count libc)))))
              digits))))

(deftest bounds
  (testing "bounds"
    (let [values [(str Float/MAX_VALUE) (str \- Float/MAX_VALUE) (str Float/MIN_VALUE) (str \- Float/MIN_VALUE)]
          digits (range 1 20)
          combos (for [v values
                       d digits] [v d])]
      (doseq [[v-str d] combos]
        (let [vf (Float. ^String v-str)
              v (format "%.12e" vf)
              libc (libc-float d v)
              ryu (ryu-float d v)]
;          (println libc ryu v d) ; dumps the libc vs. ryu output
          (is (= (Float. ^String libc) (Float. ^String ryu)) {:value v-str :digits d})
          (is (<= (count ryu) (count libc)) {:value v-str :digits d}))))))

(deftest regressions
  (testing "regressions"
    (let [values ["9.032756200e-23" "4.200000251e6" "1.035024980e-7" "7.085204700e-21"]
          digits (range 1 20)
          combos (for [v values
                       d digits] [v d])]
      (doseq [[v-str d] combos]
        (let [vf (Float. ^String v-str)
              v (format "%.12e" vf)
              libc (libc-float d v)
              ryu (ryu-float d v)]
          (is (= (Float. ^String libc) (Float. ^String ryu)) {:value v-str :digits d})
          (is (<= (count ryu) (count libc)) {:value v-str :digits d}))))))
