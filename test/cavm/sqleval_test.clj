(ns cavm.sqleval-test
  (:require [cavm.sqleval :as sqleval])
  (:require [clojure.test :as ct])
  (:require [clojure.test.check :as tc])
  (:require [clojure.test.check.generators :as gen])
  (:require [clojure.test.check.clojure-test :as tcct :refer [defspec]])
  (:require [clojure.data.int-map :as i])
  (:require [clojure.test.check.properties :as prop]))

(def ^:dynamic *test-runs* 4000)

(ct/deftest in-test
  (ct/testing "basic :in"
    (let [data {"a" [:a :b :c :d :e :f :g :h]}
          rows (sorted-set 1 3 4 5 6 7)
          store {:indexed? #{}
                 :fetch (fn [rows-in field]
                          (let [r (or rows-in rows)]
                            (zipmap r
                                    (map (data field) r))))}]
      (ct/is (= (sorted-set 4 5 7)
                (sqleval/op-in-cmp sqleval/in-set
                                   rows
                                   rows
                                   store
                                   "a"
                                   [:e :f :h]))))))

(ct/deftest eval-test
  (ct/testing "eval"
    (let [data {"a" [:a :b :c :d :e :f :g :h]
                "b" (into [] (range 20 30))}
          rows (into (i/int-set) (range 8))
          store {:indexed? #{}
                 :fetch (fn [rows-in field]
                          (let [r (or rows-in rows)]
                            (zipmap r
                                    (map (data field) r))))}]
      (ct/is (= {"b" [24 25 27]}
                (sqleval/evaluate rows
                                  store
                                  {:select ["b"] :where [:in "a" [:e :f :h]]})))
      (ct/is (= {"b" [24 25 27]}
                (sqleval/evaluate rows
                                  store
                                  {:select ["b"] :where [:and [:in "a" [:e :f :h]]]})))
      (ct/is (= {"a" [:e :f] "b" [24 25]}
                (sqleval/evaluate rows
                                  store
                                  {:select ["a" "b"] :where [:and [:in "a" [:e :f :h]] [:in "b" [24 25]]]})))
      (ct/is (= {"a" [:a :b :e :f :h] "b" [20 21 24 25 27]}
                (sqleval/evaluate rows
                                  store
                                  {:select ["a" "b"] :where [:or [:in "a" [:e :f :h]] [:in "b" [20 21]]]})))
      (ct/is (= {"a" [] "b" []}
                (sqleval/evaluate rows
                                  store
                                  {:select ["a" "b"] :where [:in "a" [:z :y]]}))))))


(ct/deftest arr-test
  (ct/testing "array operations"
    (let [data {"gene" [[:a :b :c] [:a] [:b :c] [:a :c] [:b]]
                "row" (into [] (range 5))}
          rows (apply sorted-set (range 5))
          store {:indexed? #{}
                 :fetch (fn [rows-in field]
                          (let [r (or rows-in rows)]
                            (zipmap r
                                    (map (data field) r))))}]
      (ct/is (= {"row" [0 1 3] "gene" [[:a :b :c] [:a] [:a :c]]}
                (sqleval/evaluate rows
                                  store
                                  {:select ["row" "gene"] :where [:in :any "gene" [:a]]})))
      (ct/is (= {"row" [0 2 3 4]}
                (sqleval/evaluate rows
                                  store
                                  {:select ["row"] :where [:in :any "gene" [:c :b]]})))
      (ct/is (= {"row" [2 4]}
                (sqleval/evaluate rows
                                  store
                                  {:select ["row"] :where [:in :all "gene" [:c :b]]}))))))

(ct/deftest limit-test
  (ct/testing "limit clause"
    (let [data {"gene" [[:a :b :c] [:a] [:b :c] [:a :c] [:b]]
                "row" (into [] (range 5))}
          rows (apply sorted-set (range 5))
          store {:indexed? #{}
                 :fetch (fn [rows-in field]
                          (let [r (or rows-in rows)]
                            (zipmap r
                                    (map (data field) r))))}]
      (ct/is (= {"row" [0 1 2] "gene" [[:a :b :c] [:a] [:b :c]]}
                (sqleval/evaluate rows
                                  store
                                  {:select ["row" "gene"] :where [:in :any "gene" [:a :b]] :limit 3})))
      (ct/is (= {"row" [1 2 3] "gene" [[:a] [:b :c] [:a :c]]}
                (sqleval/evaluate rows
                                  store
                                  {:select ["row" "gene"] :where [:in :any "gene" [:a :b]] :limit 3 :offset 1}))))))
