(ns cavm.sqleval-test
  (:require [cavm.sqleval :as sqleval])
  (:require [clojure.test :as ct])
  (:require [clojure.test.check :as tc])
  (:require [clojure.test.check.generators :as gen])
  (:require [clojure.test.check.clojure-test :as tcct :refer [defspec]])
  (:require [clojure.test.check.properties :as prop]))

(def ^:dynamic *test-runs* 4000)

(ct/deftest in-test
  (ct/testing "basic :in"
    (let [data {"a" [:a :b :c :d :e :f :g :h]}
          rows (sorted-set 1 3 4 5 6 7)]
      (ct/is (= (sorted-set 4 5 7)
                (sqleval/op-in rows
                                  (fn [q-rows field]
                                    (zipmap q-rows
                                            (map (data field) q-rows)))
                                  "a"
                                  [:e :f :h]))))))

(ct/deftest eval-test
  (ct/testing "eval")
  (let [data {"a" [:a :b :c :d :e :f :g :h]
              "b" (into [] (range 20 30))}
        rows (apply sorted-set (range 8))]
    (ct/is (= (sorted-set 4 5 7)
              (sqleval/evaluate rows
                                (fn [q-rows field]
                                  (zipmap q-rows
                                          (map (data field) q-rows)))
                                [:in "a" [:e :f :h]])))
    (ct/is (= (sorted-set 4 5 7)
              (sqleval/evaluate rows
                                (fn [q-rows field]
                                  (zipmap q-rows
                                          (map (data field) q-rows)))
                                [:and [:in "a" [:e :f :h]]])))
    (ct/is (= (sorted-set 4 5)
              (sqleval/evaluate rows
                                (fn [q-rows field]
                                  (zipmap q-rows
                                          (map (data field) q-rows)))
                                [:and [:in "a" [:e :f :h]] [:in "b" [24 25]]])))
    (ct/is (= (sorted-set 0 1 4 5 7)
              (sqleval/evaluate rows
                                (fn [q-rows field]
                                  (zipmap q-rows
                                          (map (data field) q-rows)))
                                [:or [:in "a" [:e :f :h]] [:in "b" [20 21]]])))))
