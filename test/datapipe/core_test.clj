(ns datapipe.core-test
  (:require [clojure.test :refer :all]
            [datapipe.core :refer :all]))

(deftest test-sample
  (testing "The sample method"
    (let [coll (sample 0.1 (range 10000))]
      (is (< (count coll) 2000)))))

(deftest test-equals
  (testing "The equals method"
    (let [rows [{:x 1} {:x 2} {:x 3}]]
      (is (= (equals :x 2 rows) [{:x 2}])))))

(deftest test-lookup
  (testing "The lookup method"
    (let [rows [{:x 1 :y 10} {:x 2 :y 20} {:x 3 :y 30}]]
      (is (= (lookup :x rows) [{:x 1} {:x 2} {:x 3}]))
      (is (= (lookup :y rows) [{:y 10} {:y 20} {:y 30}])))))

(deftest test-freq-map
  (testing "Frequency map"
    (let [rows [{:key "a"} {:key "b"} {:key "c"} {:key "a"} {:key "a"} {:key "b"}]]
      (is (= (sort-by :key (freqs :key rows)) [{:key "a" :amount 3} {:key "b" :amount 2} {:key "c" :amount 1}])))))

(deftest test-freqs
  (testing "Frequencies"
    (let [positions [{:x 1 :y 1} {:x 1 :y 2} {:x 2 :y 1}]]
      (is (= (sort-by :x (freqs :x positions)) [{:x 1 :amount 2} {:x 2 :amount 1}]))
      (is (= (sort-by #(vec (map % [:x :y])) (freqs :x :y positions)) [{:x 1 :y 1 :amount 1} {:x 1 :y 2 :amount 1} {:x 2 :y 1 :amount 1}])))))
