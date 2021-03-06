(ns datapipe.ops-test
  (:require [clojure.test :refer :all]
            [datapipe.ops :refer :all]))

(deftest test-average
  (testing "The average method"
    (let [coll [{:name "Alice" :age 40}
                {:name "Bob" :age "10"}
                {:name "Bob" :age 30}]]
      (is (= (average :age :name coll)
             [{:name "Alice" :age 40.0} {:name "Bob" :age 20.0}]))))
  (testing "Grouped average method"
    (let [coll [{:year 2015 :month 1 :value 10}
                {:year 2015 :month 1 :value 30}
                {:year 2014 :month 1 :value 99}]]
      (is (= (average :value [:year :month] coll)
             [{:year 2015 :month 1 :value 20.0}
              {:year 2014 :month 1 :value 99.0}])))))

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

(deftest test-distance
  (testing "Distance"
    (let [positions [{:x 0 :y 0} {:x 20 :y 0} {:x 20 :y 30}]]
      (is (= (distance positions) [{:x 20 :y 30 :distance 50.0}])))))

(deftest test-add-column
  (testing "Add a column"
    (let [coll [{:x 20} {:x 50}]]
      (is (= (add-column :x :x1 inc coll) [{:x 20 :x1 21} {:x 50 :x1 51}])))))

(deftest test-words
  (testing "Words"
    (let [keys [{:k "a"} {:k "b"} {:k "."} {:k " "} {:k "c"}]]
      (is (= (words :k keys) [{:word "ab" :k "a"} {:word ". " :k "."} {:word "c" :k "c"}])))))
