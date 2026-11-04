(ns app.components-test
  (:require [cljs.test :refer [deftest is testing]]
            [app.components :as components]))

(deftest catalog-test
  (testing "the quantum register is the first component"
    (is (= :quantum-register (:id (first components/components))))
    (is (= :register (:kind (first components/components)))))

  (testing "all required gates are present in the specified order"
    (is (= [:h :x :y :z :s :t :sdag :tdag :rx :ry :rz :p :i]
           (mapv :id (filter #(= :gate (:kind %)) components/components))))
    (is (= 15 (count components/components))))

  (testing "a custom gate component is available"
    (is (= {:id :custom :label "Custom" :kind :custom}
           (components/by-id :custom))))

  (testing "component labels"
    (is (= "Quantum register" (:label (components/by-id :quantum-register))))
    (is (= "Sdag" (:label (components/by-id :sdag))))
    (is (= "Tdag" (:label (components/by-id :tdag))))
    (is (= "Rx" (:label (components/by-id :rx)))))

  (testing "unknown components are not found"
    (is (nil? (components/by-id :does-not-exist)))))
