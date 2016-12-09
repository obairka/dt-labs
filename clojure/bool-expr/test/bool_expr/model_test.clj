(ns bool-expr.model-test
  (:require [clojure.test :refer :all]
            [bool-expr.model :refer :all]))

(deftest constant?-test
  (testing "constant?-test."
    (is (constant? (constant true)))
    (is (constant? (constant false)))
    ))

(deftest constant-value-test
  (testing "constant-value-test."
    (is (= true (constant-value (constant true))))
    (is (= false (constant-value (constant false))))
    ))

(deftest variable?-test
  (testing "variable?-test."
    (is variable? (variable :x))
    ))

(deftest variable-name-test
  (testing "variable-name-test."
    (is (= :x (variable-name (variable :x))))
    ))

(deftest same-variables?-test
  (testing "same-variables?-test."
    (is (same-variables? (variable :x) (variable :x)))
    ))

(deftest not-same-variables?-test
  (testing "not-same-variables?-test."
    (not (same-variables? (variable :x) (variable :x)))
    ))

(deftest atom?-test
  (testing "atom?-test."
    (is atom? (variable :x))
    (is atom? (constant true))
    ))
