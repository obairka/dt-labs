(ns bool-expr.conjunction-test
  (:require [clojure.test :refer :all]
            [bool-expr.model :refer :all]
            [bool-expr.helpers :refer :all]
            [bool-expr.operations :refer :all]))

(deftest conjunction-test
  (testing "conjunction: x & (a & b) & y"
    (let [conj (conjunction (variable :x) (conjunction (variable :a) (variable :b)) (variable :y))]
    	(is (conjunction? conj))
    	(is (every? variable? (rest conj)))
    	(is (= (set (map variable-name (rest conj))) (set '[:x :y :a :b])))
    	)))

(deftest only-distinct-test
  (testing
  	(is (= (set (only-distinct [(variable :a) (variable :b) (variable :c)])) (set [(variable :a) (variable :b) (variable :c)])))

  	))
