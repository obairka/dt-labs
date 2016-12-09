(ns bool-expr.operations-test
  (:require [clojure.test :refer :all]
            [bool-expr.model :refer :all]
            [bool-expr.operations :refer :all]))

(defn in? 
  "true if coll contains element"
  [coll elem]  
  (some #(= elem %) coll))

(defn contains-names? [args, names]
	(every? (fn [name] (in? (map (fn [arg] (variable-name arg)) args) name)) names))

(deftest conjunction-test
  (testing "conjunction: x & y"
    (let [conj (conjunction (variable :x) (variable :y))]
    	(is (conjunction? conj))
    	(is (= 2 (count (rest conj))))
    	(is (contains-names? (rest conj) [:x :y]))
    	)))

(deftest conjunction-fold-constants-test
  (testing "conjunction fold constants"
    (let [conj (conjunction (constant true) (constant false))]
    	(is (conjunction? conj))
    	(is (= 1 (count (rest conj))))
    	(is (constant? (second conj)))
    	(not (constant-value (second conj)))
    	)))

(deftest conjunction-fold-constants-with-variable-test
  (testing "conjunction fold constants with variables"
    (let [conj (conjunction (constant true) (variable :x) (constant true) )]
    	(is (conjunction? conj))
    	(is (= 1 (count (rest conj))))
    	(is (contains-names? (rest conj) [:x]))
    	)))

(deftest conjunction-normalize-test
  (testing "conjunction: x1 & (x2 & x3) = x1 & x2 & x3"
    (let [conj (conjunction (variable :x1) (conjunction (variable :x2) (variable :x3)))]
    	(is (conjunction? conj))
    	(is (every? variable? (rest conj)))
    	(is (= 3 (count (rest conj))))
    	(is (contains-names? (rest conj) [:x1 :x2 :x3]))
    	)))

(deftest conjunction?-atom-test
  (testing "conjunction? atoms"
    (is (conjunction? (conjunction (variable :x) (variable :y)) ))
    (is (conjunction? (conjunction (variable :x) (variable :y) (variable :z)) )) 
    (is (conjunction? (conjunction (constant false) (constant false)) )) 
    (is (conjunction? (conjunction (constant false) (variable :x)) )) 
    ))
