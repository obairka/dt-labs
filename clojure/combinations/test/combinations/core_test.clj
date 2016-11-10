(ns combinations.core-test
  (:require [clojure.test :refer :all]
            [combinations.core :refer :all]))

(defn pairs_not_equal?
	[comb]
	(every? 
		#(apply not= %) 
		(map list comb (rest comb)))	)

(deftest gen_comb-test
	(testing "generates combinations."
		(is (= 
				(set (gen_comb [1 2 3] 2))
				(set '((1 2) (1 3) (2 1) (2 3) (3 1) (3 2)))
			))))

(deftest gen_comb_no_equals_side_by_side-test
	(testing "combination has no two equals consequent objects."
		(let [combs (gen_comb [1 2 3] 3)]
			(is (every? pairs_not_equal? combs))
		)))

(deftest gen_comb_empty-set-test
	(testing "generates combinations: empty set."
		(is (= (gen_comb [] 2) []))
	))

(deftest gen_comb_zero-lenght-test
	(testing "generates combinations: zero length."
		(is (= (gen_comb ['a' 'b'] 0) [[]]))
	))
