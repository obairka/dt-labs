(ns boolean.dnf-test
  (:require [clojure.test :refer :all]
            [boolean.core :refer :all]
            [boolean.dnf :refer :all]))

(deftest transform-atom-test
	(is (= (constant true) (transform (constant true))))
	(is (= (constant false) (transform (constant false))))
 	(is (= (variable :x) (transform (variable :x)))))

(deftest transform-neg-neg-atom-test
	(is (= (constant true) (transform (negation (negation (constant true)))) ))
	(is (= (constant false) (transform (negation (negation (constant false)))) ))
	(is (= (variable :x) (transform (negation (negation (variable :x)))) )) )

(deftest transform-neg-conj-test
	(let [x (variable :x) y (variable :y)]
		(is (= (disjunction (negation x) (negation y)) (transform (negation (conjunction x y)))))
		(is (= (disjunction x (negation y)) (transform (negation (conjunction (negation x) y)))))
		))

(deftest transform-neg-disj-test
	(let [x (variable :x) y (variable :y)]
		(is (= (conjunction (negation x) (negation y)) (transform (negation (disjunction x y)))))
		(is (= (conjunction x (negation y)) (transform (negation (disjunction
		 (negation x) y)))))
		))

(deftest transform-complex-test
	(let [
			x (variable :x)
			y (variable :y)
			a (conjunction x (negation y))
			b (negation (disjunction (negation x) y))
			expr (negation (disjunction a b))
			expected (conjunction (disjunction (negation x) y) (disjunction (negation x) y))
		] 
		(is (= expected (transform expr)))))

(deftest transform-to-dnf-test
	(let [
			x (variable :x)
			y (variable :y)
			z (variable :z)
			expr (conjunction (disjunction x y) z)
			expected (disjunction (conjunction x z) (conjunction y z))
		]
		(is (= expected (transform-to-dnf expr)))
		(is (dnf? (transform-to-dnf expr) )) ))

(deftest transform-to-dnf-test-2
	(let [
			x (variable :x)
			y (variable :y)
			z (variable :z)
			expr (conjunction (disjunction x y) (conjunction y z))
			expected (disjunction (conjunction x (conjunction y z)) (conjunction y (conjunction y z)))
		]
		(is (= expected (transform-to-dnf expr)))
		(is (dnf? (transform-to-dnf expr) )) ))

(deftest solve-test
	(let [
			x (variable :x)
			y (variable :y)	
			z
			 (variable :z)	
			expr (conjunction (disjunction x y) (conjunction y z))
			expr-dnf (dnf expr)
			solved-by-x (assign-value :x false expr-dnf)
			solved-by-y (assign-value :y true solved-by-x)
			solved-by-z (assign-value :z true solved-by-y)
		]
		(is (= (constant true) (collapse solved-by-z) ))))


(deftest solve-test-2
	(let [
			x (variable :x)
			y (variable :y)	
			z (variable :z)	
			expr (conjunction (disjunction x y) (disjunction y z))
			expr-dnf (dnf expr)
			name-values {:x false, :y false, :z true}
			solved (assign-values name-values expr-dnf)
		]
		(is (= (constant false) (collapse solved) ))))


