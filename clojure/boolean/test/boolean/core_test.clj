(ns boolean.core-test
  (:require [clojure.test :refer :all]
            [boolean.core :refer :all]))
(let [
		x (variable :x)
		y (variable :y)
		z (variable :z)
		xy (conjunction x y)
	]
	(deftest test-dnf?
		(is (dnf? x))
		(is (dnf? xy))	
		(is (dnf? (disjunction x y)))
		(is (dnf? (disjunction x (disjunction x z))))	
		(is (dnf? (conjunction xy (negation y))))
		(is (dnf? (disjunction xy z)))	
	))
