(ns bool-expr.operations
  (:gen-class)
  (:require [bool-expr.model :refer :all]
  			[bool-expr.helpers :refer :all]))

(defn fold-conj [consts]
	(every? constant-value consts))

(defn fold-disj [consts]
	(some? constant-value consts))

(defn conjunction? [expr]
  "Check if expression is conjunction."
	(and
		(= ::conj (first expr))
		(expressions? (rest expr))))

(defn conjunction [& args]
	"Create conjunction expression."
	(let [ 
			fold (partial fold-constants fold-conj)
			normalize (partial normalize-assoc conjunction?)]

		(cons ::conj (fold (normalize args)))))

(defn disjunction? [expr]
  "Check if expression is disjunction."
	(and
		(= ::disj (first expr))
		(expressions? (rest expr))))

(defn disjunction [& args]
	"Create disjunction expression."
	(let [ 
			fold (partial fold-constants fold-disj)
			normalize (partial normalize-assoc disjunction?)]
		(cons ::conj (fold (normalize args)))))

