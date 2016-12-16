(ns boolean.core
  (:gen-class))

(defn constant 
	"Constructor of constant"
	[value]
	{:pre [(or (true? value) (false? value))]}
	(list :const value))

(defn constant? 
	"Checks if expr is constant"
	[expr]
	(= :const (first expr)))

(defn constant-value 
	"Returns value of constant"
	[expr]
	{:pre [(constant? expr)]}
	(second expr))

(defn variable 
	"Contructor of variable"
	[name]
	{:pre [(keyword? name)]}
	(list :var name))

(defn variable? [expr]
	(and (= :var (first expr)) (keyword? (second expr))))

(defn variable-name 
	"Returns variable's name"
	[expr]
	{:pre [(variable? expr)]}
	(second expr))

(defn same-variables? 
	"Checks if var1 and var2 are same variables"
	[var1, var2]
	(and
		(variable? var1) 
		(variable? var2)
		(= (variable-name var1) (variable-name var2))))

(defn negation 
	"Constructor for inversion of expression exprs."
	[expr]
	(list :neg expr))

(defn negation? [expr]
	"Checks if expr is negation"
	(= :neg (first expr)))

(defn atom?
	"Checks if expr is an atom (a variable or a constant)."
	[expr]
	(or (constant? expr) (variable? expr)))

(defn primitive?
	"Checks if expr is a primitive (an atom or negation of an atom)."
	[expr]
	(or (atom? expr) (and (negation? expr) (atom? (second expr)))))

(defn conjunction
	"Constructor for conjunction of expressions."
	[expr1 expr2]
	(list :conj expr1 expr2))

(defn conjunction? 
	"Checks if expression is conjunction"
	[expr]
  	(= (first expr) :conj))

(defn disjunction 
	"Constructor for disjunction of expressions"
	[expr1 expr2]
	(list :disj expr1 expr2))

(defn disjunction? 
	"Checks if expr is disjunction"
	[expr]
	(= :disj (first expr)))

(defn implication 
	"Constructor for implication of expressions"
	[from, to]
	(list :impl from to))

(defn implication? 
	"Checks if expr is implication"
	[expr]
	(= :impl (first expr)))

(defn basic-conj-or-primitive? [expr]
	(or 
		(primitive? expr)
		(and
			(= :conj (first expr)) 
			(every? basic-conj-or-primitive? (rest expr)) )))

(defn dnf? [expr]
	(or (basic-conj-or-primitive? expr)
		(and 
			(disjunction? expr)
			(dnf? (first (rest expr)))
			(dnf? (second (rest expr)))
		)))


