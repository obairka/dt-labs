(ns boolean.dnf
  (:gen-class)
  (:require [boolean.core :refer :all]))

(defn transform-impl
	[expr]
	(cond 
		(atom? expr) 
			expr
		(implication? expr) 
			(disjunction (negation (transform-impl (nth expr 1))) (transform-impl (nth expr 2)))
		:default 
			(cons (first expr) (map transform-impl (rest expr)))))

(defmulti transform
	"Pushes all negation expr inwards."
    (fn [expr]
	    (if (negation? expr)	
			(first (second expr))
      		:default
      )))

(defmethod transform :default [expr]
	(if (atom? expr)
		expr 
		(cons (first expr) (map transform (rest expr)))))

; (not (not E))
(defmethod transform :neg [expr]
	(transform (second (second expr))))

; (not (& e1 e2))
(defmethod transform :conj [expr]
	(let [ e1 (nth (second expr) 1) e2 (nth (second expr) 2) ]
		(disjunction (transform (negation e1)) (transform (negation e2)))))

; (not (|| e1 e2))
(defmethod transform :disj [expr]
	(let [ e1 (nth (second expr) 1) e2 (nth (second expr) 2) ]
		(conjunction (transform (negation e1)) (transform (negation e2)))))

(defmethod transform :var [expr]
	expr)

(defmethod transform :const [expr]
  (if (true? (second (second expr)))
    (constant false)
    (constant true)))

(defn apply-distr
  [expr]
  (let [disjunction-part (first (filter disjunction? (rest expr)))
        other-part (remove (fn [x] (= x disjunction-part)) (rest expr))]
    (apply disjunction (map (fn [x] (apply conjunction x other-part)) (rest disjunction-part)))))

(defn transform-to-dnf
	[expr]
	(cond
		(basic-conj-or-primitive? expr) 
			expr
		(conjunction? expr)
			(if (every? basic-conj-or-primitive? (rest expr))
				expr
				(transform-to-dnf (apply-distr expr)))
		:default
			(disjunction 
				(transform-to-dnf (first (rest expr))) 
				(transform-to-dnf (second (rest expr))))))

(defn dnf
	[expr]
	(let [prepared-expr (transform (transform-impl expr))]
		(if (dnf? prepared-expr)
			prepared-expr
			(transform-to-dnf prepared-expr))))

(defn set-variable-value
	[name value expr]
	{:pre [(and (keyword? name) (or (true? value) (false? value)))]}
  	(cond
  		(and (variable? expr) (= name (variable-name expr)))
  			(constant value)
		(atom? expr)
			expr
		:default
			(cons (first expr) (map (partial set-variable-value name value) (rest expr)))
  		))
