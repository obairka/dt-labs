(ns bool-expr.helpers
  (:gen-class)
  (:require [bool-expr.model :refer :all]))

(defn args [expr]
	"Get arguments of operator"
	{:pre [(expression? expr) (expressions? (next expr))]}
	(rest expr))

(defn normalize-assoc [op?, exprs]
	(let [	ops (filter op? exprs)
			other-exprs (remove op? exprs)]
		(if (empty? ops) 
			other-exprs 			
			(concat other-exprs (mapcat args ops) ))))

(defn fold-constants [f, exprs]
	(let [ consts (filter constant? exprs) 
			other-exprs (remove constant? exprs) 
			combined-const (f consts)]
		(cond 
			(= combined-const false)
				(list (constant false))
			(= combined-const true)
				(if (empty? other-exprs)
					(list (constant true))
					other-exprs)
			:default
				(cons (constant combined-const) other-exprs)
			)))