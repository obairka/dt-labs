(ns bool-expr.model
  (:gen-class))

(defn constant [value]
  "Create constant expression."
  {:pre [(or (true? value) (false? value))]}
  (list ::const value))

(defn constant? [expr]
  "Check if expression represents a constant."
  (= ::const (first expr)))

(defn constant-value [expr]
  "Get value of constant."
  {:pre [(constant? expr)]}
  (second expr))

(defn variable [name]
  "Create variable expression."
  {:pre [(keyword? name)]}
  (list ::var name))

(defn variable? [expr]
  "Check if expression represents a variable."
  (and (= ::var (first expr)) (keyword? (second expr))))

(defn variable-name [expr]
  "Get name of a variable represented by expr."
  {:pre [(variable? expr)]}
  (second expr))

(defn same-variables? [var1, var2]
  "Verify that var1 and var2 represent variables of the same name."
  (and
    (variable? var1)
    (variable? var2)
    (= (variable-name var1) (variable-name var2))))

; Helper functions

(defn atom? [expr]
  "Check if expr is an atom, i.e. a variable or constant."
  (or
    (variable? expr)
    (constant? expr)))

(defn expression? [expr]
  "Check if expr is a valid expression."
  (and (seq? expr) (not (empty? expr)) (keyword? (first expr))))

(defn expressions? [exprs]
  "Check if all elements in exprs are expressions."
  (and
    (seq exprs)
    (every? expression? exprs)))

; operations



