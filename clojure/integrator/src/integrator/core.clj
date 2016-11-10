(ns integrator.core
  (:gen-class))

(defn trapezoidal-rule [f a b]
	(* (Math/abs (- b a)) (/ (+ (f a) (f b)) 2)))

(defn calc_step [f step dx]
	(if (<= step 0)
		0
		(let [b (* step dx)
			  a (- b dx)]
			(+ (calc_step f (dec step) dx)
			   (trapezoidal-rule f a b)))))

(defn integrate
	[f dx]
	(let [f_memo (memoize f) 
		  calc_step_memo (memoize calc_step)]
		(fn [x]
			(let [sign (if (neg? x) -1 1)
				  dx (* sign dx)
				  n (quot x dx)
				  tail (mod x dx)]		
			(* sign (+ (calc_step_memo f_memo n dx) (trapezoidal-rule f_memo (- x tail) x)))))))