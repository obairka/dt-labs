(ns streams.core
	(:gen-class))

(defn trapeze [f a b]
	(* 0.5 (Math/abs (- b a)) (+ (f a) (f b))))

(defn integrate 
	"Возращает первообразную функции f(t) на 0 x."
	[f delta]
	(let [
			generate_seq 
				(fn [op [x1 partial-sum]] 
					(let [x2 (op x1 delta)] 
						[x2 (+ partial-sum (trapeze f x1 x2))]))

			seq_pos (map second (iterate (partial generate_seq +) [0, 0]))
			seq_neg (map second (iterate (partial generate_seq -) [0, 0]))
		]
		(fn [x] 
			(let [
					seq (if (pos? x) seq_pos seq_neg)
					sign (if (neg? x) -1 1)
					n (quot (Math/abs x) delta)
					tail (mod x delta)
				]
				(+ 
					(* sign (nth seq n)) 
					(trapeze f (- x tail) x))))))