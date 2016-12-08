(ns streams.core
	(:gen-class))

(defn trapeze [f a b]
	(* 0.5 (Math/abs (- b a)) (+ (f a) (f b))))

(defn trapeze-ex [f [a b]]
	(trapeze f a b))

(defn integrate 
	"Возращает функцию fn [x], которая вычисляет интеграл заданной функции f(t) на 0 x."
	[f delta]
	(fn [x] 
		(let [
				sign (if (neg? x) -1 1)
				delta (* sign delta)
				n (quot x delta)
				tail (mod x delta)
				trapeze-f (partial trapeze-ex f)
				integral (map 
							trapeze-f 
							(iterate 
								(fn [[start, end]] [end (+ end delta)]) 
								[0 delta]))
			]
			
			(* 
				sign 
				(+ 
					(apply +' (take n integral)) 
					(trapeze f (- x tail) x))))))