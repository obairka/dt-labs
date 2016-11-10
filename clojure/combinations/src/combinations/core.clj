(ns combinations.core
  (:gen-class))

(defn gen_comb
	"Generate combinations of objects from specified set with specified length"
	[set len]
	{:pre [(number? len) (>= len 0)]}
	(cond
		(empty? set) []
		(zero? len) [[]]
		:else (let [prev_combs (gen_comb set (dec len))]
				(for [sym set w prev_combs :when (not= sym (first w))] (cons sym w))
			)))
