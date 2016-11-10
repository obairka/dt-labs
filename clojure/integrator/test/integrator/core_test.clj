(ns integrator.core-test
  (:require [clojure.test :refer :all]
            [integrator.core :refer :all]))

(defn float-eq [expected actual]
  (and
    (> actual (- expected 0.001))
    (< actual (+ expected 0.001))))

(letfn [(f [x] x)]
  (deftest trapezoidal-rule-test
    (is (= 1/2 (trapezoidal-rule f 0 1)))
    (is (= (float 3/8) (trapezoidal-rule f 0.5 1)))))

(let [f (integrate (fn [x] 1) 0.1)]
  (deftest const-integration
    (is (function? f))
    (is (float-eq 0 (f 0)))
    (is (float-eq 17 (f 17)))
    (is (float-eq 19 (f 19)))))

(let [f (integrate (fn [x] x) 0.1)]
  (deftest integration-test
    (is (float-eq 0 (f 0)))
    (is (float-eq 2 (f 2)))
    (is (float-eq 8 (f 4)))
    ))

(let [f (integrate (fn [x] (+ x (Math/sin x))) 0.1)]
  (deftest even-function-integration-test
    (is (float-eq 0 (f 0)))
    (is (= (f 2) (f -2)))
    ))

(let [f (integrate (fn [x] 1) 0.1)]
  (deftest perfomance
    (is (float-eq (time (f 125)) (time (f 125))))))
