(ns symbolic-differentiation.core-test
  (:require [clojure.test :refer :all]
            [symbolic-differentiation.core :refer :all]))

(deftest constant-diff-tests
  (testing "FIXME, I fail."
    (is (= 0 (differentiation 1 'x')))
    (is (= 0 (differentiation -1000 'x')))
    (is (= 0 (differentiation (* 3 -3) 'y')))
    (is (= 0 (differentiation 0 "abc")))    
  )
)

(deftest variable-diff-tests
  (testing "FIXME, I fail."
    (is (= 1 (differentiation 'x' 'x')))
    (is (= 0 (differentiation 'y' 'x')))    
  )
)

(deftest trigonometrics-diff-tests
  (testing "FIXME, I fail."
    (is (= '(cos x) (differentiation '(sin x) 'x)))
    (is (= '(cos y) (differentiation '(sin y) 'y)))
    (is (= 0 (differentiation '(sin x) 'y)))
    (is (= '(-sin x) (differentiation '(cos x) 'x)))
    (is (= '(-sin y) (differentiation '(cos y) 'y)))
    (is (= 0 (differentiation '(cos x) 'y)))
    (is (= '(/ 1 (\^ (cos x) 2)) (differentiation '(tg x) 'x)))
    (is (= '(/ 1 (\^ (cos y) 2)) (differentiation '(tg y) 'y)))
    (is (= 0 (differentiation '(tg x) 'y)))
    (is (= '(/ 1 (\^ (-sin x) 2)) (differentiation '(ctg x) 'x)))
    (is (= '(/ 1 (\^ (-sin y) 2)) (differentiation '(ctg y) 'y)))
    (is (= 0 (differentiation '(ctg x) 'y)))
    (is (= '(* (tg x) (sec x)) (differentiation '(sec x) 'x)))
    (is (= '(* (tg y) (sec y)) (differentiation '(sec y) 'y)))
    (is (= 0 (differentiation '(sec x) 'y)))
    (is (= '(* (-ctg x) (csc x)) (differentiation '(csc x) 'x)))
    (is (= '(* (-ctg y) (csc y)) (differentiation '(csc y) 'y)))
    (is (= 0 (differentiation '(csc x) 'y)))
    )
)

(deftest exponential-diff-tests
  (testing "FIXME, I fail."
    (is (= '(exp x) (differentiation '(exp x) 'x)))
    (is (= '(exp y) (differentiation '(exp y) 'y)))
    (is (= 0 (differentiation '(exp 123) 'x)))    
  )
)

(deftest logarithm-diff-tests
  (testing "FIXME, I fail."
    (is (= '(/ 1 x) (differentiation '(ln x) 'x)))
    (is (= '(/ 1 y) (differentiation '(ln y) 'y)))
    (is (= 0 (differentiation '(ln 123) 'x)))
    (is (= '(/ 1 (* x (ln 123))) (differentiation '(log 123 x) 'x)))
    (is (= '(/ 1 (* y (ln x))) (differentiation '(log x y) 'y)))
    (is (= 0 (differentiation '(log 123 x) 'y)))
    (is (= 0 (differentiation '(log 123 1234) 'y)))
    (is (= 0 (differentiation '(log x y) 'x)))  
  )
)