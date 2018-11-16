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

(deftest natural-root-diff-tests
  (testing "FIXME, I fail."
    (is (= '(* (/ 1 2) (\^ x (- (/ 1 2) 1))) (differentiation '(sqrt 2 x) 'x)))
    (is (= '(* (/ 1 3) (\^ x (- (/ 1 3) 1))) (differentiation '(sqrt 3 x) 'x)))
    (is (= '(* (/ 1 4) (\^ x (- (/ 1 4) 1))) (differentiation '(sqrt 4 x) 'x)))
  )
)

(deftest arcsin-diff-tests
  (testing "FIXME, I fail."
    (is (= '(/ 1 (\^ (- 1 (\^ x 2)) (/ 1 2))) (differentiation '(arcsin x) 'x)))
  )
)

(deftest arccos-diff-tests
  (testing "FIXME, I fail."
    (is (= '(/ -1 (\^ (- 1 (\^ x 2)) (/ 1 2))) (differentiation '(arccos x) 'x)))
  )
)

(deftest arctg-diff-tests
  (testing "FIXME, I fail."
    (is (= '(/ 1 (+ (\^ x 2) 1)) (differentiation '(arctg x) 'x)))
  )
)

(deftest arcctg-diff-tests
  (testing "FIXME, I fail."
    (is (= '(/ -1 (+ (\^ x 2) 1)) (differentiation '(arcctg x) 'x)))
  )
)

(deftest arcsec-diff-tests
  (testing "FIXME, I fail."
    (is (= '(/ 1 (* (\^ (- 1 (/ 1 (\^ x 2))) (/ 1 2)) (\^ x 2))) (differentiation '(arcsec x) 'x)))
  )
)

(deftest arccsc-diff-tests
  (testing "FIXME, I fail."
    (is (= '(/ -1 (* (\^ (- 1 (/ 1 (\^ x 2))) (/ 1 2)) (\^ x 2))) (differentiation '(arccsc x) 'x)))
  )
)

(deftest sinh-diff-tests
  (testing "FIXME, I fail."
    (is (= '(cosh x) (differentiation '(sinh x) 'x)))
  )
)

(deftest cosh-diff-tests
  (testing "FIXME, I fail."
    (is (= '(sinh x) (differentiation '(cosh x) 'x)))
  )
)

(deftest tgh-diff-tests
  (testing "FIXME, I fail."
    (is (= '(\^ (sech x) 2) (differentiation '(tgh x) 'x)))
  )
)

(deftest ctgh-diff-tests
  (testing "FIXME, I fail."
    (is (= '(- 0 (\^ (csch x) 2)) (differentiation '(ctgh x) 'x)))
  )
)

(deftest sech-diff-tests
  (testing "FIXME, I fail."
    (is (= '(* (tgh x) (- 0 (sech x))) (differentiation '(sech x) 'x)))
  )
)

(deftest csch-diff-tests
  (testing "FIXME, I fail."
    (is (= '(- 0 (* (ctgh x) (csch x))) (differentiation '(csch x) 'x)))
  )
)