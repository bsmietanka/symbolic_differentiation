(ns symbolic-differentiation.core-test
  (:require [clojure.test :refer :all]
            [symbolic-differentiation.core :refer :all]))

(deftest constant-diff-test
  (testing "FIXME, I fail."
    (is (= 0 (differentiation 1 'x')))
    (is (= 0 (differentiation -1000 'x')))
    (is (= 0 (differentiation (* 3 -3) 'y')))
    (is (= 0 (differentiation 0 "abc")))    
  )
)

(deftest variable-diff-test
  (testing "FIXME, I fail."
    (is (= 1 (differentiation 'x' 'x')))
    (is (= 0 (differentiation 'y' 'x')))    
  )
)

(deftest trigonometrix-diff-test
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
