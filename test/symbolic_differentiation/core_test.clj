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
    (is (= '(* 1 (cos x)) (differentiation '(sin x) 'x)))
    (is (= '(* 1 (cos y)) (differentiation '(sin y) 'y)))
    (is (= '(* 0 (cos x)) (differentiation '(sin x) 'y)))
    (is (= '(* 1 (- 0 (sin x))) (differentiation '(cos x) 'x)))
    (is (= '(* 1 (- 0 (sin y))) (differentiation '(cos y) 'y)))
    (is (= '(* 0 (- 0 (sin x))) (differentiation '(cos x) 'y)))
    (is (= '(* 1 (/ 1 (pow (cos x) 2))) (differentiation '(tg x) 'x)))
    (is (= '(* 1 (/ 1 (pow (cos y) 2))) (differentiation '(tg y) 'y)))
    (is (= '(* 0 (/ 1 (pow (cos x) 2))) (differentiation '(tg x) 'y)))
    (is (= '(* 1 (/ -1 (pow (sin x) 2))) (differentiation '(ctg x) 'x)))
    (is (= '(* 1 (/ -1 (pow (sin y) 2))) (differentiation '(ctg y) 'y)))
    (is (= '(* 0 (/ -1 (pow (sin x) 2))) (differentiation '(ctg x) 'y)))
    (is (= '(* 1 (* (tg x) (sec x))) (differentiation '(sec x) 'x)))
    (is (= '(* 1 (* (tg y) (sec y))) (differentiation '(sec y) 'y)))
    (is (= '(* 0 (* (tg x) (sec x))) (differentiation '(sec x) 'y)))
    (is (= '(* 1 (* (- 0 (ctg x)) (csc x))) (differentiation '(csc x) 'x)))
    (is (= '(* 1 (* (- 0 (ctg y)) (csc y))) (differentiation '(csc y) 'y)))
    (is (= '(* 0 (* (- 0 (ctg x)) (csc x))) (differentiation '(csc x) 'y)))
    )
)

(deftest exponential-diff-tests
  (testing "FIXME, I fail."
    (is (= '(* 1 (exp x)) (differentiation '(exp x) 'x)))
    (is (= '(* 1 (exp y)) (differentiation '(exp y) 'y)))
    (is (= '(* 0 (exp 123)) (differentiation '(exp 123) 'x)))    
  )
)

(deftest logarithm-diff-tests
  (testing "FIXME, I fail."
    (is (= '(* 1 (/ 1 x)) (differentiation '(ln x) 'x)))
    (is (= '(* 1 (/ 1 y)) (differentiation '(ln y) 'y)))
    (is (= '(* 0 (/ 1 123)) (differentiation '(ln 123) 'x)))
    (is (= '(* 1 (/ 1 (* x (ln 123)))) (differentiation '(log 123 x) 'x)))
    (is (= '(* 1 (/ 1 (* y (ln x)))) (differentiation '(log x y) 'y)))
    (is (= '(* 0 (/ 1 (* x (ln 123)))) (differentiation '(log 123 x) 'y)))
    (is (= '(* 0 (/ 1 (* 1234 (ln 123)))) (differentiation '(log 123 1234) 'y)))
    (is (= '(* 0 (/ 1 (* y (ln x)))) (differentiation '(log x y) 'x)))  
  )
)

(deftest natural-root-diff-tests
  (testing "FIXME, I fail."
    (is (= '(* 1 (* (/ 1 2) (pow x (- (/ 1 2) 1)))) (differentiation '(sqrt 2 x) 'x)))
    (is (= '(* 1 (* (/ 1 3) (pow x (- (/ 1 3) 1)))) (differentiation '(sqrt 3 x) 'x)))
    (is (= '(* 1 (* (/ 1 4) (pow x (- (/ 1 4) 1)))) (differentiation '(sqrt 4 x) 'x)))
  )
)

(deftest pow-diff-tests
  (testing "FIXME, I fail."
    (is (= '(* 1 (* 2 (pow x (- 2 1)))) (differentiation '(pow x 2) 'x)))
    (is (= '(* 1 (* 3 (pow x (- 3 1)))) (differentiation '(pow x 3) 'x)))
    (is (= '(* 1 (* 4 (pow x (- 4 1)))) (differentiation '(pow x 4) 'x)))
  )
)

(deftest arcsin-diff-tests
  (testing "FIXME, I fail."
    (is (= '(* 1 (/ 1 (pow (- 1 (pow x 2)) 0.5))) (differentiation '(arcsin x) 'x)))
  )
)

(deftest arccos-diff-tests
  (testing "FIXME, I fail."
    (is (= '(* 1 (/ -1 (pow (- 1 (pow x 2)) 0.5))) (differentiation '(arccos x) 'x)))
  )
)

(deftest arctg-diff-tests
  (testing "FIXME, I fail."
    (is (= '(* 1 (/ 1 (+ (pow x 2) 1))) (differentiation '(arctg x) 'x)))
  )
)

(deftest arcctg-diff-tests
  (testing "FIXME, I fail."
    (is (= '(* 1 (/ -1 (+ (pow x 2) 1))) (differentiation '(arcctg x) 'x)))
  )
)

(deftest arcsec-diff-tests
  (testing "FIXME, I fail."
    (is (= '(* 1 (/ 1 (* (pow (- 1 (/ 1 (pow x 2))) 0.5) (pow x 2)))) (differentiation '(arcsec x) 'x)))
  )
)

(deftest arccsc-diff-tests
  (testing "FIXME, I fail."
    (is (= '(* 1 (/ -1 (* (pow (- 1 (/ 1 (pow x 2))) 0.5) (pow x 2)))) (differentiation '(arccsc x) 'x)))
  )
)

(deftest sinh-diff-tests
  (testing "FIXME, I fail."
    (is (= '(* 1 (cosh x)) (differentiation '(sinh x) 'x)))
  )
)

(deftest cosh-diff-tests
  (testing "FIXME, I fail."
    (is (= '(* 1 (sinh x)) (differentiation '(cosh x) 'x)))
  )
)

(deftest tgh-diff-tests
  (testing "FIXME, I fail."
    (is (= '(* 1 (pow (sech x) 2)) (differentiation '(tgh x) 'x)))
  )
)

(deftest ctgh-diff-tests
  (testing "FIXME, I fail."
    (is (= '(* 1 (- 0 (pow (csch x) 2))) (differentiation '(ctgh x) 'x)))
  )
)

(deftest sech-diff-tests
  (testing "FIXME, I fail."
    (is (= '(* 1 (* (tgh x) (- 0 (sech x)))) (differentiation '(sech x) 'x)))
  )
)

(deftest csch-diff-tests
  (testing "FIXME, I fail."
    (is (= '(* 1 (- 0 (* (ctgh x) (csch x)))) (differentiation '(csch x) 'x)))
  )
)

(defn round5
  "Funkcja oblicza wartość funkcji i zaokrągla ją do 5 miejsc po przecinku"
  [x]
  (read-string (clojure.string/replace (format "%.5f" (eval (read-string x))) #"," "."))
)

(deftest eval-diff-test
  ;(sin(y*(tg(x))))' po x
  (def diff1 (str (differentiation '(sin (* __y (tg __x))) '__x)))
  (def diff1 (clojure.string/replace diff1 #"__x" "5"))
  (def diff1 (clojure.string/replace diff1 #"__y" "3"))
  ;(sin(2*tg(3/7*exp(x))))'
  (def diff2 (str (differentiation '(sin (* 2 (tg (* (/ 3 7) (exp __x))))) '__x)))
  (def diff2 (clojure.string/replace diff2 #"__x" "0.5"))
  ;((e^(sin(x)))/(tg(sin(x))))'
  (def diff3 (str (differentiation '(/ (exp (sin __x)) (tg (sin __x))) '__x)))
  (def diff3 (clojure.string/replace diff3 #"__x" "3"))

  (testing "FIXME, I fail."
    (is (= -28.10939 (round5 diff1))) ; Wartość pobrana z WolframAlpha.com (~ -28.10939088475794)
    (is (= -0.33232 (round5 diff2))) ; Wartość pobrana z WolframAlpha.com (~ -0.3323157682996881)
    (is (= 49.60236 (round5 diff3))) ; Wartość pobrana z WolframAlpha.com (~ 49.6023584095444777)
  )
)