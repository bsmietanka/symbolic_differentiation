(ns symbolic-differentiation.core-test
  (:require [clojure.test :refer :all]
            [symbolic-differentiation.core :refer :all]))

(deftest constant-diff-test
  (testing "FIXME, I fail."
    (is (= 0 (differentiation 1 "x")))
    (is (= 0 (differentiation -1000 "x")))
    (is (= 0 (differentiation (* 3 -3) "y")))
    (is (= 0 (differentiation 0 "abc")))    
  )
)

