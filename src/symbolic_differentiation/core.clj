(ns symbolic-differentiation.core
  (:gen-class)
  (:require [functions.polynomial :as poly])
  (:require [utils.math :as cm]) ; cm as Custom Math
) 

(defn sin [x] (Math/sin x))
(defn cos [x] (Math/cos x))
(defn tg [x] (Math/tan x))
(defn ctg [x] (/ 1 (Math/tan x)))
(defn sec [x] (/ 1 (Math/cos x)))
(defn csc [x] (/ 1 (Math/sin x)))
(defn exp [x] (Math/exp x))
(defn ln [x] (Math/log x))
(defn log [x, y] (/ (Math/log y) (Math/log x)))
(defn sqrt [x, y] (Math/pow x (/ 1 y)))
(defn arcsin [x] (Math/asin x))
(defn arccos [x] (Math/acos x))
(defn arctg [x] (Math/atan x))
(defn arcctg [x] (Math/atan (/ 1 x)))
(defn arcsec [x] (Math/acos (/ 1 x)))
(defn arccsc [x] (Math/asin (/ 1 x)))
(defn sinh [x] (Math/sinh x))
(defn cosh [x] (Math/cosh x))
(defn tgh [x] (Math/tanh x))
(defn ctgh [x] (/ (cosh x) (sinh x)))
(defn sech [x] (/ 1 (cosh x)))
(defn csch [x] (/ 1 (sinh x)))
(defn pow [x, y] (Math/pow x y))

(defn sin? [x] (and (= (count x) 2) (= (first x) 'sin)))
(defn cos? [x] (and (= (count x) 2) (= (first x) 'cos)))
(defn tg? [x] (and (= (count x) 2) (= (first x) 'tg)))
(defn ctg? [x] (and (= (count x) 2) (= (first x) 'ctg)))
(defn sec? [x] (and (= (count x) 2) (= (first x) 'sec)))
(defn csc? [x] (and (= (count x) 2) (= (first x) 'csc)))
(defn exp? [x] (and (= (count x) 2) (= (first x) 'exp)))
(defn ln? [x] (and (= (count x) 2) (= (first x) 'ln)))
(defn log? [x] (and (= (count x) 3) (= (first x) 'log)))
(defn sqrt? [x] (and (= (count x) 3) (= (first x) 'sqrt)))
(defn arcsin? [x] (and (= (count x) 2) (= (first x) 'arcsin)))
(defn arccos? [x] (and (= (count x) 2) (= (first x) 'arccos)))
(defn arctg? [x] (and (= (count x) 2) (= (first x) 'arctg)))
(defn arcctg? [x] (and (= (count x) 2) (= (first x) 'arcctg)))
(defn arcsec? [x] (and (= (count x) 2) (= (first x) 'arcsec)))
(defn arccsc? [x] (and (= (count x) 2) (= (first x) 'arccsc)))
(defn sinh? [x] (and (= (count x) 2) (= (first x) 'sinh)))
(defn cosh? [x] (and (= (count x) 2) (= (first x) 'cosh)))
(defn tgh? [x] (and (= (count x) 2) (= (first x) 'tgh)))
(defn ctgh? [x] (and (= (count x) 2) (= (first x) 'ctgh)))
(defn sech? [x] (and (= (count x) 2) (= (first x) 'sech)))
(defn csch? [x] (and (= (count x) 2) (= (first x) 'csch)))
(defn pow? [x] (and (= (count x) 3) (= (first x) 'pow)))

(defn addition? [x] (and (=(count x) 3) (= (first x) '+)))
(defn subtraction? [x] (and (=(count x) 3) (= (first x) '-)))
(defn multiplication? [x] (and (=(count x) 3) (= (first x) '*)))
(defn division? [x] (and (=(count x) 3) (= (first x) '/)))

(defn differentiation 
  [expression variable]
  (cond
    (number? expression) 
      0
    (symbol? expression) 
      (if (= expression variable) 
        1 
        0)
    (sin? expression) 
      (list '*
        (differentiation (second expression) variable)
        (list 'cos (second expression)))
    (cos? expression) 
      (list '*
        (differentiation (second expression) variable)
        (list '- 0 (list 'sin (second expression))))
    (tg? expression) 
      (list '*
        (differentiation (second expression) variable)
        (list '/
          1
          (list 'pow 
            (list 'cos (second expression)) 
            2)))
    (ctg? expression) 
      (list '*
        (differentiation (second expression) variable)
        (list '/
          -1
          (list 'pow 
            (list 'sin (second expression)) 
            2)))
    (sec? expression) 
      (list '*
        (differentiation (second expression) variable)
        (list '* 
          (list 'tg (second expression)) 
          (list 'sec (second expression))))
    (csc? expression) 
      (list '*
        (differentiation (second expression) variable)
        (list '* 
          (list '- 0 (list 'ctg (second expression)))
          (list 'csc (second expression))))
    (exp? expression) 
      (list '*
        (differentiation (second expression) variable)
        (list 'exp (second expression)))
    (ln? expression) 
      (list '* 
        (differentiation (second expression) variable)
        (list '/ 1 (second expression))
      )
    (log? expression) 
      (list '* 
        (differentiation (second (next expression)) variable)
        (list '/ 
          1
          (list '* 
            (second (next expression))
            (list 'ln (second expression)))))
    (sqrt? expression)
      (list '* 
        (differentiation (second (next expression)) variable)
        (list '*  
          (list '/ 1 (second expression))
          (list 'pow (second (next expression))
            (list '- 
              (list '/ 1 (second expression)) 
              1))))
    (pow? expression)
      (list '* 
        (differentiation (second expression) variable)
        (list '*  
          (second (next expression))
          (list 'pow  (next expression)
            (list '- 
              (list (second (next expression)) 
              1)))))
    (arcsin? expression) 
      (list '*
        (differentiation (second expression) variable)
        (list '/ 
          1
          (list 'pow 
            (list '- 1 (list 'pow (second expression) 2))
            0.5
          )))
    (arccos? expression) 
      (list '*
        (differentiation (second expression) variable)
        (list '/ 
          -1
          (list 'pow 
            (list '- 1 (list 'pow (second expression) 2))
            0.5
          )))  
    (arctg? expression) 
      (list '*
        (differentiation (second expression) variable)
        (list '/ 
          1
          (list '+ (list 'pow (second expression) 2) 1)
        ))
    (arcctg? expression) 
      (list '*
        (differentiation (second expression) variable)
        (list '/ 
          -1
          (list '+ (list 'pow (second expression) 2) 1)
        ))
    (arcsec? expression) 
      (list '*
        (differentiation (second expression) variable)
        (list '/ 
          1
          (list '* 
            (list 'pow 
              (list '- 
                1 
                (list '/ 
                  1 
                  (list 'pow (second expression) 2)))
              0.5)  
            (list 'pow (second expression) 2))
        ))
    (arccsc? expression) 
      (list '*
        (differentiation (second expression) variable)
        (list '/ 
          -1
          (list '* 
            (list 'pow 
              (list '- 
                1 
                (list '/ 
                  1 
                  (list 'pow (second expression) 2)))
              0.5)  
            (list 'pow (second expression) 2))
        ))
    (sinh? expression) 
      (list '*
        (differentiation (second expression) variable)
        (list 'cosh (second expression)))
    (cosh? expression) 
      (list '*
        (differentiation (second expression) variable)
        (list 'sinh (second expression)))
    (tgh? expression) 
      (list '*
        (differentiation (second expression) variable)
        (list 'pow (list 'sech (second expression)) 2))
    (ctgh? expression) 
      (list '*
        (differentiation (second expression) variable)
        (list '- 0 (list 'pow (list 'csch (second expression)) 2)))
    (sech? expression) 
      (list '*
        (differentiation (second expression) variable) 
        (list '* (list 'tgh (second expression)) (list '- 0 (list 'sech (second expression)))))
    (csch? expression) 
      (list '*
        (differentiation (second expression) variable) 
        (list '- 0 (list '* (list 'ctgh (second expression)) (list 'csch (second expression)))))
    (addition? expression)
      (list '+ 
        (differentiation (second expression) variable)
        (differentiation (second (rest expression)) variable))
    (subtraction? expression)
      (list '- 
        (differentiation (second expression) variable)
        (differentiation (second (rest expression)) variable))
    (multiplication? expression) 
      (list '+ 
        (list '* 
          (differentiation (second expression) variable)
          (second (rest expression))) 
        (list '* 
          (second expression) 
          (differentiation (second (rest expression)) variable)))
    (division? expression)
      (list '/
          (list '-
            (list '* 
              (differentiation (second expression) variable)
              (second (rest expression))) 
            (list '* 
              (second expression) 
              (differentiation (second (rest expression)) variable)))
          (list 'pow
            (second (rest expression))
            2))
  )
)

(defn differentiation-value
  [expression variable argument-value]
  (def diff-string (differentiation expression variable))
  (eval(read-string (clojure.string/join " " ["(" "def" variable argument-value ")"])))
  (eval(read-string (pr-str diff-string)))
)

(defn diff-eval
  [diff variable argument-value]
  (def diff-string diff)
  (let 
    [pom (resolve variable)] 
    (do
     
      (if (not= pom nil)
        (let [val (eval variable)] 
          (do
            (eval(read-string (clojure.string/join " " ["(" "def" variable argument-value ")"])))
            (let [result (eval(read-string (pr-str diff-string)))] 
              (do
                (eval(read-string (clojure.string/join " " ["(" "def" variable val ")"])))
                result
              )
            )
          ))
        (do
          (eval(read-string (clojure.string/join " " ["(" "def" variable argument-value ")"])))
          (eval(read-string (pr-str diff-string)))
        )
      )
    
    ) 
  )
)

(defn function-multiple-differentiation-values
  [expression variable argument-values]
    (loop [expression (differentiation expression variable) variable variable argument-values argument-values counted []] 
      (do
        (if (= (count argument-values) 0)
          counted
          (recur expression variable (next argument-values) (conj counted (diff-eval expression variable (first argument-values))))
        )
      )
    )
)

(defn function-multiple-values
  [expression variable argument-values]
    (loop [expression expression variable variable argument-values argument-values counted []] 
      (do
        (if (= (count argument-values) 0)
          counted
          (recur expression variable (next argument-values) (conj counted (diff-eval expression variable (first argument-values))))
        )
      )
    )
)


(defn -main
  [& args]
  (ns symbolic-differentiation.core)
  (def y 3)
  (def x 5)
  (println (differentiation '(ln (tg x)) 'x))
  (println (function-multiple-differentiation-values '(ln (tg x)) 'x '(1 2 3 4)))
  (println (function-multiple-values '(* y (ln x)) 'x '(1 2 3 4)))
  (println (eval (differentiation '(sin (* y (tg x))) 'x)))
)