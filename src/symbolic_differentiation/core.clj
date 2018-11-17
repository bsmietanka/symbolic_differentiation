(ns symbolic-differentiation.core
  (:gen-class)
  (:require [functions.polynomial :as poly])
  (:require [utils.math :as cm]) ; cm as Custom Math
) 

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
;do zrobienia









(defn addition? [x] (and (=(count x) 3) (= (first x) '+)))
(defn multiplication? [x] (and (=(count x) 3) (= (first x) '*)))

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
        (list '-sin (second expression)))
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
          (list '-ctg (second expression))
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
            (list '/ 1 2)
          )))
    (arccos? expression) 
      (list '*
        (differentiation (second expression) variable)
        (list '/ 
          -1
          (list 'pow 
            (list '- 1 (list 'pow (second expression) 2))
            (list '/ 1 2)
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
              (list '/ 1 2))  
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
              (list '/ 1 2))  
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
    (multiplication? expression) 
      (list '+ 
        (list '* 
          (differentiation (second expression) variable)
          (second (rest expression))) 
        (list '* 
          (second expression) 
          (differentiation (second (rest expression)) variable)))
  )
)

(defn -main
  [& args]
  
  ;(println (differentiation '(tg (ctg x)) 'x))
  ;(println (differentiation '(sqrt 3 x) 'x))
  (def x 5)
  (println (differentiation '(ln (tg x)) 'x))
  ;(eval (differentiation '(ln x) 'x))
  ;(eval ((let [x 5] (differentiation '(ln (sinh x)) 'x))))
  ; (println (nth 0 '(sin x)))
  ; (def x 3)
  ; (println (+ 2 3))
  ; (println "Hello, World!")
  ; (println (* 7 3))
  ; (println "Test x^3. Is ok?")
  ; (println (poly/IsFunctionOk "x^3"))
  ; (println "Test x^3. Derivative is equal to:")
  ; (println (poly/Diff "x^3"))
  ;(println (eval (poly/Diff "x^3")))
)