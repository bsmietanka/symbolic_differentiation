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
      (if (= (second expression) variable) 
        (list 'cos variable)
        (if (or (number? (second expression)) (or (symbol? (second expression)) (= (count (second expression)) 1)))
          0
          (list '*
            (differentiation (second expression) variable)
            (list 'cos (second expression)))))
    (cos? expression) 
      (if (= (second expression) variable) 
        (list '-sin variable)
        (if (or (number? (second expression)) (or (symbol? (second expression)) (= (count (second expression)) 1)))
          0
          (list '*
            (differentiation (second expression) variable)
            (list '-sin (second expression)))))
    (tg? expression) 
      (if (= (second expression) variable) 
        (list '/ 
          1 
          (list '\^ 
            (list 'cos variable) 
            2)) 
        (if (or (number? (second expression)) (or (symbol? (second expression)) (= (count (second expression)) 1)))
          0
          (list '*
            (differentiation (second expression) variable)
            (list '/
              1
              (list '\^ 
                (list 'cos (second expression)) 
                2)))))
    (ctg? expression) 
      (if (= (second expression) variable) 
        (list '/ 
          -1
          (list '\^ 
            (list 'sin variable)
            2)) 
        (if (or (number? (second expression)) (or (symbol? (second expression)) (= (count (second expression)) 1)))
          0
          (list '*
            (differentiation (second expression) variable)
            (list '/
              -1
              (list '\^ 
                (list 'sin (second expression)) 
                2)))))
    (sec? expression) 
      (if (= (second expression) variable) 
        (list '* 
          (list 'tg variable) 
          (list 'sec variable)) 
        0)
    (csc? expression) 
      (if (= (second expression) variable) 
        (list '* 
          (list '-ctg variable)
          (list 'csc variable)) 
        0)
    (exp? expression) 
      (if (= (second expression) variable) 
        (list 'exp variable)
        0)
    (ln? expression) 
      (if (= (second expression) variable) 
        (list '/ 
          1
          variable)
        0)
    (log? expression) 
      (if (= (second (next expression)) variable) 
        (list '/ 
          1
          (list '* 
            variable
            (list 'ln (second expression))))
        0)
    (sqrt? expression)
      (if (= (second (next expression)) variable) 
        (list '*  
          (list '/ 1 (second expression))
          (list '\^ variable
            (list '- 
              (list '/ 1 (second expression)) 
              1)))
        0)
    (arcsin? expression) 
      (if (= (second expression) variable) 
        (list '/ 
          1
          (list '\^ 
            (list '- 1 (list '\^ variable 2))
            (list '/ 1 2)
          ))
        0)
    (arccos? expression) 
      (if (= (second expression) variable) 
        (list '/ 
          -1
          (list '\^ 
            (list '- 1 (list '\^ variable 2))
            (list '/ 1 2)
          ))
        0)  
    (arctg? expression) 
      (if (= (second expression) variable) 
        (list '/ 
          1
          (list '+ (list '\^ variable 2) 1)
        )
        0)
    (arcctg? expression) 
      (if (= (second expression) variable) 
        (list '/ 
          -1
          (list '+ (list '\^ variable 2) 1)
        )
        0)
    (arcsec? expression) 
      (if (= (second expression) variable) 
        (list '/ 
          1
          (list '* 
            (list '\^ 
              (list '- 
                1 
                (list '/ 
                  1 
                  (list '\^ variable 2)))
              (list '/ 1 2))  
            (list '\^ variable 2))
        )
        0)
    (arccsc? expression) 
      (if (= (second expression) variable) 
        (list '/ 
          -1
          (list '* 
            (list '\^ 
              (list '- 
                1 
                (list '/ 
                  1 
                  (list '\^ variable 2)))
              (list '/ 1 2))  
            (list '\^ variable 2))
        )
        0)
    (sinh? expression) 
      (if (= (second expression) variable) 
        (list 'cosh variable)
        0)
    (cosh? expression) 
      (if (= (second expression) variable) 
        (list 'sinh variable)
        0)
    (tgh? expression) 
      (if (= (second expression) variable) 
        (list '\^ (list 'sech variable) 2)
        0)
    (ctgh? expression) 
      (if (= (second expression) variable) 
        (list '- 0 (list '\^ (list 'csch variable) 2))
        0)
    (sech? expression) 
      (if (= (second expression) variable) 
        (list '* (list 'tgh variable) (list '- 0 (list 'sech variable)))
        0)
    (csch? expression) 
      (if (= (second expression) variable) 
        (list '- 0 (list '* (list 'ctgh variable) (list 'csch variable)))
        0)
      
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
  (println (differentiation '(sqrt 3 x) 'x))
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