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
(defn exp? [x] (and (= (count x) 2) (= (first x) 'cos)))
(defn ln? [x] (and (= (count x) 2) (= (first x) 'cos)))
(defn log? [x] (and (= (count x) 2) (= (first x) 'cos)))
(defn arcsin? [x] (and (= (count x) 2) (= (first x) 'cos)))
(defn arccos? [x] (and (= (count x) 2) (= (first x) 'cos)))
(defn arctg? [x] (and (= (count x) 2) (= (first x) 'cos)))
(defn arcctg? [x] (and (= (count x) 2) (= (first x) 'cos)))
(defn arcsec? [x] (and (= (count x) 2) (= (first x) 'cos)))
(defn arccsc? [x] (and (= (count x) 2) (= (first x) 'cos)))
(defn sqrt? [x] (and (= (count x) 2) (= (first x) 'cos)))
(defn sinh? [x] (and (= (count x) 2) (= (first x) 'cos)))
(defn cosh? [x] (and (= (count x) 2) (= (first x) 'cos)))
(defn tgh? [x] (and (= (count x) 2) (= (first x) 'cos)))
(defn ctgh? [x] (and (= (count x) 2) (= (first x) 'cos)))
(defn sech? [x] (and (= (count x) 2) (= (first x) 'cos)))
(defn ctgh? [x] (and (= (count x) 2) (= (first x) 'cos)))


(defn differentiation 
  [expression variable]
  (cond
    (number? expression) 0 ; d/dx const = 0
    (symbol? expression) (if (= expression variable) 1 0) ; d/dx x = 1, d/dx y = 0
    (sin? expression) (cond (= (second expression) variable)(list 'cos variable) :else 0)
    (cos? expression) (cond (= (second expression) variable)(list '-sin variable) :else 0)
    (tg? expression) (cond (= (second expression) variable)(list '/ 1 (list '\^ (list 'cos variable) 2)) :else 0)
    (ctg? expression) (cond (= (second expression) variable)(list '/ 1 (list '\^ (list '-sin variable) 2)) :else 0)
    (sec? expression) (cond (= (second expression) variable)(list '* (list 'tg variable) (list 'sec variable)) :else 0)
    (csc? expression) (cond (= (second expression) variable)(list '* (list '-ctg variable) (list 'csc variable)) :else 0)
    
    ;funkcje trygonometryczne, logarytmiczne, itp.
    ;dalej rekurencja dla dodawania i mnożenia wyrażeń
  )
)

(defn -main
  [& args]
  
  (println (differentiation '(ctg x) 'x))
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