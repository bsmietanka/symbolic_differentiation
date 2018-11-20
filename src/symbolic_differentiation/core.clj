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
          (list 'pow  (second expression)
            (list '- 
              (second (next expression)) 
              1))))
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
  "Funkcja liczy wartość danej funkcji w danym punkcie. Oryginalne wartości zmiennych pozostają bez zmian"
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
        (do ;else
          (eval(read-string (clojure.string/join " " ["(" "def" variable argument-value ")"])))
          (eval(read-string (pr-str diff-string)))
        )
      )
    ) 
  )
)

(defn function-multiple-differentiation-values
  "Funkcja liczy pochodną podanej w parametrze funkcji i oblicza wartości pochodnej w zadanych punktach"
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
  "Funkcja oblicza wartości funkcji w zadanych punktach"
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


(defn optimize-expression_priv
  [expression]
  (if (list? expression)
    (let [expr expression]
      (if (list? expr)
        (do
          (cond 
            (and (= (first expr) '*) (= (second expr) 1))
              (do
                (def __optimized__ (conj __optimized__ "("))
                (optimize-expression_priv (second (next expr)))
                (def __optimized__ (conj __optimized__ ")"))
              )
            (and (= (first expr) '*) (= (second expr) 0))
              (optimize-expression_priv '(0))
            (and (= (first expr) '+) (= (second expr) 0))
              (do
                (def __optimized__ (conj __optimized__ "("))
                (optimize-expression_priv (second (next expr)))
                (def __optimized__ (conj __optimized__ ")"))
              )
            (and (= (first expr) '-) (= (second (next expr)) 0))
              (do
                (def __optimized__ (conj __optimized__ "("))
                (optimize-expression_priv (second expr))
                (def __optimized__ (conj __optimized__ ")"))
              )
            (and (= (first expr) '/) (= (second (next expr)) 1))
              (do
                (def __optimized__ (conj __optimized__ "("))
                (optimize-expression_priv (second expr))
                (def __optimized__ (conj __optimized__ ")"))
              )
            (= 1 1)
              (do
              (def __optimized__ (conj __optimized__ "("))
              (doseq [x expr]  (optimize-expression_priv x))
              (def __optimized__ (conj __optimized__ ")"))
              )
          )
        )
        (do ;else
          (if (not= nil expr)
            (def __optimized__ (conj __optimized__ expr))
          )
        )
      )
    )
    (if (not= nil expression) ;else
      (def __optimized__ (conj __optimized__ expression))
    )  
  )
  
  __optimized__
)

(defn remove_index
  [vec index]
  (let [coll vec
    i index]
    (into [] (concat (subvec coll 0 i)
        (subvec coll (inc i)))))
)

(defn remove_doubled_brackets_priv
  [str]
  (loop [str str open 0 delete false index 0]
    (if (>= index (count str)) 
      str
      (do
        (if (= false delete)
          (if (and (= (nth str index) "(") (= (nth str (+ index 1)) "(")) ;))
            (recur (remove_index str index) 1 true (+ index 1))
            (recur str 0 false (+ index 1))
          )
          (if (= (nth str index) "(") ;)
            (recur str (+ open 1) true (+ index 1))
            (if (= (nth str index) ")") ;(
              (if (= open 1)
                (recur (remove_index str index) 0 false (+ index 1))
                (recur str (- open 1) true (+ index 1))
              )
              (recur str open true (+ index 1))
            )
          )
        )
      )
    )
  )
)

(defn remove_doubled_brackets
  [str]
  (loop [orig str res (remove_doubled_brackets_priv orig)]
    (if (= orig res)
      res
      (recur res (remove_doubled_brackets_priv res))
    )
  )
)

(defn optimize-expression
  [expression]
  (def __optimized__ [])
  (loop [expression expression result (optimize-expression_priv expression)]
    (do
      (def __optimized__ [])
      (let 
        [res (read-string (clojure.string/replace (clojure.string/join " " (remove_doubled_brackets result)) (re-pattern "\\(\\s0\\s\\)") "0"))]
        (do
            (if (= res expression)
              res
              (recur res (optimize-expression_priv res))
            )
        )
      )
    )
  )
)  

(defn nth-differentiation
  "Funkcja liczy n-tą pochodną wyrażenia" 
  [expression variable degree]
  (loop [expression expression variable variable degree degree]
    (if (< degree 1)
      (optimize-expression expression)
      (recur (optimize-expression(differentiation expression variable)) variable (- degree 1))))
)

(defn function-multiple-nth-differentiation-values
  "Funkcja liczy n-tą pochodną podanej w parametrze funkcji i oblicza wartości pochodnej w zadanych punktach"
  [expression variable argument-values degree]
    (loop [expression (nth-differentiation expression variable degree) variable variable argument-values argument-values counted []] 
      (do
        (if (= (count argument-values) 0)
          counted
          (recur expression variable (next argument-values) (conj counted (diff-eval expression variable (first argument-values))))
        )
      )
    )
)

(defmacro info []
  (loop [
    func-names ["(differentiation function x) - function - funkcja, x - zmienna po ktorej funkcja bedzie rozniczkowana" 
      "(optimize-expression function) - funkcja optymalizuje wyrazenie - odpowiednio zamienia mnozenie przez 1 i przez 0" 
      "(function-multiple-differentiation-values function x values) - funkcja oblicza pochodna danej funkcji, a nastepnie oblicza wartosci obliczonej pochodnej w zadanych punktach"
      "(function-multiple-values function x values) - funkcja oblicza wartosci funkcji w zadanych punktach"
      "(function-multiple-values-macro function x values) - makro oblicza wartosci funkcji w zadanych punktach"
      "(nth-differentiation function x degree) - function - funkcja, x - zmienna po ktorej funkcja bedzie rozniczkowana, degree - stopien pochodnej" 
      "(function-multiple-nth-differentiation-values function x values degree) - funkcja oblicza n-ta pochodna danejt funckji, a nastepnie oblicza wartosci obliczonej pochodnej w zadanych punktach" 
      "(function-multiple-differentiation-values-macro function x values) - makro oblicza pochodna danej funkcji, a nastepnie oblicza wartosci obliczonej pochodnej w zadanych punktach"
      ] 
    result ['do]]
    (if (= 0 (count func-names))
      (eval (apply list result))
      (recur (next func-names) (conj (conj result (list 'println (first func-names))) (list 'println "")))
    )
  )  
)

(defmacro function-multiple-values-macro
  [expression variable values]
  ;(println expression variable values)
  (loop [expression expression variable variable values values result []]
    (if (= 0 (count values))
      result
      (do
        ;(println result)
        (recur expression variable (next values) (conj result (list diff-eval expression variable (first values))))
      )
    )
  )
)

(defmacro function-multiple-differentiation-values-macro
  [expression variable values]
  ;(println expression variable values)
  (loop [expression (list differentiation expression variable) variable variable values values result []]
    (if (= 0 (count values))
      result
      (do
        ;(println result)
        (recur expression variable (next values) (conj result (list diff-eval expression variable (first values))))
      )
    )
  )
)

(defn -main
  [& args]
  (ns symbolic-differentiation.core)
  (def y 3)
  (def x 5)
  ;(macroexpand info)
  (info)
  (println (optimize-expression (nth-differentiation '(sin x) 'x 6)))
  (println (function-multiple-values-macro '(ln x) 'x (1 2 3 4)))
  (println (function-multiple-differentiation-values-macro '(ln x) 'x (1 2 3 4)))
  (println (optimize-expression (differentiation '(ln (sin (tg (* 3 x)))) 'x)))
  (println (eval (optimize-expression (differentiation '(ln (sin (tg (* 3 x)))) 'x))))
  (println (differentiation '(ln (tg x)) 'x))
  (println (eval (differentiation '(ln (tg x)) 'x)))
  (println (optimize-expression (differentiation '(ln (tg x)) 'x)))
  (println (eval (optimize-expression (differentiation '(ln (tg x)) 'x))))
  (println (function-multiple-differentiation-values '(ln (tg x)) 'x '(1 2 3 4)))
  (println (function-multiple-values '(* y (ln x)) 'x '(1 2 3 4)))
  (println (eval (differentiation '(sin (* y (tg x))) 'x)))
)