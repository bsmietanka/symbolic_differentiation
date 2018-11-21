(ns symbolic-differentiation.core
  (:gen-class)
) 

"Odwołania do matematycznych funkcji z biblioteki standardowej Clojure,
 liczba parametrów zależy od funkcji
 Przykła użycia: (log 2 4)"
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

"Funkcje sprawdzające czy odpowiadające wyrażenie jest prawidłowo sformułowane
 Przykład użycia: (pow 2 3) => true
                  (sin) => false - funkcja sin przyjmuje 1 argument"
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

"Główna funkcja wyznaczająca pochodną podanego wyrażenia po określonej zmiennej
 Przykład użycia: (differentiation '(ln (tg x)) 'x) 
                                => (* (* 1 (/ 1 (pow (cos x) 2))) (/ 1 (tg x)))
 Argumenty: expression - wyrażenie, z którego liczymy pochodną,
            variable - nazwa zmiennej, po której liczymy pochodną"
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

"Funkcja liczy wartość wyrażenia dla zadanej wartości zmiennej.
 Oryginalne wartości zmiennych pozostają bez zmian
 Przykład użycia: (diff-eval expression variable (first values))
 Argumenty: expression - wyrażenie, z którego liczymy pochodną,
            variable - nazwa zmiennej, po której liczymy pochodną
            argument-values - zadane wartości zmiennej"
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
        (do ;else
          (eval(read-string (clojure.string/join " " ["(" "def" variable argument-value ")"])))
          (eval(read-string (pr-str diff-string)))
        )
      )
    ) 
  )
)

"Funkcja liczy pochodną podanej w parametrze funkcji i oblicza wartości pochodnej w zadanych punktach
 Przykład użycia: (function-multiple-differentiation-values '(ln (tg x)) 'x '(1 2 3 4))
 Argumenty: expression - wyrażenie, z którego liczymy pochodną,
            variable - nazwa zmiennej, po której liczymy pochodną
            argument-values - zadane wartości zmiennej"
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

"Funkcja oblicza wartości funkcji w zadanych punktach
 Przykład użycia: (function-multiple-values '(* y (ln x)) 'x '(1 2 3 4))
 Argumenty: expression - wyrażenie, z którego liczymy pochodną,
            variable - nazwa zmiennej, po której liczymy pochodną
            argument-values - zadane wartości zmiennej"
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

"Funkcja usuwająca elementy neutralne dla danego wyrażenia np. (+ 0 x) lub (* 1 x).
 Służy do wyznaczenia uproszczonej postaci pochodnej funkcji.
 Wykorzystywana wewnętrznie przez funkcję optimize-expression"
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
            (and (= (first expr) '*) (= (second (next expr)) 0))
              (optimize-expression_priv '(0))
            (and (= (first expr) '/) (= (second expr) 0))
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

"Funkcja usuwająca z listy element o indeksie podanym w parametrze funkcji.
 Część funkcji służącej do uproszczenia otrzymanych pochodnych, do użytku wewnętrzengo"
(defn remove_index
  [vec index]
  (let [coll vec
    i index]
    (into [] (concat (subvec coll 0 i)
        (subvec coll (inc i)))))
)

"Funkcja usuwająca zbędne nawiasy z wyrażenia.
 Funkcja do użytku wewnętrznego"
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

"Funkcja rekurencyjnie usuwająca niepotrzebne nawiasy z wyrażenia.
 Funkcja do użytku wewnętrznego"
(defn remove_doubled_brackets
  [str]
  (loop [orig str res (remove_doubled_brackets_priv orig)]
    (if (= orig res)
      res
      (recur res (remove_doubled_brackets_priv res))
    )
  )
)

"Główna funkcja służąca do uproszczenia wzoru otrzymanej pochodnej.
 Wykorzystuje powyższe funkcje: optimize-expression_priv, remove_doubled_brackets
 Przykład użycia: (optimize-expression (differentiation '(ln (tg x)) 'x))
 Argumenty: expression - wyrażenie, z którego liczymy pochodną"
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

"Funkcja rekurencyjnie licząca n-tą pochodną. 
 Po każdej operacji różniczkowania próbuje uprości wyrażenie.
 Przykład użycia: (nth-differentiation '(sin (tg (ln (sqrt 2 x)))) 'x 2)
 Argumenty: expression - wyrażenie, z którego liczymy pochodną,
            variable - nazwa zmiennej, po której liczymy pochodną
            degree - stopień wyliczonej pochodnej"
(defn nth-differentiation
  [expression variable degree]
  (loop [expression expression variable variable degree degree]
    (if (< degree 1)
      (optimize-expression expression)
      (recur (optimize-expression(differentiation expression variable)) variable (- degree 1))))
)

"Funkcja liczy n-tą pochodną podanej w parametrze funkcji i oblicza wartości pochodnej w zadanych punktach
 Przykład użycia: (function-multiple-nth-differentiation-values '(sin (tg (ln (sqrt 2 x)))) 'x (1 2 3) 2)
 Argumenty: expression - wyrażenie, z którego liczymy pochodną,
            variable - nazwa zmiennej, po której liczymy pochodną
            argument-values - zadane wartości zmiennej
            degree - stopień wyliczonej pochodnej"
(defn function-multiple-nth-differentiation-values
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

"Makro rekurencyjne drukujące informacje o użyciu zdefiniowanych w programi funkcji"
(defmacro info []
  (loop [
    func-names ["(differentiation function x) - function - funkcja, x - zmienna po ktorej funkcja bedzie rozniczkowana" 
      "(optimize-expression function) - funkcja optymalizuje wyrazenie - odpowiednio zamienia mnozenie przez 1 i przez 0" 
      "(function-multiple-differentiation-values function x values) - funkcja oblicza pochodna danej funkcji, a nastepnie oblicza wartosci obliczonej pochodnej w zadanych punktach"
      "(function-multiple-values function x values) - funkcja oblicza wartosci funkcji w zadanych punktach"
      "(function-multiple-values-macro function x values) - makro oblicza wartosci funkcji w zadanych punktach"
      "(nth-differentiation function x degree) - function - funkcja, x - zmienna po ktorej funkcja bedzie rozniczkowana, degree - stopien pochodnej" 
      "(function-multiple-nth-differentiation-values function x values degree) - funkcja oblicza n-ta pochodna danej funckji, a nastepnie oblicza wartosci obliczonej pochodnej w zadanych punktach" 
      "(function-multiple-differentiation-values-macro function x values) - makro oblicza pochodna danej funkcji, a nastepnie oblicza wartosci obliczonej pochodnej w zadanych punktach"
      ] 
    result ['do]]
    (if (= 0 (count func-names))
      (eval (apply list result))
      (recur (next func-names) (conj (conj result (list 'println (first func-names))) (list 'println "")))
    )
  )  
)

"Makro rekurencyjne wyznaczające wartość funkcji dla wszystkich zadanych wartości zmiennej
 Przykład użycia: (function-multiple-values-macro '(ln x) 'x (1 2 3 4))
 Argumenty: expression - wyrażenie do ewaluacji,
            variable - zmienna, pod którą będziemy podstawiać wartości,
            values - lista wartości"
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

"Makro rekurencyjne wyliczające pochodną funkcji oraz wyznaczające wartości pochodnej dla kilku zadanych wartości
 Przykład użycia: (function-multiple-differentiation-values-macro '(ln x) 'x (1 2 3 4))
 Argumenty: expression - wyrażenie do ewaluacji,
            variable - zmienna, pod którą będziemy podstawiać wartości,
            values - lista wartości"
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
  (println "Informacje o użytych funkcjach:")
  (info)
  (println "2-ga pochodna '(sin (tg (ln (sqrt 2 x)))):")
  (println (nth-differentiation '(sin (tg (ln (sqrt 2 x)))) 'x 2))
  (println "Wartości funkcji '(ln x) dla x równego kolejno (1 2 3 4):")
  (println (function-multiple-values-macro '(ln x) 'x (1 2 3 4)))
  (println "Wartości pochodnej funkcji '(ln x) dla x równego kolejno (1 2 3 4):")
  (println (function-multiple-differentiation-values-macro '(ln x) 'x (1 2 3 4)))
  (println "Pochodna wyrażenia '(ln (sin (tg (* 3 x)))) po uproszczeniu:")
  (println (optimize-expression (differentiation '(ln (sin (tg (* 3 x)))) 'x)))
  (println "Wartość '(ln (sin (tg (* 3 x)))) dla x = 5 (zdefiniowany powyżej):")
  (println (eval (optimize-expression (differentiation '(ln (sin (tg (* 3 x)))) 'x))))
  (println "Pochodna '(ln (tg x)):")
  (println (differentiation '(ln (tg x)) 'x))
  (println "Wartość pochodnej z  '(ln (tg x)) dla x = 5:")
  (println (eval (differentiation '(ln (tg x)) 'x)))
  (println "Pochodna '(ln (tg x)) po uproszczeniu:")
  (println (optimize-expression (differentiation '(ln (tg x)) 'x)))
  (println "Wartość pochodnej z '(ln (tg x)) dla x = 5:")
  (println (eval (optimize-expression (differentiation '(ln (tg x)) 'x))))
  (println "Wartości pochodnej z '(ln (tg x)) dla x równych kolejno (1 2 3 4):")
  (println (function-multiple-differentiation-values '(ln (tg x)) 'x '(1 2 3 4)))
  (println "Wartości funkcji '(* y (ln x)) dla x = (1 2 3 4) oraz y = 3 (zdefiniowany powyżej):")
  (println (function-multiple-values '(* y (ln x)) 'x '(1 2 3 4)))
  (println "Wartość pochodnej '(sin (* y (tg x))) dla x = 5:")
  (println (eval (differentiation '(sin (* y (tg x))) 'x)))
)