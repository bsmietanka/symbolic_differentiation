(ns symbolic-differentiation.core
  (:gen-class)
  (:require [functions.polynomial :as poly])
  (:require [utils.math :as cm]) ; cm as Custom Math
) 

(defn -main
  [& args]
  (def x 3)
  (println (+ 2 3))
  (println "Hello, World!")
  (println (* 7 3))
  (println "Test x^3. Is ok?")
  (println (poly/IsFunctionOk "x^3"))
  (println "Test x^3. Derivative is equal to:")
  (println (poly/Diff "x^3"))
  ;(println (eval (poly/Diff "x^3")))
)