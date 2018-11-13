(ns functions.polynomial 
    (:gen-class) 
    ;(:require [clojure.string :as str])
    (:require [utils.math :as cm]) ; cm as Custom Math
    )

(defn Diff
    "Function calculates simple polynomial derivative"
    [chars]
    (let [pow (read-string (subs chars 2))]
        ;(read-string (str "(* " "(power x " (- pow 1) ") " pow ")"))
        ;(println (cm/power 2 3))
        ;(println (load-string (str "(* " "(cm/power x " (- pow 1) ") " pow ")")))
        ;(str "(* " "(cm/power x " (- pow 1) ") " pow ")")
        ;(println (eval `(* '~pow (cm/power x (- '~pow 1)))))
        `(* ~pow (cm/power (read-string (subs ~chars 0 1)) (- ~pow 1)))
    )
    
)

(defn IsFunctionOk
    "Function checks if string is valid to parse"
    [str]
    (def re (re-pattern "^[a-z][\\^][-]?[1-9]+[0-9]*$"))
    (if (re-find (re-matcher re str)) 
        true
        false
    )
)