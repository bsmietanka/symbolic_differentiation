(ns utils.math 
    (:gen-class))

(defn _power 
    "Recursive exp"
    [number exp]
    (loop [acc 1 exp exp]
        (if (zero? exp) 
            acc
            (recur (* number acc) (dec exp))
        )
    )
)

(defn power
    "Function raises number to specific exp"
    [number exp]
    (if (< exp 0) 
        (do 
            (def number (/ 1.0 number))
            (def exp (* -1 exp))
        )
    )
    (_power number exp)
)