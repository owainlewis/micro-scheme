(import (scheme base))

(define (fact n acc)
  (if (= n 0)
      acc
      (fact (- n 1) (* acc n))))

(fact 6 1)
