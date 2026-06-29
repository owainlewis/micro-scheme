(import (scheme base))

(define values (vector 1 2 3))
(vector-set! values 1 42)
(vector-ref values 1)
