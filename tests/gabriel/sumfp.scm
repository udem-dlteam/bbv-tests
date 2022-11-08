;;; SUMFP -- Compute sum of integers from 0 to n using floating point

;;(import (scheme base) (scheme read) (scheme write) (scheme time))

(define (_run n)
  (let loop ((i n) (sum 0.0))
    (if (SFL< i 0.0)
        sum
        (loop (SFL- i 1.0) (SFL+ i sum)))))

(define (run #!key (n (unknown 1e6 1e4)))
  (_run n))

(define (check result)
  (equal? result 5.000005e11))
