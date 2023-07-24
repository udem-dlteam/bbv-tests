;;; SUM -- Compute sum of integers from 0 to 10000

;;(import (scheme base) (scheme read) (scheme write) (scheme time))

(define (xrun n)
  (let loop ((i n) (sum 0))
    (if (SFX< i 0)
        sum
        (loop (SFX- i 1) (SFX+ i sum)))))

(define (run #!key (n (unknown 10000)))
  (xrun n))

(define (check result)
  (equal? result 50005000))
