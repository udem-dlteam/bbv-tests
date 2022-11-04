(define (run #!key (n (unknown 39.)))

  (define (fibfp n)
    (if (GEN< n 2.)
      n
      (GEN+ (fibfp (GEN- n 1.))
            (fibfp (GEN- n 2.)))))

  (fibfp n))

(define (check result)
  (equal? result 63245986.))
