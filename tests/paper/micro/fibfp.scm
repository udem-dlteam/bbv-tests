(define-keys (run !key (n (unknown 39.0 10.0)))

  (define (fibfp n)
    (if (SFL< n 2.0)
        n
        (SFL+ (fibfp (SFL- n 1.0))
              (fibfp (SFL- n 2.0)))))

  (fibfp n))

(define (check result)
  (equal? result (unknown 63245986.0 55.0)))
