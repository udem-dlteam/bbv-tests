(define-keys (run !key (n (unknown 39 10)))
   
  (define (fib n)
    (if (SFX< n 2)
        n
        (SFX+ (fib (SFX- n 1))
              (fib (SFX- n 2)))))
   
  (fib n))

(define (check result)
  (equal? result (unknown 63245986 55)))
