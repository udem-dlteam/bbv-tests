(define (run #!key (n (unknown 39)))
   
   (define (fib n)
      (if (GFX< n 2)
          n
          (GFX+ (fib (GFX- n 1))
                (fib (GFX- n 2)))))
   
   (fib n))
