(define (run #!key (arg (unknown 39)))
   
   (define (fib x)
      (if (GFX< x 2)
          x
          (GFX+ (fib (GFX- x 1))
                (fib (GFX- x 2)))))
   
   (fib arg))
