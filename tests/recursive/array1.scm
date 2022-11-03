(define (run #!key (n (unknown 200000)))

  (define (create-x n)
    (define result (Smake-vector1 n))
    (do ((i 0 (GFX+ i 1)))
        ((GFX>= i n) result)
      (Svector-set! result i i)))

  (define (create-y x)
    (let* ((n (Svector-length x))
           (result (Smake-vector1 n)))
      (do ((i (GFX- n 1) (GFX- i 1)))
          ((GFX< i 0) result)
        (Svector-set! result i (Svector-ref x i)))))

  (define (my-try n)
    (Svector-length (create-y (create-x n))))

  (define (go n)
    (let loop ((repeat 100)
               (result '()))
      (if (GFX> repeat 0)
          (loop (GFX- repeat 1) (my-try n))
          result)))

  (go n))
