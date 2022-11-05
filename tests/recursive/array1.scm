(define (run #!key (n (unknown 200000 2000)))

   (define (create-x n)
      (define result (Smake-vector1 n))
      (do ((i 0 (SFX+ i 1)))
          ((SFX>= i n) result)
          (Svector-set! result i i)))

  (define (create-y x)
    (let* ((n (Svector-length x))
           (result (Smake-vector1 n)))
      (do ((i (SFX- n 1) (SFX- i 1)))
          ((SFX< i 0) result)
        (Svector-set! result i (Svector-ref x i)))))

  (define (my-try n)
    (Svector-length (create-y (create-x n))))

  (define (go n)
    (let loop ((repeat 100)
               (result '()))
      (if (SFX> repeat 0)
          (loop (SFX- repeat 1) (my-try n))
          result)))

  (go n))

(define (check result)
  (equal? result 200000))
