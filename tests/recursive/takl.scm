(define (run #!key (x (unknown 18)) (y (unknown 12)) (z (unknown 6)))
   
  (define (listn n)
    (if (GFX= n 0)
        '()
        (cons n (listn (GFX- n 1)))))
 
  (define (mas x y z)
    (if (not (shorterp y x))
        z
        (mas (mas (Scdr x) y z)
             (mas (Scdr y) z x)
             (mas (Scdr z) x y))))
 
  (define (shorterp x y)
    (and (not (null? y))
         (or (null? x)
             (shorterp (Scdr x)
                       (Scdr y)))))
 
  (let* ((lx (listn x))
         (ly (listn y))
         (lz (listn z)))
    (mas lx ly lz)))

(define (main . args)
  (run-benchmark
    "takl"
    takl-iters
    (lambda (result) (equal? result '(7 6 5 4 3 2 1)))
    (lambda (x y z) (lambda () (mas x y z)))
    l18
    l12
    l6))
