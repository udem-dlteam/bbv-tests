(define (run #!key (x (unknown 18)) (y (unknown 12)) (z (unknown 6)))
   
  (define (listn n)
    (if (SFX= n 0)
        '()
        (cons n (listn (SFX- n 1)))))
 
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

(define (check result)
  (equal? result '(7 6 5 4 3 2 1)))
