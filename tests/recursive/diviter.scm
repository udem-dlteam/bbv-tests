(define (create-n n)
  (do ((n n (SFX- n 1))
       (a '() (cons '() a)))
      ((SFX= n 0) a)))

(define *ll* (create-n 200))
(define expect (create-n 100))

(define (run)

  (define (iterative-div2 l)
    (do ((l l (Scddr l))
         (a '() (cons (Scar l) a)))
        ((null? l) a)))

  (iterative-div2 *ll*))

(define (check result)
  (equal? result expect))
