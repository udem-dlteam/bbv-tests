(define (create-n n)
  (do ((n n (GFX- n 1))
       (a '() (cons '() a)))
      ((GFX= n 0) a)))

(define *ll* (create-n 2000))

(define (run)

  (define (iterative-div2 l)
    (do ((l l (Scddr l))
         (a '() (cons (Scar l) a)))
        ((null? l) a)))

  (iterative-div2 *ll*))
