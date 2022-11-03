(define (create-n n)
  (do ((n n (GFX- n 1))
       (a '() (cons '() a)))
      ((GFX= n 0) a)))

(define *ll* (create-n 2000))

(define (run)

  (define (recursive-div2 l)
    (cond ((null? l) '())
          (else (cons (Scar l) (recursive-div2 (Scddr l))))))

  (recursive-div2 *ll*))
