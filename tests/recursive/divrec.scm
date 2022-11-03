(define (create-n n)
  (do ((n n (SFX- n 1))
       (a '() (cons '() a)))
      ((SFX= n 0) a)))

(define *ll* (create-n 200))
(define expect (create-n 100))

(define (run)

  (define (recursive-div2 l)
    (cond ((null? l) '())
          (else (cons (Scar l) (recursive-div2 (Scddr l))))))

  (recursive-div2 *ll*))

(define (check result)
  (equal? result expect))
