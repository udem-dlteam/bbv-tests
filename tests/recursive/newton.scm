(define (run #!key (n (unknown 10000.0)))

  (define (newton n)
    (let loop ((a n))
      (let ((ap (SFL/ (SFL+ a (SFL/ n a)) 2.)))
        (if (SFL= ap a)
            a
            (loop ap)))))

  (newton n))

(define (check result)
  (equal? result 100.0))
