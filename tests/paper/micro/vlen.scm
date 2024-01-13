(define (run #!key (n (unknown 39 20)))

   (define (vloop v)
      (when (vector? v)
         (let ((L (SFX- (vector-length v) 1)))
            (let loop ((j L))
               (if (SFX>= j 0)
                  (loop (SFX- j 1))
                  (SFX+ j L))))))
  (let ((v '#(0 1 2 3 4)))
     (vloop v)))

(define (check result)
  (equal? result 3))
