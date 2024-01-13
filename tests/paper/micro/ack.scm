(define (run #!key (m (unknown 3 3)) (n (unknown 9 6)))
   
  (define (ack m n)
    (cond ((SFX= m 0) (SFX+ n 1))
          ((SFX= n 0) (ack (SFX- m 1) 1))
          (else (ack (SFX- m 1) (ack m (SFX- n 1))))))

  (ack m n))

(define (check result)
  (equal? result (unknown 4093 509)))
