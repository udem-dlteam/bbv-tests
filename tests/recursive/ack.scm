(define (run #!key (m (unknown 3)) (n (unknown 9)))
   
  (define (ack m n)
    (cond ((GFX= m 0) (GFX+ n 1))
          ((GFX= n 0) (ack (GFX- m 1) 1))
          (else (ack (GFX- m 1) (ack m (GFX- n 1))))))

  (ack m n))
