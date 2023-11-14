(define (run #!key (n (unknown 4)))

   (define (rev v)
      (let loop ((i 0) (j (SFX- (vector-length v) 1)))
	 (if (SFX< i j)
	     (let ((t (vector-ref v i)))
		(vector-set! v i (vector-ref v j))
		(vector-set! v j t)
		(loop (SFX+ i 1) (SFX- j 1))))))

   (rev (apply vector (iota n))))

(define (check result)
  (equal? result '#(4 3 2 1 0)))
