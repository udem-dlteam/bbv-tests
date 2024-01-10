(define (run)
   
  (define (vref v i)
     (if (and (vector? v) (fixnum? i))
	 (let ((len (Svector-length v)))
	    (if (SFX< i len)
		(if (SFX>= i 0)
		    (if (SFX< i len)
			(Svector-ref v i)
			len)
		    100)
		200))
	 300))

  (let ((v '#(0 1 2 3 4)))
     (+ (vref v 0) (vref v 1) (vref v 2) (vref v (+ (Svector-length v) 10)))))

(define (check result)
  (equal? result 203))
