(define (run #!key (n (unknown 39 20)))
   
  (define (vref v i)
     (if (and (vector? v) (fixnum? i))
	 (let ((len (vector-length v)))
	    (if (<fx i len)
		(if (>=fx i 0)
		    (if (<fx i len)
			(vector-ref v i)
			len)
		    100)
		200))
	 300))

  (let ((v '#(0 1 2 3 4)))
     (+ (vref v 0) (vref v 1) (vref v 2) (vref v (+ (vector-length v) 10)))))

(define (check result)
  (equal? result 203))
