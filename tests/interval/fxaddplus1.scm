(define (program x)
  (if (and (fixnum? x)
           (fx< x 0))
      (let ((r (fx+ x 1)))
        (vector (cons #t r) x r))))

;; .gvm contains: cons #t .*x|>=\.\.-1 .*r|>\.\.0 
