(define (program x)
  (if (fixnum? x)
      (if (fx< x 0)
          (vector #t (cons #t x) x)
          (vector #f (cons #f x) x))))

;; .gvm contains: cons #t .*=x|>=\.\.-1 
;; .gvm contains: cons #f .*=x|0\.\.<= 
