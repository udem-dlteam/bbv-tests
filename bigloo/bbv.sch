(directives
   (extern (include "./bbv_saw.h")
	   (macro bbv-saw-statistics::int () "bbv_saw_statistics")))

(define-macro (BBVop op x y)
   (cond
      ((flonum? x)
       (let ((b (gensym)))
	  `(let ((,b ,y))
	      (cond
		 ((flonum? ,b)
		  (,(symbol-append op 'fl) ,x ,b))
		 (else
		  (,op ,x ,b))))))
      ((flonum? y)
       (let ((a (gensym)))
	  `(let ((,a ,x))
	      (cond
		 ((flonum? ,a)
		  (,(symbol-append op 'fl) ,a ,y))
		 (else
		  (,op ,a ,y))))))
      (else
       (let ((a (gensym))
	     (b (gensym)))
	  `(let ((,a ,x)
		 (,b ,y))
	      (cond
		 ((and (fixnum? ,a) (fixnum? ,b))
		  (,(symbol-append op 'fx/ov) ,a ,b))
		 ((and (flonum? ,a) (flonum? ,b))
		  (,(symbol-append op 'fl) ,a ,b))
		 (else
		  (,op ,a ,b))))))))

(define-macro (BBVcmp op x y)
   (let ((a (gensym))
	 (b (gensym)))
      `(let ((,a ,x)
	     (,b ,y))
	  (cond
	     ((and (fixnum? ,a) (fixnum? ,b))
	      (,(symbol-append op 'fx) ,a ,b))
	     ((and (flonum? ,a) (flonum? ,b))
	      (,(symbol-append op 'fl) ,a ,b))
	     (else
	      (,op ,a ,b))))))

(define-macro (BBV+ x y) `(BBVop + ,x ,y))
(define-macro (BBV- x y) `(BBVop - ,x ,y))
(define-macro (BBV* x y) `(BBVop * ,x ,y))
(define-macro (BBV/ x y)
   (let ((a (gensym))
	 (b (gensym)))
      `(let ((,a ,x)
	     (,b ,y))
	  (cond
	     ((and (flonum? ,a) (flonum? ,b))
	      (/fl ,a ,b))
	     (else
	      (/ ,a ,b))))))

(define-macro (BBVremainder x y)
   (let ((a (gensym))
	 (b (gensym)))
      `(let ((,a ,x)
	     (,b ,y))
	  (cond
	     ((and (fixnum? ,a) (fixnum? ,b))
	      (remainderfx ,a ,b))
	     ((and (flonum? ,a) (flonum? ,b))
	      (remainderfl ,a ,b))
	     (else
	      (remainder ,a ,b))))))

(define-macro (BBV< x y) `(BBVcmp < ,x ,y))
(define-macro (BBV<= x y) `(BBVcmp <= ,x ,y))
(define-macro (BBV>= x y) `(BBVcmp >= ,x ,y))
(define-macro (BBV> x y) `(BBVcmp > ,x ,y))
(define-macro (BBV= x y) `(BBVcmp = ,x ,y))

(define-macro (BBVzero? x)
   (let ((a (gensym)))
      `(let ((,a ,x))
	  (cond
	     ((fixnum? ,a) (zerofx? ,a))
	     ((flonum? ,a) (zerofl? ,a))
	     (else (zero? ,a))))))

(define-macro (BBVsqrt x)
   (let ((a (gensym)))
      `(let ((,a ,x))
	  (cond
	     ((flonum? ,a) (sqrtfl ,a))
	     (else (sqrt ,a))))))

(define-macro (BBVatan-2 x y)
   (let ((a (gensym))
	 (b (gensym)))
      `(let ((,a ,x)
	     (,b ,y))
	  (cond
	     ((and (flonum? ,a) (flonum? ,b))
	      (atanfl ,a ,b))
	     (else
	      (atan ,a ,b))))))

(define-macro (BBVcos x)
   (let ((a (gensym)))
      `(let ((,a ,x))
	  (if (flonum? ,a)
	      (cosfl ,a)
	      (cos ,a)))))

(define-macro (BBVsin x)
   (let ((a (gensym)))
      `(let ((,a ,x))
	  (if (flonum? ,a)
	      (sinfl ,a)
	      (sin ,a)))))

(define-macro (FL+ x y) `(+fl ,x ,y))
(define-macro (FL- x y) `(-fl ,x ,y))
(define-macro (FL/ x y) `(/fl ,x ,y))
(define-macro (FL* x y) `(*fl ,x ,y))

(define-macro (FL< x y) `(<fl ,x ,y))
(define-macro (FL<= x y) `(<=fl ,x ,y))
(define-macro (FL>= x y) `(>=fl ,x ,y))
(define-macro (FL> x y) `(>fl ,x ,y))
(define-macro (FL= x y) `(=fl ,x ,y))

(define-macro (FLremainder x y) `(remainderfl ,x ,y))
(define-macro (FLsqrt x) `(sqrtfl ,x))
(define-macro (FLatan-2 x y) `(atanfl ,x ,y))

(define-macro (FX+ x y) `(+fx ,x ,y))
(define-macro (FX- x y) `(-fx ,x ,y))
(define-macro (FX/ x y) `(/fx ,x ,y))
(define-macro (FX* x y) `(*fx ,x ,y))

(define-macro (FX< x y) `(<fx ,x ,y))
(define-macro (FX<= x y) `(<=fx ,x ,y))
(define-macro (FX>= x y) `(>=fx ,x ,y))
(define-macro (FX> x y) `(>fx ,x ,y))
(define-macro (FX= x y) `(=fx ,x ,y))

(define (unknown val)
   ((car (list (lambda () val)))))

(register-exit-function! (lambda (status) (bbv-saw-statistics) status))
