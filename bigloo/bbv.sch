(directives
   (extern (include "./bbv_saw.h")
           (macro bbv-saw-statistics::int () "bbv_saw_statistics")))

(define-macro (FLop op . args)
   (case op
      ((atan2) `(atan-2fl ,@args))
      ((zero?) `(zerofl? ,@args))
      (else `(,(symbol-append op 'fl) ,@args))))
(define-macro (FXop op . args)
   (case op
      ((zero?) `(zerofx? ,@args))
      (else `(,(symbol-append op 'fx) ,@args))))

(define-macro (PRIMop op . args)
   (case op
      ((vector-set!) `(vector-set-ur! ,@args))
      ((vector-ref) `(vector-ref-ur ,@args))
      ((atan2) `(atan-2fl ,@args))
      (else `(,op ,@args))))

(define (unknown x . rest) ((car (list (lambda () x)))))

(define-macro (MAPop kind op . args)
  (define arithmetic
     (cond-expand
	(arithmeticG 'G)
	(arithmeticS 'S)
	(else 'G)))
  (cond
   ((eq? kind 'FL)
    `(PRIMop ,(symbol-append 'fl op) ,@args))
   ((eq? kind 'FX)
    `(PRIMop ,(symbol-append 'fx op) ,@args))
   ((or (eq? kind 'GEN) (eq? arithmetic 'G)) ;; force generic?
    `(,(symbol-append 'BBV op) ,@args))
   ((eq? kind 'SFL)
    `(,(symbol-append 'FL op) ,@args))
   ((eq? kind 'SFX)
    `(,(symbol-append 'FX op) ,@args))))

(define-macro (GEN+ x y)         `(MAPop GEN + ,x ,y))
(define-macro (GEN- x y)         `(MAPop GEN - ,x ,y))
(define-macro (GEN* x y)         `(MAPop GEN * ,x ,y))
(define-macro (GEN/ x y)         `(MAPop GEN / ,x ,y))
(define-macro (GENquotient x y)  `(MAPop GEN quotient ,x ,y))
(define-macro (GENremainder x y) `(MAPop GEN remainder ,x ,y))
(define-macro (GENmodulo x y)    `(MAPop GEN modulo ,x ,y))
(define-macro (GEN= x y)         `(MAPop GEN = ,x ,y))
(define-macro (GEN< x y)         `(MAPop GEN < ,x ,y))
(define-macro (GEN> x y)         `(MAPop GEN > ,x ,y))
(define-macro (GEN<= x y)        `(MAPop GEN <=,x ,y))
(define-macro (GEN>= x y)        `(MAPop GEN >= ,x ,y))
(define-macro (GENzero? x)       `(MAPop GEN zero? ,x))
(define-macro (GENsqrt x)        `(MAPop GEN sqrt ,x))
(define-macro (GENsin x)         `(MAPop GEN sin ,x))
(define-macro (GENcos x)         `(MAPop GEN cos ,x))
(define-macro (GENatan2 x y)     `(MAPop GEN atan2 ,x ,y))
(define-macro (GENatan x)        `(MAPop GEN atan ,x))

(define-macro (SFL+ x y)         `(MAPop SFL + ,x ,y))
(define-macro (SFL- x y)         `(MAPop SFL - ,x ,y))
(define-macro (SFL* x y)         `(MAPop SFL * ,x ,y))
(define-macro (SFL/ x y)         `(MAPop SFL / ,x ,y))
(define-macro (SFLquotient x y)  `(MAPop SFL quotient ,x ,y))
(define-macro (SFLremainder x y) `(MAPop SFL remainder ,x ,y))
(define-macro (SFLmodulo x y)    `(MAPop SFL modulo ,x ,y))
(define-macro (SFL= x y)         `(MAPop SFL = ,x ,y))
(define-macro (SFL< x y)         `(MAPop SFL < ,x ,y))
(define-macro (SFL> x y)         `(MAPop SFL > ,x ,y))
(define-macro (SFL<= x y)        `(MAPop SFL <= ,x ,y))
(define-macro (SFL>= x y)        `(MAPop SFL >= ,x ,y))
(define-macro (SFLzero? x)       `(MAPop SFL zero? ,x))
(define-macro (SFLsqrt x)        `(MAPop SFL sqrt ,x))
(define-macro (SFLsin x)         `(MAPop SFL sin ,x))
(define-macro (SFLcos x)         `(MAPop SFL cos ,x))
(define-macro (SFLatan2 x y)     `(MAPop SFL atan2 ,x ,y))
(define-macro (SFLatan x)        `(MAPop SFL atan ,x))

(define-macro (SFX+ x y)         `(MAPop SFX + ,x ,y))
(define-macro (SFX2- x y)        `(MAPop SFX - ,x ,y))
(define-macro (SFX- x . rest)    (if (null? rest) `(MAPop SFX - 0 ,x) `(SFX2- ,x ,@rest)))
(define-macro (SFX* x y)         `(MAPop SFX * ,x ,y))
(define-macro (SFXquotient x y)  `(MAPop SFX quotient ,x ,y))
(define-macro (SFXremainder x y) `(MAPop SFX remainder ,x ,y))
(define-macro (SFXmodulo x y)    `(MAPop SFX modulo ,x ,y))
(define-macro (SFX= x y)         `(MAPop SFX = ,x ,y))
(define-macro (SFX< x y)         `(MAPop SFX < ,x ,y))
(define-macro (SFX> x y)         `(MAPop SFX > ,x ,y))
(define-macro (SFX<= x y)        `(MAPop SFX <= ,x ,y))
(define-macro (SFX>= x y)        `(MAPop SFX >= ,x ,y))
(define-macro (SFXzero? x)       `(MAPop SFX zero? ,x))
(define-macro (SFXodd? x)        `(MAPop SFX odd? ,x))
(define-macro (SFXeven? x)       `(MAPop SFX even? ,x))

(define-macro (FL+ x y)         `(FLop + ,x ,y))
(define-macro (FL- x y)         `(FLop - ,x ,y))
(define-macro (FL* x y)         `(FLop * ,x ,y))
(define-macro (FL/ x y)         `(FLop / ,x ,y))
(define-macro (FLquotient x y)  `(FLop quotient ,x ,y))
(define-macro (FLremainder x y) `(FLop remainder ,x ,y))
(define-macro (FLmodulo x y)    `(FLop modulo ,x ,y))
(define-macro (FL= x y)         `(FLop = ,x ,y))
(define-macro (FL< x y)         `(FLop < ,x ,y))
(define-macro (FL> x y)         `(FLop > ,x ,y))
(define-macro (FL<= x y)        `(FLop <= ,x ,y))
(define-macro (FL>= x y)        `(FLop >= ,x ,y))
(define-macro (FLzero? x)       `(FLop zero? ,x))
(define-macro (FLsqrt x)        `(FLop sqrt ,x))
(define-macro (FLsin x)         `(FLop sin ,x))
(define-macro (FLcos x)         `(FLop cos ,x))
(define-macro (FLatan2 x y)     `(FLop atan2 ,x ,y))
(define-macro (FLatan x)        `(FLop atan ,x))

(define-macro (FX+ x y)         `(FXop + ,x ,y))
(define-macro (FX- x y)         `(FXop - ,x ,y))
(define-macro (FX* x y)         `(FXop * ,x ,y))
(define-macro (FXquotient x y)  `(FXop quotient ,x ,y))
(define-macro (FXremainder x y) `(FXop remainder ,x ,y))
(define-macro (FXmodulo x y)    `(FXop modulo ,x ,y))
(define-macro (FX= x y)         `(FXop = ,x ,y))
(define-macro (FX< x y)         `(FXop < ,x ,y))
(define-macro (FX> x y)         `(FXop > ,x ,y))
(define-macro (FX<= x y)        `(FXop <=,x ,y))
(define-macro (FX>= x y)        `(FXop >= ,x ,y))
(define-macro (FXzero? x)       `(FXop zero? ,x))
(define-macro (FXodd? x)        `(FXop odd? ,x))
(define-macro (FXeven? x)       `(FXop even? ,x))

(define-macro (FLONUM? x) `(PRIMop flonum? ,x))
(define-macro (FIXNUM? x) `(PRIMop fixnum? ,x))

(define-macro (BBVop op x y)
  (cond
   ((flonum? x)
    (let ((b (gensym)))
      `(let ((,b ,y))
         (cond
          ((FLONUM? ,b)
           (FLop ,op ,x ,b))
          (else
           (PRIMop ,op ,x ,b))))))
   ((flonum? y)
    (let ((a (gensym)))
      `(let ((,a ,x))
         (cond
          ((FLONUM? ,a)
           (FLop ,op ,a ,y))
          (else
           (PRIMop ,op ,a ,y))))))
   (else
    (let ((a (gensym))
          (b (gensym)))
      `(let ((,a ,x)
             (,b ,y))
         (cond
          ((and (FIXNUM? ,a) (FIXNUM? ,b))
           (,(if (eq? op '/) '/fx (symbol-append op 'fx/ov)) ,a ,b))
          ((and (FLONUM? ,a) (FLONUM? ,b))
           (FLop ,op ,a ,b))
          (else
           (PRIMop ,op ,a ,b))))))))

(define-macro (BBVcmp op x y)
  (let ((a (gensym))
        (b (gensym)))
    `(let ((,a ,x)
           (,b ,y))
       (cond
        ((and (FIXNUM? ,a) (FIXNUM? ,b))
         (FXop ,op ,a ,b))
        ((and (FLONUM? ,a) (FLONUM? ,b))
         (FLop ,op ,a ,b))
        (else
         (PRIMop ,op ,a ,b))))))

(define-macro (BBV+ x y) `(BBVop + ,x ,y))
(define-macro (BBV- x y) `(BBVop - ,x ,y))
(define-macro (BBV* x y) `(BBVop * ,x ,y))

(define-macro (BBV/ x y)
  (let ((a (gensym))
        (b (gensym)))
    `(let ((,a ,x)
           (,b ,y))
       (cond
        ((and (FLONUM? ,a) (FLONUM? ,b))
         (FL/ ,a ,b))
        (else
         (PRIMop / ,a ,b))))))

(define-macro (BBVquotient x y)
  (let ((a (gensym))
        (b (gensym)))
    `(let ((,a ,x)
           (,b ,y))
       (cond
        ((and (FIXNUM? ,a) (FIXNUM? ,b))
         (FXquotient ,a ,b)) ;; not correct when b = -1 or 0
        ((and (FLONUM? ,a) (FLONUM? ,b))
         (FLquotient ,a ,b))
        (else
         (PRIMop quotient ,a ,b))))))

(define-macro (BBVremainder x y)
  (let ((a (gensym))
        (b (gensym)))
    `(let ((,a ,x)
           (,b ,y))
       (cond
        ((and (FIXNUM? ,a) (FIXNUM? ,b))
         (FXremainder ,a ,b))
        ((and (FLONUM? ,a) (FLONUM? ,b))
         (FLremainder ,a ,b))
        (else
         (PRIMop remainder ,a ,b))))))

(define-macro (BBVmodulo x y)
  (let ((a (gensym))
        (b (gensym)))
    `(let ((,a ,x)
           (,b ,y))
       (cond
        ((and (FIXNUM? ,a) (FIXNUM? ,b))
         (FXmodulo ,a ,b))
        ((and (FLONUM? ,a) (FLONUM? ,b))
         (FLmodulo ,a ,b))
        (else
         (PRIMop modulo ,a ,b))))))

(define-macro (BBV= x y) `(BBVcmp = ,x ,y))
(define-macro (BBV< x y) `(BBVcmp < ,x ,y))
(define-macro (BBV<= x y) `(BBVcmp <= ,x ,y))
(define-macro (BBV>= x y) `(BBVcmp >= ,x ,y))
(define-macro (BBV> x y) `(BBVcmp > ,x ,y))

(define-macro (BBVzero? x)
  (let ((a (gensym)))
    `(let ((,a ,x))
       (cond
        ((FIXNUM? ,a) (FXzero? ,a))
        ((FLONUM? ,a) (FLzero? ,a))
        (else (PRIMop zero? ,a))))))

(define-macro (BBVodd? x)
  (let ((a (gensym)))
    `(let ((,a ,x))
       (cond
        ((FIXNUM? ,a) (FXodd? ,a))
        (else (PRIMop odd? ,a))))))

(define-macro (BBVeven? x)
  (let ((a (gensym)))
    `(let ((,a ,x))
       (cond
        ((FIXNUM? ,a) (FXeven? ,a))
        (else (PRIMop even? ,a))))))

(define-macro (BBVsqrt x)
  (let ((a (gensym)))
    `(let ((,a ,x))
       (cond
        ((FLONUM? ,a) (FLsqrt ,a))
        (else (PRIMop sqrt ,a))))))

(define-macro (BBVcos x)
  (let ((a (gensym)))
    `(let ((,a ,x))
       (if (FLONUM? ,a)
           (FLcos ,a)
           (PRIMop cos ,a)))))

(define-macro (BBVsin x)
  (let ((a (gensym)))
    `(let ((,a ,x))
       (if (FLONUM? ,a)
           (FLsin ,a)
           (PRIMop sin ,a)))))

(define-macro (BBVatan2 x y)
  (let ((a (gensym))
        (b (gensym)))
    `(let ((,a ,x)
           (,b ,y))
       (cond
        ((and (FLONUM? ,a) (FLONUM? ,b))
         (FLatan2 ,a ,b))
        (else
         (PRIMop atan2 ,a ,b))))))

(define-macro (BBVatan x)
  (let ((a (gensym)))
    `(let ((,a ,x))
       (cond
        ((FLONUM? ,a)
         (FLatan ,a))
        (else
         (PRIMop atan ,a))))))

(define-macro (Scar x)
  (define arithmetic
    (cond-expand
      (arithmeticG 'G)
      (arithmeticS 'S)
      (else 'G)))
  (let ((a (gensym)))
    `(let ((,a ,x))
       ,(if (eq? arithmetic 'S)
            `(PRIMop car ,a)
            `(if (pair? ,a)
                 (PRIMop car ,a)
                 (DEAD-END "car type error"))))))

(define-macro (Scdr x)
  (define arithmetic
    (cond-expand
      (arithmeticG 'G)
      (arithmeticS 'S)
      (else 'G)))
  (let ((a (gensym)))
    `(let ((,a ,x))
       ,(if (eq? arithmetic 'S)
            `(PRIMop cdr ,a)
            `(if (pair? ,a)
                 (PRIMop cdr ,a)
                 (DEAD-END "cdr type error"))))))

(define-macro (Sset-car! x y)
  (define arithmetic
    (cond-expand
      (arithmeticG 'G)
      (arithmeticS 'S)
      (else 'G)))
  (let ((a (gensym))
        (b (gensym)))
    `(let ((,a ,x)
           (,b ,y))
       ,(if (eq? arithmetic 'S)
            `(PRIMop set-car! ,a ,b)
            `(if (pair? ,a)
                 (PRIMop set-car! ,a ,b)
                 (DEAD-END "set-car! type error"))))))

(define-macro (Sset-cdr! x y)
  (define arithmetic
    (cond-expand
      (arithmeticG 'G)
      (arithmeticS 'S)
      (else 'G)))
  (let ((a (gensym))
        (b (gensym)))
    `(let ((,a ,x)
           (,b ,y))
       ,(if (eq? arithmetic 'S)
            `(PRIMop set-cdr! ,a ,b)
            `(if (pair? ,a)
                 (PRIMop set-cdr! ,a ,b)
                 (DEAD-END "set-cdr! type error"))))))

(define-macro (Scaar x) `(Scar (Scar ,x)))
(define-macro (Scadr x) `(Scar (Scdr ,x)))
(define-macro (Scdar x) `(Scdr (Scar ,x)))
(define-macro (Scddr x) `(Scdr (Scdr ,x)))
(define-macro (Scdadr x) `(Scdr (Scar (Scdr ,x))))
(define-macro (Scaddr x) `(Scar (Scdr (Scdr ,x))))
(define-macro (Scdddr x) `(Scdr (Scdr (Scdr ,x))))
(define-macro (Scadddr x) `(Scar (Scdr (Scdr (Scdr ,x)))))

(define-macro (Sstring->symbol x) `(string->symbol ,x))
(define-macro (Ssymbol->string x) `(symbol->string ,x))
(define-macro (Sstring-append x . y) `(string-append ,x ,@y))
(define-macro (Slist->string x) `(list->string ,x))
(define-macro (Sstring->list x) `(string->list ,x))
(define-macro (SFXnumber->string x) `(number->string ,x))
(define-macro (Sstring->number x) `(string->number ,x))
(define-macro (Sstring->number2 x base) `(string->number ,x ,base))
(define-macro (Slength lst) `(length ,lst))
(define-macro (Sappend lst1 lst2) `(append ,lst1 ,lst2))
(define-macro (Sassq x lst) `(assq ,x ,lst))
(define-macro (Smember x lst) `(member ,x ,lst))
(define-macro (Smap2 f lst) `(map ,f ,lst))
(define-macro (Smemq o lst) `(memq ,o ,lst))

(define-macro (SFLexact x) `(exact ,x))
(define-macro (SFXinexact x) `(inexact ,x))
(define-macro (SFLtruncate x) `(truncate ,x))
(define-macro (Svector-map2 f vect) `(vector-map ,f ,vect))
(define-macro (Slist->vector lst) `(list->vector ,lst))
(define-macro (Svector->list vect) `(vector->list ,vect))

(define-macro (Scall-with-current-continuation f)
   (match-case f
      ((lambda (?esc) ?body)
       `(bind-exit (,esc) ,body))
      (else
       (error "$call-with-current-continuation" "bad form" ',f))))

(define-macro (call-with-current-continuation f)
   (match-case f
      ((lambda (?esc) . ?body)
       `(bind-exit (,esc) ,@body))
      (else
       (error "call-with-current-continuation" "bad form" `',f))))

(define-macro (Smake-vector1 n)
   (define arithmetic
      (cond-expand
	 (arithmeticG 'G)
	 (arithmeticS 'S)
	 (else 'G)))
   (let ((a (gensym)))
      `(let ((,a ,n))
	  ,(if (eq? arithmetic 'S)
	       `(PRIMop make-vector ,a)
	       `(if (and (FIXNUM? ,a) (FX>= ,a 0))
		    (PRIMop make-vector ,a)
		    (DEAD-END "make-vector type error"))))))

(define-macro (Smake-vector2 n init)
   (define arithmetic
      (cond-expand
	 (arithmeticG 'G)
	 (arithmeticS 'S)
	 (else 'G)))
   (let ((a (gensym))
         (b (gensym)))
      `(let ((,a ,n)
             (,b ,init))
	  ,(if (eq? arithmetic 'S)
	       `(PRIMop make-vector ,a ,b)
	       `(if (and (FIXNUM? ,a) (FX>= ,a 0))
		    (PRIMop make-vector ,a ,b)
		    (DEAD-END "make-vector type error"))))))

(define-expander Svector-ref
   (lambda (x e)
      (define arithmetic
	 (cond-expand
	    (arithmeticG 'G)
	    (arithmeticS 'S)
	    (else 'G)))
      (let ((v (cadr x))
	    (i (caddr x))
	    (a (gensym))
	    (b (gensym)))
	 (e `(let ((,a ,v)
		   (,b ,i))
		,(if (eq? arithmetic 'S)
		     `(PRIMop vector-ref ,a ,b)
		     `(if (and (vector? ,a)
			       (FIXNUM? ,b)
			       (FX>= ,b 0)
			       (FX< ,b (PRIMop vector-length ,a)))
			  (PRIMop vector-ref ,a ,b)
			  (DEAD-END (format "type-error (vector-ref ~a ~a):~a"
				       (typeof ,a) (typeof ,b)
				       ',(cer x))))))
	    e))))

(define-macro (Svector-set! v i x)
   (define arithmetic
      (cond-expand
	 (arithmeticG 'G)
	 (arithmeticS 'S)
	 (else 'G)))
   (let ((a (gensym))
	 (b (gensym))
	 (c (gensym)))
      `(let ((,a ,v)
	     (,b ,i)
	     (,c ,x))
	  ,(if (eq? arithmetic 'S)
	       `(PRIMop vector-set! ,a ,b ,c)
	       `(if (and (vector? ,a) (FIXNUM? ,b) (FX>= ,b 0) (FX< ,b (PRIMop vector-length ,a)))
		    (PRIMop vector-set! ,a ,b ,c)
		    (DEAD-END (format "type-error (vector-set! ~a ~a ~a):~a"
				       (typeof ,a) ,b (typeof ,c) 
				       ',(if (epair? x) (cer x) x))))))))

(define-macro (Svector-length v)
   (define arithmetic
      (cond-expand
	 (arithmeticG 'G)
	 (arithmeticS 'S)
	 (else 'G)))
   (let ((a (gensym)))
      `(let ((,a ,v))
	  ,(if (eq? arithmetic 'S)
	       `(PRIMop vector-length ,a)
	       `(if (vector? ,a)
		    (PRIMop vector-length ,a)
		    (DEAD-END "vector-length type error"))))))

(define-macro (Smake-string1 n)
   (define arithmetic
      (cond-expand
	 (arithmeticG 'G)
	 (arithmeticS 'S)
	 (else 'G)))
  (let ((a (gensym)))
    `(let ((,a ,n))
       ,(if (eq? arithmetic 'S)
            `(PRIMop make-string ,a)
            `(if (and (FIXNUM? ,a) (FX>= ,a 0))
                 (PRIMop make-string ,a)
                 (DEAD-END "make-string type error"))))))

(define-macro (Smake-string2 n init)
   (define arithmetic
      (cond-expand
	 (arithmeticG 'G)
	 (arithmeticS 'S)
	 (else 'G)))
  (let ((a (gensym))
        (b (gensym)))
    `(let ((,a ,n)
           (,b ,init))
       ,(if (eq? arithmetic 'S)
            `(PRIMop make-string ,a ,b)
            `(if (and (FIXNUM? ,a) (FX>= ,a 0) (char? ,b))
                 (PRIMop make-string ,a ,b)
                 (DEAD-END "make-string type error"))))))

(define-expander Sstring-ref
   (lambda (x e)
      (define arithmetic
	 (cond-expand
	    (arithmeticG 'G)
	    (arithmeticS 'S)
	    (else 'G)))
      (match-case x
	 ((Sstring-ref ?s ?i)
	  (let ((a (gensym))
		(b (gensym)))
	     (e `(let ((,a ,s)
		       (,b ,i))
		    ,(if (eq? arithmetic 'S)
			 `(PRIMop string-ref ,a ,b)
			 `(if (not (string? ,a))
			      (DEAD-END
				 (format "type-error, string not a string (string-ref ~a ~a):~a"
				    (typeof ,a) (typeof ,b)
				    ',(if (epair? x) (cer x) x)))
			      (if (not (FIXNUM? ,b))
				  (DEAD-END
				     (format "type-error, index not a fixnum (string-ref ~a ~a):~a"
					(typeof ,a) (typeof ,b)
					',(if (epair? x) (cer x) x)))
				  (if (and (FX>= ,b 0) (FX< ,b (PRIMop string-length ,a)))
				      (PRIMop string-ref ,a ,b)
				      (DEAD-END
					 (format "type-error index out of bounds [~a/~a](string-ref ~a ~a):~a"
					    ,b (string-length ,a)
					    (typeof ,a) (typeof ,b)
					    ',(if (epair? x) (cer x) x))))))))
		e))))))

(define-macro (Sstring-ref.orig s i)
   (define arithmetic
      (cond-expand
	 (arithmeticG 'G)
	 (arithmeticS 'S)
	 (else 'G)))
  (let ((a (gensym))
        (b (gensym)))
    `(let ((,a ,s)
           (,b ,i))
       ,(if (eq? arithmetic 'S)
            `(PRIMop string-ref ,a ,b)
            `(if (and (string? ,a) (FIXNUM? ,b) (FX>= ,b 0) (FX< ,b (PRIMop string-length ,a)))
                 (PRIMop string-ref ,a ,b)
                 (DEAD-END (format "string-ref type error (~s, ~s)" ,a ,b)))))))

(define-macro (Sstring-set! s i x)
   (define arithmetic
      (cond-expand
	 (arithmeticG 'G)
	 (arithmeticS 'S)
	 (else 'G)))
  (let ((a (gensym))
        (b (gensym))
        (c (gensym)))
    `(let ((,a ,s)
           (,b ,i)
           (,c ,x))
       ,(if (eq? arithmetic 'S)
            `(PRIMop string-set! ,a ,b ,c)
            `(if (and (string? ,a) (FIXNUM? ,b) (FX>= ,b 0) (FX< ,b (PRIMop string-length ,a)))
                 (PRIMop string-set! ,a ,b ,c)
                 (DEAD-END "string-set! type error"))))))

(define-macro (Sstring-length s)
   (define arithmetic
      (cond-expand
	 (arithmeticG 'G)
	 (arithmeticS 'S)
	 (else 'G)))
  (let ((a (gensym)))
    `(let ((,a ,s))
       ,(if (eq? arithmetic 'S)
            `(PRIMop string-length ,a)
            `(if (string? ,a)
                 (PRIMop string-length ,a)
                 (DEAD-END "string-length type error"))))))

(define-macro (Sstring-append . args) `(string-append ,@args))

(define-macro (Ssubstring . args) `(substring ,@args))

(define-macro (Schar->integer x) `(char->integer ,x))
(define-macro (Sinteger->char x) `(integer->char ,x))

(define-macro (Schar<? x y)
   (define arithmetic
      (cond-expand
	 (arithmeticG 'G)
	 (arithmeticS 'S)
	 (else 'G)))
  (let ((a (gensym))
        (b (gensym)))
    `(let ((,a ,x)
           (,b ,y))
       ,(if (eq? arithmetic 'S)
            `(PRIMop char<? ,a ,b)
            `(if (and (char? ,a) (char? ,b))
                 (PRIMop char<? ,a ,b)
                 (DEAD-END "char<? type error"))))))

(define-macro (Schar>? x y)
   (define arithmetic
      (cond-expand
	 (arithmeticG 'G)
	 (arithmeticS 'S)
	 (else 'G)))
  (let ((a (gensym))
        (b (gensym)))
    `(let ((,a ,x)
           (,b ,y))
       ,(if (eq? arithmetic 'S)
            `(PRIMop char>? ,a ,b)
            `(if (and (char? ,a) (char? ,b))
                 (PRIMop char>? ,a ,b)
                 (DEAD-END "char>? type error"))))))


(define-macro (Schar<=? x y)
   (define arithmetic
      (cond-expand
	 (arithmeticG 'G)
	 (arithmeticS 'S)
	 (else 'G)))
  (let ((a (gensym))
        (b (gensym)))
    `(let ((,a ,x)
           (,b ,y))
       ,(if (eq? arithmetic 'S)
            `(PRIMop char<=? ,a ,b)
            `(if (and (char? ,a) (char? ,b))
                 (PRIMop char<=? ,a ,b)
                 (DEAD-END "char<=? type error"))))))

(define-macro (Schar>=? x y)
   (define arithmetic
      (cond-expand
	 (arithmeticG 'G)
	 (arithmeticS 'S)
	 (else 'G)))
  (let ((a (gensym))
        (b (gensym)))
    `(let ((,a ,x)
           (,b ,y))
       ,(if (eq? arithmetic 'S)
            `(PRIMop char>=? ,a ,b)
            `(if (and (char? ,a) (char? ,b))
                 (PRIMop char>=? ,a ,b)
                 (DEAD-END "char>=? type error"))))))

(define-expander Schar=?
   (lambda (x e)
      (define arithmetic
	 (cond-expand
	    (arithmeticG 'G)
	    (arithmeticS 'S)
	    (else 'G)))
	 (match-case x
	    ((Schar=? ?x ?y)
	     (let ((a (gensym))
		   (b (gensym)))
		(e `(let ((,a ,x)
			  (,b ,y))
		       ,(if (eq? arithmetic 'S)
			    `(PRIMop char=? ,a ,b)
			    `(if (and (char? ,a) (char? ,b))
				 (PRIMop char=? ,a ,b)
				 (DEAD-END
				    (format "type-error (char=? ~a ~a):~a"
				       (typeof ,a) (typeof ,b)
				       ',(if (epair? x) (cer x) x))))))
		   e))))))

(define-macro (Schar=?.orig x y)
   (define arithmetic
      (cond-expand
	 (arithmeticG 'G)
	 (arithmeticS 'S)
	 (else 'G)))
  (let ((a (gensym))
        (b (gensym)))
    `(let ((,a ,x)
           (,b ,y))
       ,(if (eq? arithmetic 'S)
            `(PRIMop char=? ,a ,b)
            `(if (and (char? ,a) (char? ,b))
                 (PRIMop char=? ,a ,b)
                 (DEAD-END "char=? type error"))))))

(define-macro (DEAD-END msg)
  `(error "bbv" "error" ,msg))

(define-macro (fatal-error msg . obj)
   `(DEAD-END ,msg))

(register-exit-function! (lambda (status) (bbv-saw-statistics) status))

(define (Sequal? x y)
   (cond
      ((eq? x y) #t)
      ((pair? x)
       (and (pair? y)
	    (Sequal? (Scar x) (Scar y))
	    (Sequal? (Scdr x) (Scdr y))))
      ((pair? y) #f)
      ((FLONUM? x)
       (and (FLONUM? y) (SFL= x y)))
      ((FLONUM? y)
       #f)
      ((vector? x)
       (and (vector? y)
	    (SFX= (vector-length x) (vector-length y))
	    (let loop ((i (SFX- (vector-length x) 1)))
	       (if (SFX>= i 0)
		   (if (Sequal? (Svector-ref x i) (Svector-ref y i))
		       (loop (SFX- i 1))
		       #f)
		   #t))))
      (else
       (equal? x y))))

(define-inline (odd?fx x)
   (oddfx? x))

(define-inline (even?fx x)
   (evenfx? x))

(define-inline (modulofl x y)
   (error "modulofl" "not implemented" #f))

(define-inline (quotientfl x y)
   (error "modulofl" "not implemented" #f))

