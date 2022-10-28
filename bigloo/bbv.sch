(directives
   (extern (include "./bbv_saw.h")
           (macro bbv-saw-statistics::int () "bbv_saw_statistics")))

(define-macro (FLop op . args)   `(,(symbol-append op 'fl) ,@args))
(define-macro (FXop op . args)   `(,(symbol-append op 'fx) ,@args))
(define-macro (PRIMop op . args) `(,op ,@args))

(define (unknown x) ((car (list (lambda () x)))))

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
   ((eq? kind 'GFL)
    `(,(symbol-append 'FL op) ,@args))
   ((eq? kind 'GFX)
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

(define-macro (GFL+ x y)         `(MAPop GFL + ,x ,y))
(define-macro (GFL- x y)         `(MAPop GFL - ,x ,y))
(define-macro (GFL* x y)         `(MAPop GFL * ,x ,y))
(define-macro (GFL/ x y)         `(MAPop GFL / ,x ,y))
(define-macro (GFLquotient x y)  `(MAPop GFL quotient ,x ,y))
(define-macro (GFLremainder x y) `(MAPop GFL remainder ,x ,y))
(define-macro (GFLmodulo x y)    `(MAPop GFL modulo ,x ,y))
(define-macro (GFL= x y)         `(MAPop GFL = ,x ,y))
(define-macro (GFL< x y)         `(MAPop GFL < ,x ,y))
(define-macro (GFL> x y)         `(MAPop GFL > ,x ,y))
(define-macro (GFL<= x y)        `(MAPop GFL <=,x ,y))
(define-macro (GFL>= x y)        `(MAPop GFL >= ,x ,y))
(define-macro (GFLzero? x)       `(MAPop GFL zero? ,x))
(define-macro (GFLsqrt x)        `(MAPop GFL sqrt ,x))
(define-macro (GFLsin x)         `(MAPop GFL sin ,x))
(define-macro (GFLcos x)         `(MAPop GFL cos ,x))
(define-macro (GFLatan2 x y)     `(MAPop GFL atan2 ,x ,y))

(define-macro (GFX+ x y)         `(MAPop GFX + ,x ,y))
(define-macro (GFX- x y)         `(MAPop GFX - ,x ,y))
(define-macro (GFX* x y)         `(MAPop GFX * ,x ,y))
(define-macro (GFXquotient x y)  `(MAPop GFX quotient ,x ,y))
(define-macro (GFXremainder x y) `(MAPop GFX remainder ,x ,y))
(define-macro (GFXmodulo x y)    `(MAPop GFX modulo ,x ,y))
(define-macro (GFX= x y)         `(MAPop GFX = ,x ,y))
(define-macro (GFX< x y)         `(MAPop GFX < ,x ,y))
(define-macro (GFX> x y)         `(MAPop GFX > ,x ,y))
(define-macro (GFX<= x y)        `(MAPop GFX <=,x ,y))
(define-macro (GFX>= x y)        `(MAPop GFX >= ,x ,y))
(define-macro (GFXzero? x)       `(MAPop GFX zero? ,x))

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
(define-macro (FL<= x y)        `(FLop <=,x ,y))
(define-macro (FL>= x y)        `(FLop >= ,x ,y))
(define-macro (FLzero? x)       `(FLop zero? ,x))
(define-macro (FLsqrt x)        `(FLop sqrt ,x))
(define-macro (FLsin x)         `(FLop sin ,x))
(define-macro (FLcos x)         `(FLop cos ,x))
(define-macro (FLatan2 x y)     `(FLop atan2 ,x ,y))

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
         (FLatan ,a ,b))
        (else
         (PRIMop atan ,a ,b))))))

(register-exit-function! (lambda (status) (bbv-saw-statistics) status))