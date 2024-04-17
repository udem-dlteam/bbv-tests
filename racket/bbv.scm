(require compatibility/defmacro)

(define-macro (if c . branches)
  (if (= (length branches) 1)
    `(when ,c ,@branches)
    `(cond (,c ,(car branches)) (else ,(cadr branches)))))

(define-macro (cond-expand . clauses)
  (cdr (or (assq 'racket clauses) (assq 'else clauses))))

(define-macro (eof-object) 'eof)

(define-macro (FLop op . args)
  `(,op ,@args))

(define-macro (FXop op . args)
  `(,op ,@args))

(define-macro (PRIMop op . args)
  `(,op ,@args))

(define-macro (unknown . args)
`((lambda (first . others) first) ,@args))

(define-macro (MAPop* kind op . args)
  (if (<= (length args) 2)
      `(MAPop ,kind ,op ,@args)
      `(MAPop* ,kind ,op (MAPop ,kind ,op ,(car args) ,(cadr args)) ,@(cddr args))))

(define-macro (MAPop kind op . args)
  (define (symbol-append x y)
    (string->symbol (string-append (symbol->string x) (symbol->string y))))
  (cond
   ((eq? kind 'FL)
    `(PRIMop ,(symbol-append 'fl op) ,@args))
   ((eq? kind 'FX)
    `(PRIMop ,(symbol-append 'fx op) ,@args))
   (else
    `(,(symbol-append 'BBV op) ,@args))))

(define-macro (GEN+ x . rest)    `(MAPop* GEN + ,x ,@rest))
(define-macro (GEN- x . rest)    `(MAPop* GEN - ,x ,@rest))
(define-macro (GEN* x . rest)    `(MAPop* GEN * ,x ,@rest))
(define-macro (GEN/ x . rest)    `(MAPop* GEN / ,x ,@rest))
(define-macro (GENquotient x y)  `(MAPop GEN quotient ,x ,y))
(define-macro (GENremainder x y) `(MAPop GEN remainder ,x ,y))
(define-macro (GENmodulo x y)    `(MAPop GEN modulo ,x ,y))
(define-macro (GEN= x y)         `(MAPop GEN = ,x ,y))
(define-macro (GEN< x y)         `(MAPop GEN < ,x ,y))
(define-macro (GEN> x y)         `(MAPop GEN > ,x ,y))
(define-macro (GEN<= x y)        `(MAPop GEN <= ,x ,y))
(define-macro (GEN>= x y)        `(MAPop GEN >= ,x ,y))
(define-macro (GENzero? x)       `(MAPop GEN zero? ,x))
(define-macro (GENsqrt x)        `(MAPop GEN sqrt ,x))
(define-macro (GENsin x)         `(MAPop GEN sin ,x))
(define-macro (GENcos x)         `(MAPop GEN cos ,x))
(define-macro (GENatan x)        `(MAPop GEN atan ,x))
(define-macro (GENatan2 x y)     `(MAPop GEN atan2 ,x ,y))
(define-macro (GENmin . args)    `(MAPop GEN min ,@args))
(define-macro (GENmax . args)    `(MAPop GEN max ,@args))

(define-macro (SFL+ x . rest)    `(MAPop* SFL + ,x ,@rest))
(define-macro (SFL- x . rest)    `(MAPop* SFL - ,x ,@rest))
(define-macro (SFL* x . rest)    `(MAPop* SFL * ,x ,@rest))
(define-macro (SFL/ x . rest)    `(MAPop* SFL / ,x ,@rest))
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
(define-macro (SFLatan x)        `(MAPop SFL atan ,x))
(define-macro (SFLatan2 x y)     `(MAPop SFL atan2 ,x ,y))
(define-macro (SFLmin . args)    `(MAPop SFL min ,@args))
(define-macro (SFLmax . args)    `(MAPop SFL max ,@args))

(define-macro (SFX+ x . rest)    `(MAPop* SFX + ,x ,@rest))
(define-macro (SFX- x . rest)    `(MAPop* SFX - ,x ,@rest))
(define-macro (SFX* x . rest)    `(MAPop* SFX * ,x ,@rest))
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
(define-macro (SFXmin . args)    `(MAPop SFX min ,@args))
(define-macro (SFXmax . args)    `(MAPop SFX max ,@args))

(define-macro (SFXbit-lsh x y)   `(MAPop SFX bit-lsh ,x ,y))
(define-macro (SFXbit-and x y)   `(MAPop SFX bit-and ,x ,y))
(define-macro (SFXbit-or x y)    `(MAPop SFX bit-or ,x ,y))
(define-macro (SFXbit-not x)     `(MAPop SFX bit-not ,x))

(define-macro (FL+ x y)         `(FLop + ,x ,y))
(define-macro (FL- x . rest)    `(FLop - ,x ,@rest))
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
(define-macro (FLatan x)        `(FLop atan ,x))
(define-macro (FLatan2 x y)     `(FLop atan ,x ,y))
(define-macro (FLmin . args)    `(MAPop FL min ,@args))
(define-macro (FLmax . args)    `(MAPop FL max ,@args))

(define-macro (FX+ x y)         `(FXop + ,x ,y))
(define-macro (FX- x . rest)    `(FXop - ,x ,@rest))
(define-macro (FX* x y)         `(FXop * ,x ,y))
(define-macro (FXquotient x y)  `(FXop quotient ,x ,y))
(define-macro (FXremainder x y) `(FXop remainder ,x ,y))
(define-macro (FXmodulo x y)    `(FXop modulo ,x ,y))
(define-macro (FX= x y)         `(FXop = ,x ,y))
(define-macro (FX< x y)         `(FXop < ,x ,y))
(define-macro (FX> x y)         `(FXop > ,x ,y))
(define-macro (FX<= x y)        `(FXop <= ,x ,y))
(define-macro (FX>= x y)        `(FXop >= ,x ,y))
(define-macro (FXzero? x)       `(FXop zero? ,x))
(define-macro (FXodd? x)        `(FXop odd? ,x))
(define-macro (FXeven? x)       `(FXop even? ,x))
(define-macro (FXmin . args)    `(MAPop FX min ,@args))
(define-macro (FXmax . args)    `(MAPop FX max ,@args))

(define-macro (FXbit-lsh x y)   `(MAPop FX lshift ,x ,y))
(define-macro (FXbit-and x y)   `(MAPop FX and ,x ,y))
(define-macro (FXbit-or x y)    `(MAPop FX ior ,x ,y))
(define-macro (FXbit-not x)     `(MAPop FX not ,x))

(define-macro (FLONUM? x) `(PRIMop flonum? ,x))
(define-macro (FIXNUM? x) `(PRIMop fixnum? ,x))

(define-macro (BBVop op x . rest)
  `(,op ,x ,@rest))

(define-macro (BBVcmp op x y)
  `(,op ,x ,y))

(define-macro (BBV+ x y) `(BBVop + ,x ,y))
(define-macro (BBV- x . rest) `(BBVop - ,x ,@rest))
(define-macro (BBV* x y) `(BBVop * ,x ,y))

(define-macro (BBV/ x y)
  `(/ ,x ,y))

(define-macro (BBVquotient x y)
  `(quotient ,x ,y))

(define-macro (BBVremainder x y)
  `(remainder ,x ,y))

(define-macro (BBVmodulo x y)
  `(modulo ,x ,y))

(define-macro (BBVbit-lsh x y)
  `(arithmetic-shift ,x ,y))

(define-macro (BBVbit-and x y)
  `(bitwise-and ,x ,y))

(define-macro (BBVbit-or x y)
  `(bitwise-ior ,x ,y))

(define-macro (BBVbit-not x)
  `(bitwise-not ,x))

(define-macro (BBV= x y) `(BBVcmp = ,x ,y))
(define-macro (BBV< x y) `(BBVcmp < ,x ,y))
(define-macro (BBV<= x y) `(BBVcmp <= ,x ,y))
(define-macro (BBV>= x y) `(BBVcmp >= ,x ,y))
(define-macro (BBV> x y) `(BBVcmp > ,x ,y))

(define-macro (BBVzero? x)
  `(zero? ,x))

(define-macro (BBVodd? x)
  `(odd? ,x))

(define-macro (BBVeven? x)
  `(even? ,x))

(define-macro (BBVsqrt x)
  `(sqrt ,x))

(define-macro (BBVcos x)
  `(cos ,x))

(define-macro (BBVsin x)
  `(sin ,x))

(define-macro (BBVatan x)
  `(atan ,x))

(define-macro (BBVatan2 x y)
  `(atan ,x ,y))

(define-macro (BBVmax . args)
  `(max ,@args))

(define-macro (BBVmin . args)
  `(min ,@args))

(define-macro (Scar x)
  `(car ,x))

(define-macro (Scdr x)
  `(cdr ,x))

(define-macro (Scaar x) `(Scar (Scar ,x)))
(define-macro (Scadr x) `(Scar (Scdr ,x)))
(define-macro (Scdar x) `(Scdr (Scar ,x)))
(define-macro (Scddr x) `(Scdr (Scdr ,x)))
(define-macro (Scaadr x) `(Scar (Scar (Scdr ,x))))
(define-macro (Scdddr x) `(Scdr (Scdr (Scdr ,x))))
(define-macro (Scaddr x) `(Scar (Scdr (Scdr ,x))))
(define-macro (Scadar x) `(Scar (Scdr (Scar ,x))))
(define-macro (Scdadr x) `(Scdr (Scar (Scdr ,x))))
(define-macro (Scadddr x) `(Scar (Scdr (Scdr (Scdr ,x)))))

(define-macro (Smcar x)
  `(mcar ,x))

(define-macro (Smcdr x)
  `(mcdr ,x))

(define-macro (Sset-mcar! x y)
  `(set-mcar! ,x ,y))

(define-macro (Sset-mcdr! x y)
  `(set-mcdr! ,x ,y))

(define-macro (Smcaar x) `(Smcar (Smcar ,x)))
(define-macro (Smcadr x) `(Smcar (Smcdr ,x)))
(define-macro (Smcdar x) `(Smcdr (Smcar ,x)))
(define-macro (Smcddr x) `(Smcdr (Smcdr ,x)))
(define-macro (Smcaadr x) `(Smcar (Smcar (Smcdr ,x))))
(define-macro (Smcdddr x) `(Smcdr (Smcdr (Smcdr ,x))))
(define-macro (Smcaddr x) `(Smcar (Smcdr (Smcdr ,x))))
(define-macro (Smcadar x) `(Smcar (Smcdr (Smcar ,x))))
(define-macro (Smcdadr x) `(Smcdr (Smcar (Smcdr ,x))))
(define-macro (Smcadddr x) `(Smcar (Smcdr (Smcdr (Smcdr ,x)))))

(define-macro (Sstring->symbol x)
  `(string->symbol ,x))

(define-macro (Ssymbol->string x)
  `(symbol->string ,x))

(define-macro (Sstring->number s)
  `(string->number ,s))

(define-macro (Sstring->number2 s base)
  `(string->number ,s ,base))

(define-macro (SFXnumber->string x)
  `(number->string ,x))

(define-macro (SFXinexact x)
  `(inexact ,x))

(define-macro (SFLexact x)
  `(exact ,x))

(define-macro (SFLtruncate x)
  `(truncate ,x))

(define-macro (Schar->integer x)
  `(char->integer ,x))

(define-macro (Sinteger->char x)
  `(integer->char ,x))

(define-macro (Schar<? x y)
  `(char<? ,x ,y))

(define-macro (Schar>? x y)
  `(char>? ,x ,y))

(define-macro (Schar<=? x y)
  `(char<=? ,x ,y))

(define-macro (Schar>=? x y)
  `(char>=? ,x ,y))

(define-macro (Schar=? x y)
  `(char=? ,x ,y))

(define-macro (Schar-downcase c)
  `(char-downcase ,c))

(define-macro (Schar-alphabetic? c)
  `(char-alphabetic? ,c))

(define-macro (Schar-whitespace? c)
  `(Schar<=? ,c #\space))

(define-macro (Schar-numeric? c)
  `(let ((c ,c))
     (and (Schar>=? c #\0) (Schar<=? c #\9))))

(define-macro (Sstring=? str1 str2)
  `(LIBstring=? ,str1 ,str2))

(define-macro (Sstring<? str1 str2)
  `(LIBstring<? ,str1 ,str2))

(define-macro (Sstring-ci=? str1 str2)
  `(LIBstring-ci=? ,str1 ,str2))

(define-macro (Sequal? x y)
  `(LIBequal? ,x ,y))

(define-macro (Slist-tail lst i)
  `(let ((lst ,lst) (i ,i))
     (let loop ((lst lst) (i i))
       (if (SFX<= i 0)
           lst
           (loop (Scdr lst) (SFX- i 1))))))

(define-macro (Slist-ref lst i)
  `(let ((lst ,lst) (i ,i))
     (let loop ((lst lst) (i i))
       (if (SFX<= i 0)
           (Scar lst)
           (loop (Scdr lst) (SFX- i 1))))))

(define-macro (Slength lst)
  `(let ((lst ,lst))
     (let loop ((lst lst) (len 0))
       (if (pair? lst)
           (loop (Scdr lst) (SFX+ len 1))
           len))))

(define-macro (Smemq key lst)
  `(let ((key ,key) (lst ,lst))
     (let loop ((lst lst))
       (and (pair? lst)
            (if (eq? key (Scar lst))
                lst
                (loop (Scdr lst)))))))

(define-macro (Smemv key lst)
  `(let ((key ,key) (lst ,lst))
     (let loop ((lst lst))
       (and (pair? lst)
            (if (eqv? key (Scar lst))
                lst
                (loop (Scdr lst)))))))

(define-macro (Smember key lst)
  `(let ((key ,key) (lst ,lst))
     (let loop ((lst lst))
       (and (pair? lst)
            (if (Sequal? key (Scar lst))
                lst
                (loop (Scdr lst)))))))

(define-macro (Sassq key lst)
  `(let ((key ,key) (lst ,lst))
     (let loop ((lst lst))
       (and (pair? lst)
            (let ((x (Scar lst)))
              (if (eq? key (Scar x))
                  x
                  (loop (Scdr lst))))))))

(define-macro (Sassv key lst)
  `(let ((key ,key) (lst ,lst))
     (let loop ((lst lst))
       (and (pair? lst)
            (let ((x (Scar lst)))
              (if (eqv? key (Scar x))
                  x
                  (loop (Scdr lst))))))))

(define-macro (Sassoc key lst)
  `(let ((key ,key) (lst ,lst))
     (let loop ((lst lst))
       (and (pair? lst)
            (let ((x (Scar lst)))
              (if (Sequal? key (Scar x))
                  x
                  (loop (Scdr lst))))))))

(define-macro (Sappend lst1 lst2)
  `(let ((lst1 ,lst1) (lst2 ,lst2))
     (let loop ((lst lst1))
       (if (pair? lst)
           (cons (Scar lst) (loop (Scdr lst)))
           lst2))))

(define-macro (Smap2 f lst) ;; 2 parameter map
  `(let ((f ,f) (lst ,lst))
     (if (procedure? f)
      (letrec ((map2
                (lambda (f lst)
                  (if (pair? lst)
                      (cons (f (Scar lst)) (map2 f (Scdr lst)))
                      '()))))
        (map2 f lst))
      (DEAD-END "map type error"))))

(define-macro (Smap3 f lst1 lst2) ;; 3 parameter map
  `(let ((f ,f) (lst1 ,lst1) (lst2 ,lst2))
     (if (procedure? f)
      (letrec ((map3
                (lambda (f lst1 lst2)
                  (if (and (pair? lst1) (pair? lst2))
                      (cons (f (Scar lst1) (Scar lst2)) (map3 f (Scdr lst1) (Scdr lst2)))
                      '()))))
        (map3 f lst1 lst2))
      (DEAD-END "map type error"))))

(define-macro (Sfor-each2 f lst) ;; 2 parameter map
  `(let ((f ,f) (lst ,lst))
    (if (procedure? ,f)
      (letrec ((for-each2
                (lambda (f lst)
                  (if (pair? lst)
                      (begin
                        (f (Scar lst))
                        (for-each2 f (Scdr lst)))
                      #f))))
        (for-each2 f lst))
      (DEAD-END "for-each! type error"))))

(define-macro (Sfor-each3 f lst1 lst2) ;; 3 parameter map
  `(let ((f ,f) (lst1 ,lst1) (lst2 ,lst2))
    (if (procedure? ,f)
      (letrec ((for-each3
                (lambda (f lst1 lst2)
                  (if (and (pair? lst1) (pair? lst2))
                      (begin
                        (f (Scar lst1) (Scar lst2))
                        (for-each3 f (Scdr lst1) (Scdr lst2)))
                      #f))))
        (for-each3 f lst1 lst2))
      (DEAD-END "for-each! type error"))))

(define-macro (Sreverse lst)
  `(let ((lst ,lst))
     (let loop ((lst lst) (result '()))
       (if (pair? lst)
           (loop (Scdr lst) (cons (Scar lst) result))
           result))))

(define-macro (Slist->vector lst)
  `(LIBlist->vector ,lst))

(define-macro (Smlist->vector lst)
  `(LIBmlist->vector ,lst))

(define-macro (Slist->string lst)
  `(LIBlist->string ,lst))

(define-macro (Scall-with-current-continuation f)
  `(call-with-current-continuation ,f))

(define-macro (Svector-map2 f vect)
  `(vector-map ,f ,vect))

(define-macro (Svector->list vect)
  `(vector->list ,vect))

(define-macro (Sstring->list s)
  `(string->list ,s))

(define-macro (Smake-vector1 n)
  `(make-vector ,n))

(define-macro (Smake-vector2 n init)
  `(make-vector ,n ,init))

(define-macro (Svector-ref v i)
  `(vector-ref ,v ,i))

(define-macro (Svector-set! v i x)
  `(vector-set! ,v ,i ,x))

(define-macro (Svector-length v)
  `(vector-length ,v))

(define-macro (Smake-string1 n)
  `(make-string ,n))

(define-macro (Smake-string2 n init)
  `(make-string ,n ,init))

(define-macro (Sstring-ref s i)
  `(string-ref ,s ,i))

(define-macro (Sstring-set! s i x)
  `(string-set! ,s ,i ,x))

(define-macro (Sstring-length s)
  `(string-length ,s))

(define-macro (Sstring-append . args)
  `(string-append ,@args))

(define-macro (Ssubstring str start end)
  `(substring ,str ,start ,end))

(define-macro (fatal-error msg . rest)
  `(error "fatal-error" ,msg ,@rest))

(define-macro (DEAD-END msg)
  `(error 'dead-end "dead-end"))

(define-macro (Sdefine-record name . fields)
  `(error 'Sdefine-record "nit impelemented"))

(define-macro (define-keys signature . body)
  (let ()
    (define (get-base-signature signature)
      (if (eq? (car signature) '!key)
          '()
          (cons (car signature) (get-base-signature (cdr signature)))))

    (define (get-keys signature)
      (if (eq? (car signature) '!key)
          (cdr signature)
          (get-keys (cdr signature))))

    (define (get-key-binding pair)
      (let ((name (car pair))
            (default (cadr pair))
            (k (gensym)))
        `(,name (let ((,k (memq (quote ,(string->symbol (string-append (symbol->string name) ":"))) keywords)))
            (if ,k (cadr ,k) ,default)))))

  (let ((base-signature (get-base-signature signature))
        (keys (get-keys signature)))
    `(define (,@base-signature . keywords)
      (let (,@(map get-key-binding keys))
        ,@body)))))
