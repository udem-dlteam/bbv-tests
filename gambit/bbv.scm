(declare
  (standard-bindings)
  (extended-bindings)
  (not safe)
  (block)
)

(define-macro (for-syntax . exprs)
  (for-each eval exprs)
  `(begin))

(for-syntax
 (define (symbol-append . rest)
   (string->symbol (string-concatenate (map symbol->string rest)))))

(define-macro (FLop op . args)   `(,(symbol-append '|##fl| op) ,@args))
(define-macro (FXop op . args)   `(,(symbol-append '|##fx| op) ,@args))
(define-macro (PRIMop op . args) `(let () (declare (not inline-primitives)) (,(symbol-append '|##| op) ,@args)))

(define-macro (unknown . args) `(PRIMop first-argument ,@args))

(define-macro (MAPop kind op . args)
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
(define-macro (SFL<= x y)        `(MAPop SFL <=,x ,y))
(define-macro (SFL>= x y)        `(MAPop SFL >= ,x ,y))
(define-macro (SFLzero? x)       `(MAPop SFL zero? ,x))
(define-macro (SFLsqrt x)        `(MAPop SFL sqrt ,x))
(define-macro (SFLsin x)         `(MAPop SFL sin ,x))
(define-macro (SFLcos x)         `(MAPop SFL cos ,x))
(define-macro (SFLatan2 x y)     `(MAPop SFL atan2 ,x ,y))

(define-macro (SFX+ x y)         `(MAPop SFX + ,x ,y))
(define-macro (SFX- x y)         `(MAPop SFX - ,x ,y))
(define-macro (SFX* x y)         `(MAPop SFX * ,x ,y))
(define-macro (SFXquotient x y)  `(MAPop SFX quotient ,x ,y))
(define-macro (SFXremainder x y) `(MAPop SFX remainder ,x ,y))
(define-macro (SFXmodulo x y)    `(MAPop SFX modulo ,x ,y))
(define-macro (SFX= x y)         `(MAPop SFX = ,x ,y))
(define-macro (SFX< x y)         `(MAPop SFX < ,x ,y))
(define-macro (SFX> x y)         `(MAPop SFX > ,x ,y))
(define-macro (SFX<= x y)        `(MAPop SFX <=,x ,y))
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
          (b (gensym))
          (c (gensym)))
      `(let ((,a ,x)
             (,b ,y))
         (cond
          ((and (FIXNUM? ,a) (FIXNUM? ,b))
           (let ((,c (,(symbol-append '|##fx| op '?) ,a ,b)))
             (if ,c ,c (PRIMop ,op ,a ,b))))
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
       (PRIMop atan ,a ,b))))

(define-macro (Scar x)
;$$$$$$$$$$$$$$$$$$$$$$$$$$$:@$WWk#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
;$$$$$$$$$$$$$$$$$$$$$$$$$Rb$RR$*!!!"*RR$$$$$$$$$$$$$$$$$$$$$$$$$
;$$$$$$$$$$$$$$$$$$$$$$$$#d$?R!!~!~         '"$$$$$$$$$$$$$$$$$$$
;$$$$$$$$$$$$$$$$$$$$$$@RMTM4!~!~          '*$$$$$$$$$$$$$$$$$$$$
;$$$$$$$$$$$$$$$$$$$$$$\?!!` ~               ^$$$$$$$$$$$$$$$$$$$
;$$$$$$$$$$$$$$$$$$$$#!H!                      "$$$$$$$$$$$$$$$$$
;$$$$$$$$$$$$$$$$$$$#zR"`     x@&!/             `2*$$$$$$$$$$$$$$
;$$$$$$$$$$$$$$$$$*.WRM!:u   d$$$X              'R$$$$$$$$$$$$$$$
;$$$$$$$$$$$$$$$$)W$MQW$$$N $$$$B~                *$$$$$$$$$$$$$$
;$$$$$$$$$$$$$$$`$!  R$$$$F@$$$$F <!           `X%!cX?$$$$$$$$$$$
;$$$$$$$$$$$$$$R~@L   "$$$$$$$$F .!!>           !!!!?$W#$$$$$$$$$
;$$$$$$$$$$$$$* !!!   :"?$$$$#F  ...   X!:      `` :x`M!$$$$$$$$$
;$$$$$$$$$$$)$$ !~~! '  4$$$$k  =**~ . XX?>     ':!~>.!3$$$$$$$$$
;$$$$$$$$$$$X$R ~>`   " @$$$$>  .:.!?!.!!X~`     ~~< !<$$$$$$$$$$
;$$$$$$$$$$$$MF.   zM+ d$$$RX.   ~!.\!!!!!>      '  !  $$$$$$$$$$
;$$$$$$$$$$$$$    '9!d$$$$$$t~     `~.>~~<<      </~   9$$$$$$$$$
;$$$$$$$$$$$$$'    "@$$$$$N#` - &u   ~` .  L           `$$$$$$$$$
;$$$$$$$$$$$$"      ^#$$R"     dRF  -    ' M           X$$$$$$$$$
;$$$$$$$$$$$$-      .  `  .+ u$R*"         '           t$$$$$$$$$
;$$$$$$$$$$$E      '6ic  sFu@$$$T .                    ?$$$$$$$$$
;$$$$$$$$$$$"       "$$L4$$$$$$T# $                    `$$$$$$$$$
;$$$$$$$$$$F         "$$'$$$$$$$R:K                     ?$$$$$$$$
;$$$$$$$$$R           '   "*$$$$FdE                      R$$$$$$$
;$$$$$$$$$ >           sBNeL ^##.$F                      '$$$$$$$
;$$$$$$$$F~           d$$$$$$WWW$$k                       ~$$$$$$
;$$$$$$$$' ~         f9$$$$$$$$$$$"                         #$$$$
;$$$$$$$F:<          4$$$$$$$$$*"                            #$$$
;$$$$$$F<!           9#$$$$$F~                               u?$$
;$$$$$"XX!~          9 $$$$#                                 $$J$
;$$$$\@X!!           @'$$$F                                  9$$$
;$$$)M)H!!-          ' $$$                                   M$$$
;$$!WhM@!`             `$E                                   9$$$
;$"$#8B!!<              ?>                                  .$$$$
;F$$d$RX!~                                      ~<          @$$$$
;W$%$8S!(.                               <  .~~:<`         s$$$$$
;$$!X$MX!                            -`:'::!!::<~!!       :$$$$$$
;$E\$$!!~                            <<'<!!<X!!XX!!!     '$$$$$$$
;$E<R5X!>                        ~~<<!!!!!!!!Xt!XXMX      #$$$$$$
;$R?M!!!                     :< /!!!!XH!XMMXMR7HNSXNX      $$$$$$
;$$`!!!.                  .:!!<<!!!XtXXM9MXB$@WRM$$!k      9$$$$$
;$$:!!~~'               x!!HX:!!?XtTM@$M&B$WB$$$$$$$$.     t$$$$$
;$$k!(~               x!!69!!RMXURMW$RE$$$B$$$$$$$$$BE     @$$$$$
;$$$`~:~             XM@9XX$$9&B$@$$$$$$$$$$$$$$$$$$$$   .@$$$$$$
;R$*%~~             '"*"#*##"**#**#**##**#******#**#*#   ********
  (let ((a (gensym)))
    `(let ((,a ,x))
       ,(if (eq? arithmetic 'S)
            `(PRIMop car ,a)
            `(if (pair? ,a)
                 (PRIMop car ,a)
                 (DEAD-END "car type error"))))))

(define-macro (Scdr x)
  (let ((a (gensym)))
    `(let ((,a ,x))
       ,(if (eq? arithmetic 'S)
            `(PRIMop cdr ,a)
            `(if (pair? ,a)
                 (PRIMop cdr ,a)
                 (DEAD-END "cdr type error"))))))

(define-macro (Sset-car! x y)
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
  (let ((a (gensym))
        (b (gensym)))
    `(let ((,a ,x)
           (,b ,y))
       ,(if (eq? arithmetic 'S)
            `(PRIMop set-cdr! ,a ,b)
            `(if (pair? ,a)
                 (PRIMop set-cdr! ,a ,b)
                 (DEAD-END "set-cdr! type error"))))))

(define-macro (Scadr x) `(Scar (Scdr ,x)))
(define-macro (Scddr x) `(Scdr (Scdr ,x)))

(define-macro (Sstring->symbol x) `(string->symbol ,x))
(define-macro (Sstring->list x) `(string->list ,x))
(define-macro (Ssymbol->string x) `(symbol->string ,x))
(define-macro (SFXnumber->string x) `(number->string ,x))
(define-macro (Slength lst) `(length ,lst))
(define-macro (Sappend lst1 lst2) `(append ,lst1 ,lst2))
(define-macro (Sassq x lst) `(assq ,x ,lst))
(define-macro (Smember x lst) `(member ,x ,lst))
(define-macro (Smemq x lst) `(memq ,x ,lst))
(define-macro (Smap2 f lst) `(map ,f ,lst))

(define-macro (SFLexact x) `(exact ,x))
(define-macro (SFXinexact x) `(inexact ,x))
(define-macro (SFLtruncate x) `(truncate ,x))
(define-macro (Svector-map2 f vect) `(vector-map ,f ,vect))
(define-macro (Scall-with-current-continuation f) `(call-with-current-continuation ,f))
(define-macro (Slist->vector lst) `(list->vector ,lst))
(define-macro (Svector->list vect) `(vector->list ,vect))
(define-macro (Slist->string lst) `(list->string ,lst))

(define-macro (Smake-vector1 n)
  (let ((a (gensym)))
    `(let ((,a ,n))
       ,(if (eq? arithmetic 'S)
            `(PRIMop make-vector ,a)
            `(if (and (FIXNUM? ,a) (FX>= ,a 0))
                 (PRIMop make-vector ,a)
                 (DEAD-END "make-vector type error"))))))

(define-macro (Smake-vector2 n init)
  (let ((a (gensym))
        (b (gensym)))
    `(let ((,a ,n)
           (,b ,init))
       ,(if (eq? arithmetic 'S)
            `(PRIMop make-vector ,a ,b)
            `(if (and (FIXNUM? ,a) (FX>= ,a 0))
                 (PRIMop make-vector ,a ,b)
                 (DEAD-END "make-vector type error"))))))

(define-macro (Svector-ref v i)
  (let ((a (gensym))
        (b (gensym)))
    `(let ((,a ,v)
           (,b ,i))
       ,(if (eq? arithmetic 'S)
            `(PRIMop vector-ref ,a ,b)
            `(if (and (vector? ,a) (FIXNUM? ,b) (FX>= ,b 0) (FX< ,b (PRIMop vector-length ,a)))
                 (PRIMop vector-ref ,a ,b)
                 (DEAD-END "vector-ref type error"))))))

(define-macro (Svector-set! v i x)
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
                 (DEAD-END "vector-set! type error"))))))

(define-macro (Svector-length v)
  (let ((a (gensym)))
    `(let ((,a ,v))
       ,(if (eq? arithmetic 'S)
            `(PRIMop vector-length ,a)
            `(if (vector? ,a)
                 (PRIMop vector-length ,a)
                 (DEAD-END "vector-length type error"))))))

(define-macro (Sstring-ref s i)
  (let ((a (gensym))
        (b (gensym)))
    `(let ((,a ,s)
           (,b ,i))
       ,(if (eq? arithmetic 'S)
            `(PRIMop string-ref ,a ,b)
            `(if (and (string? ,a) (FIXNUM? ,b) (FX>= ,b 0) (FX< ,b (PRIMop string-length ,a)))
                 (PRIMop string-ref ,a ,b)
                 (DEAD-END "string-ref type error"))))))

(define-macro (Sstring-set! s i x)
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
  (let ((a (gensym)))
    `(let ((,a ,s))
       ,(if (eq? arithmetic 'S)
            `(PRIMop string-length ,a)
            `(if (string? ,a)
                 (PRIMop string-length ,a)
                 (DEAD-END "string-length type error"))))))

(define-macro (Sstring-append . args) `(string-append ,@args))

(define-macro (DEAD-END msg)
  `(PRIMop dead-end))
