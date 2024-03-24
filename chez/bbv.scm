'\" ;; For Gambit, this starts commenting code; Chez will ignore this line

(import (scheme))

(define-syntax defmacro
  (lambda (stx)
    (syntax-case stx ()
      ((_ (name . args) body ...)
       (identifier? (syntax name))
       (syntax (define-syntax name
                 (lambda (stx2)
                   (define transformer (lambda args body ...))
                   (syntax-case stx2 ()
                     ((name2 form (... ...))
                      (datum->syntax
                       (syntax name2)
                       (apply transformer
                              (syntax->datum (syntax (form (... ...)))))))))))))))

(defmacro (cond-expand . clauses)
  (cond ((or (assq 'chez clauses)
             (assq 'else clauses))
         =>
         (lambda (clause)
           `(begin ,@(cdr clause))))
        (else
         (error 'cond-expand (symbol->string 'no-applicable-clause)))))

(defmacro (define-macro pattern . body)
  `(defmacro ,pattern

     (define arithmetic 'G)

     (define (symbol-append . syms)
       (string->symbol (apply string-append (map symbol->string syms))))

     (define-syntax defmacro
       (lambda (stx)
         (syntax-case stx ()
           ((_ (name . args) body ...)
            (identifier? (syntax name))
            (syntax (define-syntax name
                      (lambda (stx2)
                        (define transformer (lambda args body ...))
                        (syntax-case stx2 ()
                          ((name2 form (... ...))
                           (datum->syntax
                            (syntax name2)
                            (apply transformer
                                   (syntax->datum (syntax (form (... ...)))))))))))))))

     (defmacro (cond-expand . clauses)
       (cond ((or (assq 'chez clauses)
                  (assq 'else clauses))
              =>
              (lambda (clause)
                `(begin ,@(cdr clause))))
             (else
              (error 'cond-expand (symbol->string 'no-applicable-clause)))))

     ,@body))

;" ;; For Gambit, this ends commenting code

(cond-expand
  (gambit
   ;; Compilation declarations for Gambit (ignored by Chez)
   (declare
     (standard-bindings)
     (extended-bindings)
     (not safe) ;; safety is ensured by the macros BBV+, etc
     (block)
     )
   (define-macro (dummy)
     (eval '(define arithmetic 'G))
     (eval '(define (symbol-append . syms)
              (string->symbol (apply string-append (map symbol->string syms)))))
     #f)
   (dummy))
  (else
   #f))
   
(define-macro (FLop op . args)
  (cond-expand
    (gambit `(,(symbol-append (string->symbol "##fl") op) ,@args))
    (else `(,op ,@args))))

(define-macro (FXop op . args)
  (cond-expand
    (gambit `(,(symbol-append (string->symbol "##fx") op) ,@args))
    (else `(,op ,@args))))

(define-macro (PRIMop op . args)
  (cond-expand
    (gambit `(,(symbol-append (string->symbol "##") op) ,@args))
    (else `(,op ,@args))))

(define-macro (unknown . args)
  (cond-expand
    (gambit `(PRIMop first-argument ,@args))
    (else `((lambda (first . others) first) ,@args))))

(define-macro (MAPop* kind op . args)
  (if (<= (length args) 2)
      `(MAPop ,kind ,op ,@args)
      `(MAPop* ,kind ,op (MAPop ,kind ,op ,(car args) ,(cadr args)) ,@(cddr args))))

(define-macro (MAPop kind op . args)
  (cond
   ((eq? kind 'FL)
    `(PRIMop ,(symbol-append 'fl op) ,@args))
   ((eq? kind 'FX)
    `(PRIMop ,(symbol-append 'fx op) ,@args))
   ((or (eq? kind 'GEN) (eq? 'arithmetic 'G)) ;; force generic?
    `(,(symbol-append 'BBV op) ,@args))
   ((eq? kind 'SFL)
    `(,(symbol-append 'FL op) ,@args))
   ((eq? kind 'SFX)
    `(,(symbol-append 'FX op) ,@args))))

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

(define-macro (FXbit-lsh x y)   `(MAPop FX arithmetic-shift-left ,x ,y))
(define-macro (FXbit-and x y)   `(MAPop FX and ,x ,y))
(define-macro (FXbit-or x y)    `(MAPop FX ior ,x ,y))
(define-macro (FXbit-not x)     `(MAPop FX not ,x))

(define-macro (FLONUM? x) `(PRIMop flonum? ,x))
(define-macro (FIXNUM? x) `(PRIMop fixnum? ,x))

(define-macro (BBVop op x . rest)
  (cond-expand
    (gambit
     (if (pair? rest)
         (let ((y (car rest)))
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
                    (let ((,c (,(symbol-append (string->symbol "##fx") op '?) ,a ,b)))
                      (if ,c ,c (PRIMop ,op ,a ,b))))
                   ((and (FLONUM? ,a) (FLONUM? ,b))
                    (FLop ,op ,a ,b))
                   (else
                    (PRIMop ,op ,a ,b))))))))
         (let ((a (gensym))
               (c (gensym)))
           `(let ((,a ,x))
              (cond
               ((FIXNUM? ,a)
                (let ((,c (,(symbol-append (string->symbol "##fx") op '?) ,a)))
                  (if ,c ,c (PRIMop ,op ,a))))
               ((FLONUM? ,a)
                (FLop ,op ,a))
               (else
                (PRIMop ,op ,a)))))))
    (else
     `(,op ,x ,@rest))))

(define-macro (BBVcmp op x y)
  (cond-expand
    (gambit
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
    (else
     `(,op ,x ,y))))

(define-macro (BBV+ x y) `(BBVop + ,x ,y))
(define-macro (BBV- x . rest) `(BBVop - ,x ,@rest))
(define-macro (BBV* x y) `(BBVop * ,x ,y))

(define-macro (BBV/ x y)
  (cond-expand
    (gambit
     (let ((a (gensym))
           (b (gensym)))
       `(let ((,a ,x)
              (,b ,y))
          (cond
           ((and (FLONUM? ,a) (FLONUM? ,b))
            (FL/ ,a ,b))
           (else
            (PRIMop / ,a ,b))))))
    (else
     `(/ ,x ,y))))

(define-macro (BBVquotient x y)
  (cond-expand
    (gambit
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
    (else
     `(quotient ,x ,y))))

(define-macro (BBVremainder x y)
  (cond-expand
    (gambit
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
    (else
     `(remainder ,x ,y))))

(define-macro (BBVmodulo x y)
  (cond-expand
    (gambit
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
    (else
     `(modulo ,x ,y))))

(define-macro (BBVbit-lsh x y)
  (cond-expand
    (gambit
     (let ((a (gensym))
           (b (gensym)))
       `(let ((,a ,x)
              (,b ,y))
          (cond
           ((and (FIXNUM? ,a) (FIXNUM? ,b))
            (FXbit-lsh ,a ,b))
           (else
            (DEAD-END "bit-lsh type error"))))))
    (else
     `(ash ,x ,y))))

(define-macro (BBVbit-and x y)
  (cond-expand
    (gambit
     (let ((a (gensym))
           (b (gensym)))
       `(let ((,a ,x)
              (,b ,y))
          (cond
           ((and (FIXNUM? ,a) (FIXNUM? ,b))
            (FXbit-and ,a ,b))
           (else
            (DEAD-END "bit-and type error"))))))
    (else
     `(logand ,x ,y))))

(define-macro (BBVbit-or x y)
  (cond-expand
    (gambit
     (let ((a (gensym))
           (b (gensym)))
       `(let ((,a ,x)
              (,b ,y))
          (cond
           ((and (FIXNUM? ,a) (FIXNUM? ,b))
            (FXbit-or ,a ,b))
           (else
            (DEAD-END "bit-or type error"))))))
    (else
     `(logior ,x ,y))))

(define-macro (BBVbit-not x)
  (cond-expand
    (gambit
     (let ((a (gensym)))
       `(let ((,a ,x))
          (cond
           ((FIXNUM? ,a)
            (FXbit-not ,a))
           (else
            (DEAD-END "bit-not type error"))))))
    (else
     `(lognot ,x))))

(define-macro (BBV= x y) `(BBVcmp = ,x ,y))
(define-macro (BBV< x y) `(BBVcmp < ,x ,y))
(define-macro (BBV<= x y) `(BBVcmp <= ,x ,y))
(define-macro (BBV>= x y) `(BBVcmp >= ,x ,y))
(define-macro (BBV> x y) `(BBVcmp > ,x ,y))

(define-macro (BBVzero? x)
  (cond-expand
    (gambit
     (let ((a (gensym)))
       `(let ((,a ,x))
          (cond
           ((FIXNUM? ,a) (FXzero? ,a))
           ((FLONUM? ,a) (FLzero? ,a))
           (else (PRIMop zero? ,a))))))
    (else
     `(zero? ,x))))

(define-macro (BBVodd? x)
  (cond-expand
    (gambit
     (let ((a (gensym)))
       `(let ((,a ,x))
          (cond
           ((FIXNUM? ,a) (FXodd? ,a))
           (else (PRIMop odd? ,a))))))
    (else
     `(odd? ,x))))

(define-macro (BBVeven? x)
  (cond-expand
    (gambit
     (let ((a (gensym)))
       `(let ((,a ,x))
          (cond
           ((FIXNUM? ,a) (FXeven? ,a))
           (else (PRIMop even? ,a))))))
    (else
     `(even? ,x))))

(define-macro (BBVsqrt x)
  (cond-expand
    (gambit
     (let ((a (gensym)))
       `(let ((,a ,x))
          (cond
           ((FLONUM? ,a) (FLsqrt ,a))
           (else (PRIMop sqrt ,a))))))
    (else
     `(sqrt ,x))))

(define-macro (BBVcos x)
  (cond-expand
    (gambit
     (let ((a (gensym)))
       `(let ((,a ,x))
          (if (FLONUM? ,a)
              (FLcos ,a)
              (PRIMop cos ,a)))))
    (else
     `(cos ,x))))

(define-macro (BBVsin x)
  (cond-expand
    (gambit
     (let ((a (gensym)))
       `(let ((,a ,x))
          (if (FLONUM? ,a)
              (FLsin ,a)
              (PRIMop sin ,a)))))
    (else
     `(sin ,x))))

(define-macro (BBVatan x)
  (cond-expand
    (gambit
     (let ((a (gensym)))
       `(let ((,a ,x))
          (PRIMop atan ,a))))
    (else
     `(atan ,x))))

(define-macro (BBVatan2 x y)
  (cond-expand
    (gambit
     (let ((a (gensym))
           (b (gensym)))
       `(let ((,a ,x)
              (,b ,y))
          (PRIMop atan ,a ,b))))
    (else
     `(atan ,x ,y))))

(define-macro (Scar x)
  (cond-expand
    (gambit
     (let ((a (gensym)))
       `(let ((,a ,x))
          ,(if (eq? arithmetic 'S)
               `(PRIMop car ,a)
               `(if (pair? ,a)
                    (PRIMop car ,a)
                    (DEAD-END "car type error"))))))
    (else
     `(car ,x))))

(define-macro (Scdr x)
  (cond-expand
    (gambit
     (let ((a (gensym)))
       `(let ((,a ,x))
          ,(if (eq? arithmetic 'S)
               `(PRIMop cdr ,a)
               `(if (pair? ,a)
                    (PRIMop cdr ,a)
                    (DEAD-END "cdr type error"))))))
    (else
     `(cdr ,x))))

(define-macro (Sset-car! x y)
  (cond-expand
    (gambit
     (let ((a (gensym))
           (b (gensym)))
       `(let ((,a ,x)
              (,b ,y))
          ,(if (eq? arithmetic 'S)
               `(PRIMop set-car! ,a ,b)
               `(if (pair? ,a)
                    (PRIMop set-car! ,a ,b)
                    (DEAD-END "set-car! type error"))))))
    (else
     `(set-car! ,x ,y))))

(define-macro (Sset-cdr! x y)
  (cond-expand
    (gambit
     (let ((a (gensym))
           (b (gensym)))
       `(let ((,a ,x)
              (,b ,y))
          ,(if (eq? arithmetic 'S)
               `(PRIMop set-cdr! ,a ,b)
               `(if (pair? ,a)
                    (PRIMop set-cdr! ,a ,b)
                    (DEAD-END "set-cdr! type error"))))))
    (else
     `(set-cdr! ,x ,y))))

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

(define-macro (Sstring->symbol x)
  (cond-expand
    (gambit
     (let ((a (gensym)))
       `(let ((,a ,x))
          ,(if (eq? arithmetic 'S)
               `(PRIMop string->symbol ,a)
               `(if (string? ,a)
                    (PRIMop string->symbol ,a)
                    (DEAD-END "string->symbol type error"))))))
    (else
     `(string->symbol ,x))))

(define-macro (Ssymbol->string x)
  (cond-expand
    (gambit
     (let ((a (gensym)))
       `(let ((,a ,x))
          ,(if (eq? arithmetic 'S)
               `(PRIMop symbol->string ,a)
               `(if (symbol? ,a)
                    (PRIMop symbol->string ,a)
                    (DEAD-END "symbol->string type error"))))))
    (else
     `(symbol->string ,x))))

(define-macro (Sstring->number s)
  (cond-expand
    (gambit
     (let ((a (gensym)))
       `(let ((,a ,s))
          ,(if (eq? arithmetic 'S)
               `(PRIMop string->number ,a)
               `(if (string? ,a)
                    (PRIMop string->number ,a)
                    (DEAD-END "string->number type error"))))))
    (else
     `(string->number ,s))))

(define-macro (Sstring->number2 s base)
  (cond-expand
    (gambit
     (let ((a (gensym))
           (b (gensym)))
       `(let ((,a ,s)
              (,b ,base))
          ,(if (eq? arithmetic 'S)
               `(PRIMop string->number ,a ,b)
               `(if (and (string? ,a) (FIXNUM? ,b) (FX>= ,b 2) (FX< ,b 16))
                    (PRIMop string->number ,a ,b)
                    (DEAD-END "string->number type error"))))))
    (else
     `(string->number ,s ,base))))

(define-macro (SFXnumber->string x)
  (cond-expand
    (gambit
     (let ((a (gensym)))
       `(let ((,a ,x))
          ,(if (eq? arithmetic 'S)
               `(PRIMop fixnum->string ,a)
               `(if (FIXNUM? ,a)
                    (PRIMop fixnum->string ,a)
                    (PRIMop number->string ,a))))))
    (else
     `(number->string ,x))))

(define-macro (SFXinexact x)
  (cond-expand
    (gambit
     (let ((a (gensym)))
       `(let ((,a ,x))
          ,(if (eq? arithmetic 'S)
               `(PRIMop fixnum->flonum ,a)
               `(if (FIXNUM? ,a)
                    (PRIMop fixnum->flonum ,a)
                    (PRIMop inexact ,a))))))
    (else
     `(inexact ,x))))

(define-macro (SFLexact x)
  (cond-expand
    (gambit
     (let ((a (gensym)))
       `(let ((,a ,x))
          ,(if (eq? arithmetic 'S)
               `(PRIMop flonum->fixnum ,a)
               `(if (FLONUM? ,a)
                    (PRIMop flonum->fixnum ,a)
                    (PRIMop exact ,a))))))
    (else
     `(exact ,x))))

(define-macro (SFLtruncate x)
  (cond-expand
    (gambit
     (let ((a (gensym)))
       `(let ((,a ,x))
          ,(if (eq? arithmetic 'S)
               `(FLop truncate ,a)
               `(if (FLONUM? ,a)
                    (FLop truncate ,a)
                    (PRIMop truncate ,a))))))
    (else
     `(truncate ,x))))

(define-macro (Schar->integer x)
  (cond-expand
    (gambit
     (let ((a (gensym)))
       `(let ((,a ,x))
          ,(if (eq? arithmetic 'S)
               `(PRIMop char->integer ,a)
               `(if (char? ,a)
                    (PRIMop char->integer ,a)
                    (DEAD-END "char->integer type error"))))))
    (else
     `(char->integer ,x))))

(define-macro (Sinteger->char x)
  (cond-expand
    (gambit
     (let ((a (gensym)))
       `(let ((,a ,x))
          ,(if (eq? arithmetic 'S)
               `(PRIMop integer->char ,a)
               `(if (and (FIXNUM? ,a) (FX>= ,a 0) (FX< ,a #x110000))
                    (PRIMop integer->char ,a)
                    (DEAD-END "integer->char type error"))))))
    (else
     `(integer->char ,x))))

(define-macro (Schar<? x y)
  (cond-expand
    (gambit
     (let ((a (gensym))
           (b (gensym)))
       `(let ((,a ,x)
              (,b ,y))
          ,(if (eq? arithmetic 'S)
               `(PRIMop char<? ,a ,b)
               `(if (and (char? ,a) (char? ,b))
                    (PRIMop char<? ,a ,b)
                    (DEAD-END "char<? type error"))))))
    (else
     `(char<? ,x ,y))))

(define-macro (Schar>? x y)
  (cond-expand
    (gambit
     (let ((a (gensym))
           (b (gensym)))
       `(let ((,a ,x)
              (,b ,y))
          ,(if (eq? arithmetic 'S)
               `(PRIMop char>? ,a ,b)
               `(if (and (char? ,a) (char? ,b))
                    (PRIMop char>? ,a ,b)
                    (DEAD-END "char>? type error"))))))
    (else
     `(char>? ,x ,y))))

(define-macro (Schar<=? x y)
  (cond-expand
    (gambit
     (let ((a (gensym))
           (b (gensym)))
       `(let ((,a ,x)
              (,b ,y))
          ,(if (eq? arithmetic 'S)
               `(PRIMop char<=? ,a ,b)
               `(if (and (char? ,a) (char? ,b))
                    (PRIMop char<=? ,a ,b)
                    (DEAD-END "char<=? type error"))))))
    (else
     `(char<=? ,x ,y))))

(define-macro (Schar>=? x y)
  (cond-expand
    (gambit
     (let ((a (gensym))
           (b (gensym)))
       `(let ((,a ,x)
              (,b ,y))
          ,(if (eq? arithmetic 'S)
               `(PRIMop char>=? ,a ,b)
               `(if (and (char? ,a) (char? ,b))
                    (PRIMop char>=? ,a ,b)
                    (DEAD-END "char>=? type error"))))))
    (else
     `(char>=? ,x ,y))))

(define-macro (Schar=? x y)
  (cond-expand
    (gambit
     (let ((a (gensym))
           (b (gensym)))
       `(let ((,a ,x)
              (,b ,y))
          ,(if (eq? arithmetic 'S)
               `(PRIMop char=? ,a ,b)
               `(if (and (char? ,a) (char? ,b))
                    (PRIMop char=? ,a ,b)
                    (DEAD-END "char=? type error"))))))
    (else
     `(char=? ,x ,y))))

(define-macro (Schar-downcase c)
  (cond-expand
    (gambit
     `(let ((c ,c))
        (if (Schar<=? c #\Z)
            (if (Schar>=? c #\A)
                (PRIMop integer->char (FX+ (PRIMop char->integer c) 32))
                c)
            c)))
    (else
     `(char-downcase ,c))))

(define-macro (Schar-alphabetic? c)
  (cond-expand
    (gambit
     `(let ((c ,c))
        (if (Schar>=? c #\a)
            (Schar<=? c #\z)
            (and (Schar>=? c #\A) (Schar<=? c #\Z)))))
    (else
     `(char-alphabetic? ,c))))

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

(define-macro (Slist->string lst)
  `(LIBlist->string ,lst))

(define-macro (Scall-with-current-continuation f)
  (cond-expand
    (gambit
     (let ((a (gensym)))
       `(let ((,a ,f))
          ,(if (eq? arithmetic 'S)
               `(PRIMop call-with-current-continuation ,a)
               `(if (procedure? ,a)
                    (PRIMop call-with-current-continuation ,a)
                    (DEAD-END "call-with-current-continuation type error"))))))
    (else
     `(call-with-current-continuation ,f))))

(define-macro (Svector-map2 f vect)
  (cond-expand
    (gambit
     (let ((a (gensym))
           (b (gensym)))
       `(let ((,a ,f)
              (,b ,vect))
          ,(if (eq? arithmetic 'S)
               `(PRIMop vector-map ,a ,b)
               `(if (and (procedure? ,a) (vector? ,b))
                    (PRIMop vector-map ,a ,b)
                    (DEAD-END "vector-map type error"))))))
    (else
     `(vector-map ,f ,vect))))

(define-macro (Svector->list vect)
  (cond-expand
    (gambit
     (let ((a (gensym)))
       `(let ((,a ,vect))
          ,(if (eq? arithmetic 'S)
               `(PRIMop vector->list ,a)
               `(if (vector? ,a)
                    (PRIMop vector->list ,a)
                    (DEAD-END "vector->list type error"))))))
    (else
     `(vector->list ,vect))))

(define-macro (Sstring->list s)
  (cond-expand
    (gambit
     (let ((a (gensym)))
       `(let ((,a ,s))
          ,(if (eq? arithmetic 'S)
               `(PRIMop string->list ,a)
               `(if (string? ,a)
                    (PRIMop string->list ,a)
                    (DEAD-END "string->list type error"))))))
    (else
     `(string->list ,s))))

(define-macro (Smake-vector1 n)
  (cond-expand
    (gambit
     (let ((a (gensym)))
       `(let ((,a ,n))
          ,(if (eq? arithmetic 'S)
               `(PRIMop make-vector ,a)
               `(if (and (FIXNUM? ,a) (FX>= ,a 0))
                    (PRIMop make-vector ,a)
                    (DEAD-END "make-vector type error"))))))
    (else
     `(make-vector ,n))))

(define-macro (Smake-vector2 n init)
  (cond-expand
    (gambit
     (let ((a (gensym))
           (b (gensym)))
       `(let ((,a ,n)
              (,b ,init))
          ,(if (eq? arithmetic 'S)
               `(PRIMop make-vector ,a ,b)
               `(if (and (FIXNUM? ,a) (FX>= ,a 0))
                    (PRIMop make-vector ,a ,b)
                    (DEAD-END "make-vector type error"))))))
    (else
     `(make-vector ,n ,init))))

(define-macro (Svector-ref v i)
  (cond-expand
    (gambit
     `(Svector-ref-with-FX>=0-FX< ,v ,i)
;;     `(Svector-ref-with-in-bounds ,v ,i)
)
    (else
     `(vector-ref ,v ,i))))

(define-macro (Svector-set! v i x)
  (cond-expand
    (gambit
     `(Svector-set!-with-FX>=0-FX< ,v ,i ,x)
;;     `(Svector-set!-with-in-bounds ,v ,i ,x)
)
    (else
     `(vector-set! ,v ,i ,x))))

(define-macro (Svector-ref-with-FX>=0-FX< v i)
  (let ((a (gensym))
        (b (gensym)))
    `(let ((,a ,v)
           (,b ,i))
       ,(if (eq? arithmetic 'S)
            `(PRIMop vector-ref ,a ,b)
            `(if (and (vector? ,a) (FIXNUM? ,b) (FX>= ,b 0) (FX< ,b (PRIMop vector-length ,a)))
                 (PRIMop vector-ref ,a ,b)
                 (DEAD-END "vector-ref type error"))))))

(define-macro (Svector-ref-with-in-bounds v i)
  (let ((a (gensym))
        (b (gensym)))
    `(let ((,a ,v)
           (,b ,i))
       ,(if (eq? arithmetic 'S)
            `(PRIMop vector-ref ,a ,b)
            `(if (and (vector? ,a) (FIXNUM? ,b) (PRIMop vector-in-bounds? ,a ,b))
                 (PRIMop vector-ref ,a ,b)
                 (DEAD-END "vector-ref type error"))))))

(define-macro (Svector-set!-with-FX>=0-FX< v i x)
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

(define-macro (Svector-set!-with-in-bounds v i x)
  (let ((a (gensym))
        (b (gensym))
        (c (gensym)))
    `(let ((,a ,v)
           (,b ,i)
           (,c ,x))
       ,(if (eq? arithmetic 'S)
            `(PRIMop vector-set! ,a ,b ,c)
            `(if (and (vector? ,a) (FIXNUM? ,b) (PRIMop vector-in-bounds? ,a ,b))
                 (PRIMop vector-set! ,a ,b ,c)
                 (DEAD-END "vector-set! type error"))))))

(define-macro (Svector-length v)
  (cond-expand
    (gambit
     (let ((a (gensym)))
       `(let ((,a ,v))
          ,(if (eq? arithmetic 'S)
               `(PRIMop vector-length ,a)
               `(if (vector? ,a)
                    (PRIMop vector-length ,a)
                    (DEAD-END "vector-length type error"))))))
    (else
     `(vector-length ,v))))

(define-macro (Smake-string1 n)
  (cond-expand
    (gambit
     (let ((a (gensym)))
       `(let ((,a ,n))
          ,(if (eq? arithmetic 'S)
               `(PRIMop make-string ,a)
               `(if (and (FIXNUM? ,a) (FX>= ,a 0))
                    (PRIMop make-string ,a)
                    (DEAD-END "make-string type error"))))))
    (else
     `(make-string ,n))))

(define-macro (Smake-string2 n init)
  (cond-expand
    (gambit
     (let ((a (gensym))
           (b (gensym)))
       `(let ((,a ,n)
              (,b ,init))
          ,(if (eq? arithmetic 'S)
               `(PRIMop make-string ,a ,b)
               `(if (and (FIXNUM? ,a) (FX>= ,a 0) (char? ,b))
                    (PRIMop make-string ,a ,b)
                    (DEAD-END "make-string type error"))))))
    (else
     `(make-string ,n ,init))))

(define-macro (Sstring-ref s i)
  (cond-expand
    (gambit
     `(Sstring-ref-with-FX>=0-FX< ,s ,i)
;;     `(Sstring-ref-with-in-bounds ,s ,i)
)
    (else
     `(string-ref ,s ,i))))

(define-macro (Sstring-set! s i x)
  (cond-expand
    (gambit
     `(Sstring-set!-with-FX>=0-FX< ,s ,i ,x)
;;     `(Sstring-set!-with-in-bounds ,s ,i ,x)
)
    (else
     `(string-set! ,s ,i ,x))))

(define-macro (Sstring-ref-with-FX>=0-FX< s i)
  (let ((a (gensym))
        (b (gensym)))
    `(let ((,a ,s)
           (,b ,i))
       ,(if (eq? arithmetic 'S)
            `(PRIMop string-ref ,a ,b)
            `(if (and (string? ,a) (FIXNUM? ,b) (FX>= ,b 0) (FX< ,b (PRIMop string-length ,a)))
                 (PRIMop string-ref ,a ,b)
                 (DEAD-END "string-ref type error"))))))

(define-macro (Sstring-ref-with-in-bounds s i)
  (let ((a (gensym))
        (b (gensym)))
    `(let ((,a ,s)
           (,b ,i))
       ,(if (eq? arithmetic 'S)
            `(PRIMop string-ref ,a ,b)
            `(if (and (string? ,a) (FIXNUM? ,b) (PRIMop string-in-bounds? ,a ,b))
                 (PRIMop string-ref ,a ,b)
                 (DEAD-END "string-ref type error"))))))

(define-macro (Sstring-set!-with-FX>=0-FX< s i x)
  (let ((a (gensym))
        (b (gensym))
        (c (gensym)))
    `(let ((,a ,s)
           (,b ,i)
           (,c ,x))
       ,(if (eq? arithmetic 'S)
            `(PRIMop string-set! ,a ,b ,c)
            `(if (and (string? ,a) (FIXNUM? ,b) (FX>= ,b 0) (FX< ,b (PRIMop string-length ,a)) (char? ,c))
                 (PRIMop string-set! ,a ,b ,c)
                 (DEAD-END "string-set! type error"))))))

(define-macro (Sstring-set!-with-in-bounds s i x)
  (let ((a (gensym))
        (b (gensym))
        (c (gensym)))
    `(let ((,a ,s)
           (,b ,i)
           (,c ,x))
       ,(if (eq? arithmetic 'S)
            `(PRIMop string-set! ,a ,b ,c)
            `(if (and (string? ,a) (FIXNUM? ,b) (PRIMop string-in-bounds? ,a ,b) (char? ,c))
                 (PRIMop string-set! ,a ,b ,c)
                 (DEAD-END "string-set! type error"))))))

(define-macro (Sstring-length s)
  (cond-expand
    (gambit
     (let ((a (gensym)))
       `(let ((,a ,s))
          ,(if (eq? arithmetic 'S)
               `(PRIMop string-length ,a)
               `(if (string? ,a)
                    (PRIMop string-length ,a)
                    (DEAD-END "string-length type error"))))))
    (else
     `(string-length ,s))))

(define-macro (Sstring-append . args)
  (cond-expand
    (gambit
     (let ((vars (map (lambda (_) (gensym)) args)))
       `(let ,(map list vars args)
          ,(if (eq? arithmetic 'S)
               `(PRIMop string-append ,@vars)
               `(if (and ,@(map (lambda (var) `(string? ,var)) vars))
                    (PRIMop string-append ,@vars)
                    (DEAD-END "string-append type error"))))))
    (else
     `(string-append ,@args))))

(define-macro (Ssubstring str start end)
  (cond-expand
    (gambit
     (let ((a (gensym))
           (b (gensym))
           (c (gensym)))
       `(let ((,a ,str)
              (,b ,start)
              (,c ,end))
          ,(if (eq? arithmetic 'S)
               `(PRIMop substring ,a ,b ,c)
               `(if (and (string? ,a)
                         (FIXNUM? ,b) (FX>= ,b 0) (FX< ,b (PRIMop string-length ,a))
                         (FIXNUM? ,c) (FX>= ,c ,b) (FX< ,c (PRIMop string-length ,a)))
                    `(PRIMop substring ,a ,b ,c)
                    (DEAD-END "substring type error"))))))
    (else
     `(substring ,str ,start ,end))))

(define-macro (fatal-error msg . rest)
  `(PRIMop dead-end))

(define-macro (DEAD-END msg)
  (cond-expand
    (gambit
     `(PRIMop dead-end))
    (else
     `(error 'dead-end "dead-end"))))

(define-macro (Sdefine-record name . fields)
  (cond-expand
    (gambit
     (let* ((macro? (memq macro: fields))
            (fields (filter (lambda (x) (not (eq? x macro:))) fields))
            (make-func-name (string->symbol (string-append "make-" (symbol->string name))))
            (test-func-name (string->symbol (string-append (symbol->string name) "?")))
            (get-field-name
             (lambda (field)
               (if (pair? field) (car field) field)))
            (field-names (map get-field-name fields))
            (get-accessor-name
             (lambda (field)
               (if (pair? field)
                   (cadr field)
                   (string->symbol
                    (string-append (symbol->string name)
                                   ":"
                                   (symbol->string field))))))
            (get-mutator-name
             (lambda (field)
               (if (pair? field)
                   (caddr field)
                   (string->symbol
                    (string-append "set-"
                                   (symbol->string name)
                                   ":"
                                   (symbol->string field)))))))
       (if macro?
           `(begin
              ;; constructor macro
              (define-macro (,make-func-name . fields)
                `(vector ',',name ,@fields))

              (define-macro (,test-func-name obj)
                `(vector? ,obj))

              ;; accessors and mutators for each field
              ,@(apply append
                       (map
                        (lambda (field index)
                          (let ((accessor-name (get-accessor-name field))
                                (mutator-name (get-mutator-name field)))
                            `((define-macro (,accessor-name o)
                                `(let ((o ,o))
                                   (if (,',test-func-name o)
                                       (vector-ref o ,,index)
                                       (DEAD-END "record type error"))))
                              (define-macro (,mutator-name o v)
                                `(let ((o ,o) (v ,v))
                                   (if (,',test-func-name o)
                                       (vector-set! o ,,index v)
                                       (DEAD-END "record type error")))))))
                        fields
                        (iota (length fields) 1))))
           `(begin
              ;; constructor procedure
              (define (,make-func-name ,@field-names)
                (vector ',name ,@field-names))

              (define ,test-func-name vector?)

              ;; accessors and mutators for each field
              ,@(apply append
                       (map
                        (lambda (field index)
                          (let ((accessor-name (get-accessor-name field))
                                (mutator-name (get-mutator-name field)))
                            `((define (,accessor-name o)
                                (if (,test-func-name o)
                                    (vector-ref o ,index)
                                    (DEAD-END "record type error")))
                              (define (,mutator-name o v)
                                (if (,test-func-name o)
                                    (vector-set! o ,index v)
                                    (DEAD-END "record type error"))))))
                        fields
                        (iota (length fields) 1)))))))
    (else
     #f)))

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
