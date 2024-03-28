; Utilities

(define (pevery? pred? l) (set-bbv-version-limit! #f) 
  (let loop ((l l))
    (or (null? l) (and (pred? (Scar l)) (loop (Scdr l))))))

(define (some? pred? l) (set-bbv-version-limit! #f) 
  (let loop ((l l))
    (if (null? l) #f (or (pred? (Scar l)) (loop (Scdr l))))))

(define (map2 f l1 l2) (set-bbv-version-limit! #f) 
  (let loop ((l1 l1) (l2 l2))
    (if (pair? l1)
      (cons (f (Scar l1) (Scar l2)) (loop (Scdr l1) (Scdr l2)))
      '())))

(define (plast-pair l) (set-bbv-version-limit! #f) 
  (let loop ((l l))
    (let ((x (Scdr l))) (if (pair? x) (loop x) l))))

(define (proper-list? l) (set-bbv-version-limit! #f)  (list? l))

;------------------------------------------------------------------------------
;
; The partial evaluator.

(define (partial-evaluate proc args) (set-bbv-version-limit! #f) 
  (peval (alphatize proc '()) args))

(define (alphatize exp env) (set-bbv-version-limit! #f)  ; return a copy of 'exp' where each bound var has
  (define (alpha exp) (set-bbv-version-limit! #f)        ; been renamed (to prevent aliasing problems)
    (cond ((const-expr? exp)
           (quot (const-value exp)))
          ((symbol? exp)
           (let ((x (Sassq exp env))) (if x (Scdr x) exp)))
          ((or (eq? (Scar exp) 'if) (eq? (Scar exp) 'begin))
           (cons (Scar exp) (Smap2 alpha (Scdr exp))))
          ((or (eq? (Scar exp) 'let) (eq? (Scar exp) 'letrec))
           (let ((new-env (new-variables (Smap2 (lambda (x) (set-bbv-version-limit! #f)  (Scar x)) (Scadr exp)) env)))
             (list (Scar exp)
                   (Smap2 (lambda (x) (set-bbv-version-limit! #f) 
                          (list (Scdr (Sassq (Scar x) new-env))
                                (if (eq? (Scar exp) 'let)
                                  (alpha (Scadr x))
                                  (alphatize (Scadr x) new-env))))
                        (Scadr exp))
                   (alphatize (Scaddr exp) new-env))))
          ((eq? (Scar exp) 'lambda)
           (let ((new-env (new-variables (Scadr exp) env)))
             (list 'lambda
                   (Smap2 (lambda (x) (set-bbv-version-limit! #f)  (Scdr (Sassq x new-env))) (Scadr exp))
                   (alphatize (Scaddr exp) new-env))))
          (else
           (Smap2 alpha exp))))
  (alpha exp))

(define (const-expr? expr) (set-bbv-version-limit! #f)  ; is 'expr' a constant expression?
   (and (not (symbol? expr))
	(or (not (pair? expr))
	    (eq? (Scar expr) 'quote))))

(define (const-value expr) (set-bbv-version-limit! #f)  ; return the value of a constant expression
  (if (pair? expr) ; then it must be a quoted constant
    (Scadr expr)
    expr))

(define (quot val) (set-bbv-version-limit! #f)  ; make a quoted constant whose value is 'val'
  (list 'quote val))

(define (new-variables parms env) (set-bbv-version-limit! #f) 
  (Sappend (Smap2 (lambda (x) (set-bbv-version-limit! #f)  (cons x (new-variable x))) parms) env))

(define *current-num* 0)

(define (new-variable name) (set-bbv-version-limit! #f) 
  (set! *current-num* (+ *current-num* 1))
  (Sstring->symbol
    (Sstring-append (Ssymbol->string name)
                   "_"
                   (SFXnumber->string *current-num*))))

;------------------------------------------------------------------------------
;
; (peval proc args) will transform a procedure that is known to be called
; with constants as some of its arguments into a specialized procedure that
; is 'equivalent' but accepts only the non-constant parameters.  'proc' is the
; list representation of a lambda-expression and 'args' is a list of values,
; one for each parameter of the lambda-expression.  A special value (i.e.
; 'not-constant') is used to indicate an argument that is not a constant.
; The returned procedure is one that has as parameters the parameters of the
; original procedure which are NOT passed constants.  Constants will have been
; substituted for the constant parameters that are referenced in the body
; of the procedure.
;
; For example:
;
;   (peval
;     '(lambda (x y z) (set-bbv-version-limit! #f)  (f z x y)) ; the procedure
;     (list 1 not-constant #t))   ; the knowledge about x, y and z
;
; will return: (lambda (y) (set-bbv-version-limit! #f)  (f '#t '1 y))

(define (peval proc args) (set-bbv-version-limit! #f) 
  (simplify!
    (let ((parms (Scadr proc))  ; get the parameter list
          (body (Scaddr proc))) ; get the body of the procedure
      (list 'lambda
            (remove-constant parms args) ; remove the constant parameters
            (beta-subst ; in the body, replace variable refs to the constant
              body      ; parameters by the corresponding constant
              (map2 (lambda (x y) (set-bbv-version-limit! #f)  (if (not-constant? y)
				      '(()) (cons x (quot y))))
                    parms
                    args))))))

(define not-constant (list '?)) ; special value indicating non-constant parms.

(define (not-constant? x) (set-bbv-version-limit! #f)  (eq? x not-constant))

(define (remove-constant l a) (set-bbv-version-limit! #f)  ; remove from list 'l' all elements whose
  (cond ((null? l)            ; corresponding element in 'a' is a constant
         '())
        ((not-constant? (Scar a))
         (cons (Scar l) (remove-constant (Scdr l) (Scdr a))))
        (else
         (remove-constant (Scdr l) (Scdr a)))))

(define (extract-constant l a) (set-bbv-version-limit! #f)  ; extract from list 'l' all elements whose
  (cond ((null? l)             ; corresponding element in 'a' is a constant
         '())
        ((not-constant? (Scar a))
         (extract-constant (Scdr l) (Scdr a)))
        (else
         (cons (Scar l) (extract-constant (Scdr l) (Scdr a))))))

(define (beta-subst exp env) (set-bbv-version-limit! #f)  ; return a modified 'exp' where each var named in
  (define (bs exp) (set-bbv-version-limit! #f)            ; 'env' is replaced by the corresponding expr (it
    (cond ((const-expr? exp) ; is assumed that the code has been alphatized)
           (quot (const-value exp)))
          ((symbol? exp)
           (let ((x (Sassq exp env))) 
             (if x (Scdr x) exp)))
          ((or (eq? (Scar exp) 'if) (eq? (Scar exp) 'begin))
           (cons (Scar exp) (Smap2 bs (Scdr exp))))
          ((or (eq? (Scar exp) 'let) (eq? (Scar exp) 'letrec))
           (list (Scar exp)
                 (Smap2 (lambda (x) (set-bbv-version-limit! #f)  (list (Scar x) (bs (Scadr x)))) (Scadr exp))
                 (bs (Scaddr exp))))
          ((eq? (Scar exp) 'lambda)
           (list 'lambda
                 (Scadr exp)
                 (bs (Scaddr exp))))
          (else
           (Smap2 bs exp))))
  (bs exp))

;------------------------------------------------------------------------------
;
; The expression simplifier.

(define (simplify! exp) (set-bbv-version-limit! #f)      ; simplify the expression 'exp' destructively (it
                            ; is assumed that the code has been alphatized)
  (define (simp! where env) (set-bbv-version-limit! #f) 

    (define (s! where) (set-bbv-version-limit! #f) 
      (let ((exp (Scar where)))

        (cond ((const-expr? exp))  ; leave constants the way they are

              ((symbol? exp))      ; leave variable references the way they are

              ((eq? (Scar exp) 'if) ; dead code removal for conditionals
               (s! (Scdr exp))      ; simplify the predicate
               (if (const-expr? (Scadr exp)) ; is the predicate a constant?
                 (begin
                   (Sset-car! where
                     (if (Smemq (const-value (Scadr exp)) '(#f ())) ; false?
                       (if (SFX= (Slength exp) 3) ''() (Scadddr exp))
                       (Scaddr exp)))
                   (s! where))
                 (for-each! s! (Scddr exp)))) ; simplify consequent and alt.

              ((eq? (Scar exp) 'begin)
               (for-each! s! (Scdr exp))
               (let loop ((exps exp)) ; remove all useless expressions
                 (if (not (null? (Scddr exps))) ; not last expression?
                   (let ((x (Scadr exps)))
                     (loop (if (or (const-expr? x)
                                   (symbol? x)
                                   (and (pair? x) (eq? (Scar x) 'lambda)))
                             (begin (Sset-cdr! exps (Scddr exps)) exps)
                             (Scdr exps))))))
               (if (null? (Scddr exp)) ; only one expression in the begin?
                 (Sset-car! where (Scadr exp))))

              ((or (eq? (Scar exp) 'let) (eq? (Scar exp) 'letrec))
               (let ((new-env (cons exp env)))
                 (define (keep i) (set-bbv-version-limit! #f) 
                   (if (SFX>= i (Slength (Scadar where)))
                     '()
                     (let* ((var (Scar (list-ref (Scadar where) i)))
                            (val (Scadr (Sassq var (Scadar where))))
                            (refs (ref-count (Scar where) var))
                            (self-refs (ref-count val var))
                            (total-refs (SFX- (Scar refs) (Scar self-refs)))
                            (oper-refs (SFX- (Scadr refs) (Scadr self-refs))))
                       (cond ((SFX= total-refs 0)
                              (keep (SFX+ i 1)))
                             ((or (const-expr? val)
                                  (symbol? val)
                                  (and (pair? val)
                                       (eq? (Scar val) 'lambda)
                                       (SFX= total-refs 1)
                                       (SFX= oper-refs 1)
                                       (SFX= (Scar self-refs) 0))
                                  (and (Scaddr refs)
                                       (= total-refs 1)))
                              (Sset-car! where
                                (beta-subst (Scar where)
                                            (list (cons var val))))
                              (keep (SFX+ i 1)))
                             (else
                              (cons var (keep (SFX+ i 1))))))))
                 (simp! (Scddr exp) new-env)
                 (for-each! (lambda (x) (set-bbv-version-limit! #f)  (simp! (cdar x) new-env)) (Scadr exp))
                 (let ((to-keep (keep 0)))
                   (if (SFX< (Slength to-keep) (Slength (Scadar where)))
                     (begin
                       (if (null? to-keep)
                         (Sset-car! where (caddar where))
                         (Sset-car! (cdar where)
                           (Smap2 (lambda (v) (set-bbv-version-limit! #f)  (Sassq v (Scadar where))) to-keep)))
                       (s! where))
                     (if (null? to-keep)
                       (Sset-car! where (caddar where)))))))

              ((eq? (Scar exp) 'lambda)
               (simp! (Scddr exp) (cons exp env)))

              (else
               (for-each! s! exp)
               (cond ((symbol? (Scar exp)) ; is the operator position a var ref?
                      (let ((frame (binding-frame (Scar exp) env)))
                        (if frame ; is it a bound variable?
                          (let ((proc (bound-expr (Scar exp) frame)))
                            (if (and (pair? proc)
                                     (eq? (Scar proc) 'lambda)
                                     (some? const-expr? (Scdr exp)))
                              (let* ((args (arg-pattern (Scdr exp)))
                                     (new-proc (peval proc args))
                                     (new-args (remove-constant (Scdr exp)
								args)))
                                (Sset-car! where
                                  (cons (add-binding new-proc frame (Scar exp))
                                        new-args)))))
                          (Sset-car! where
                            (constant-fold-global (Scar exp) (Scdr exp))))))
                     ((not (pair? (Scar exp))))
                     ((eq? (caar exp) 'lambda)
                      (Sset-car! where
                        (list 'let
                              (map2 list (Scadar exp) (Scdr exp))
                              (caddar exp)))
                      (s! where)))))))

    (s! where))

  (define (remove-empty-calls! where env) (set-bbv-version-limit! #f) 

    (define (rec! where) (set-bbv-version-limit! #f) 
      (let ((exp (Scar where)))

        (cond ((const-expr? exp))
              ((symbol? exp))
              ((eq? (Scar exp) 'if)
               (rec! (Scdr exp))
               (rec! (Scddr exp))
               (rec! (cdddr exp)))
              ((eq? (Scar exp) 'begin)
               (for-each! rec! (Scdr exp)))
              ((or (eq? (Scar exp) 'let) (eq? (Scar exp) 'letrec))
               (let ((new-env (cons exp env)))
                 (remove-empty-calls! (Scddr exp) new-env)
                 (for-each! (lambda (x) (set-bbv-version-limit! #f)  (remove-empty-calls! (cdar x) new-env))
                            (Scadr exp))))
              ((eq? (Scar exp) 'lambda)
               (rec! (Scddr exp)))
              (else
               (for-each! rec! (Scdr exp))
               (if (and (null? (Scdr exp)) (symbol? (Scar exp)))
                 (let ((frame (binding-frame (Scar exp) env)))
                   (if frame ; is it a bound variable?
                     (let ((proc (bound-expr (Scar exp) frame)))
                       (if (and (pair? proc)
                                (eq? (Scar proc) 'lambda))
                         (begin
                           (set! changed? #t)
                           (Sset-car! where (Scaddr proc))))))))))))

    (rec! where))

  (define changed? #f)

  (let ((x (list exp)))
    (let loop ()
      (set! changed? #f)
      (simp! x '())
      (remove-empty-calls! x '())
      (if changed? (loop) (Scar x)))))

(define (ref-count exp var) (set-bbv-version-limit! #f)  ; compute how many references to variable 'var'
  (let ((total 0)           ; are contained in 'exp'
        (oper 0)
        (always-evaled #t))
    (define (rc exp ae) (set-bbv-version-limit! #f) 
      (cond ((const-expr? exp))
            ((symbol? exp)
             (if (eq? exp var)
               (begin
                 (set! total (+ total 1))
                 (set! always-evaled (and ae always-evaled)))))
            ((eq? (Scar exp) 'if)
             (rc (Scadr exp) ae)
             (Sfor-each2 (lambda (x) (set-bbv-version-limit! #f)  (rc x #f)) (Scddr exp)))
            ((eq? (Scar exp) 'begin)
             (Sfor-each2 (lambda (x) (set-bbv-version-limit! #f)  (rc x ae)) (Scdr exp)))
            ((or (eq? (Scar exp) 'let) (eq? (Scar exp) 'letrec))
             (Sfor-each2 (lambda (x) (set-bbv-version-limit! #f)  (rc (Scadr x) ae)) (Scadr exp))
             (rc (Scaddr exp) ae))
            ((eq? (Scar exp) 'lambda)
             (rc (Scaddr exp) #f))
            (else
             (Sfor-each2 (lambda (x) (set-bbv-version-limit! #f)  (rc x ae)) exp)
             (if (symbol? (Scar exp))
               (if (eq? (Scar exp) var) (set! oper (SFX+ oper 1)))))))
    (rc exp #t)
    (list total oper always-evaled)))

(define (binding-frame var env) (set-bbv-version-limit! #f) 
  (cond ((null? env) #f)
        ((or (eq? (caar env) 'let) (eq? (caar env) 'letrec))
         (if (Sassq var (Scadar env)) (Scar env) (binding-frame var (Scdr env))))
        ((eq? (caar env) 'lambda)
         (if (Smemq var (Scadar env)) (Scar env) (binding-frame var (Scdr env))))
        (else
         (error '() '() "ill-formed environment"))))

(define (bound-expr var frame) (set-bbv-version-limit! #f) 
  (cond ((or (eq? (Scar frame) 'let) (eq? (Scar frame) 'letrec))
         (Scadr (Sassq var (Scadr frame))))
        ((eq? (Scar frame) 'lambda)
         not-constant)
        (else
         (error '() '() "ill-formed frame"))))

(define (add-binding val frame name) (set-bbv-version-limit! #f) 
  (define (find-val val bindings) (set-bbv-version-limit! #f) 
    (cond ((null? bindings) #f)
          ((Sequal? val (Scadar bindings)) ; *kludge* equal? is not exactly what
           (caar bindings))              ; we want...
          (else
           (find-val val (Scdr bindings)))))
  (or (find-val val (Scadr frame))
      (let ((var (new-variable name)))
        (Sset-cdr! (plast-pair (Scadr frame)) (list (list var val)))
        var)))

(define (for-each! proc! l) (set-bbv-version-limit! #f)  ; call proc! on each CONS CELL in the list 'l'
  (if (not (null? l))
    (begin (proc! l) (for-each! proc! (Scdr l)))))

(define (arg-pattern exps) (set-bbv-version-limit! #f)  ; return the argument pattern (i.e. the list of
  (if (null? exps)         ; constants in 'exps' but with the not-constant
    '()                    ; value wherever the corresponding expression in
    (cons (if (const-expr? (Scar exps)) ; 'exps' is not a constant)
            (const-value (Scar exps))
            not-constant)
          (arg-pattern (Scdr exps)))))

;------------------------------------------------------------------------------
;
; Knowledge about primitive procedures.

(define *primitives*
  (list
    (cons 'car (lambda (args) (set-bbv-version-limit! #f) 
                 (and (SFX= (Slength args) 1)
                      (pair? (Scar args))
                      (quot (Scar (Scar args))))))
    (cons 'cdr (lambda (args) (set-bbv-version-limit! #f) 
                 (and (SFX= (Slength args) 1)
                      (pair? (Scar args))
                      (quot (Scdr (Scar args))))))
    (cons '+ (lambda (args) (set-bbv-version-limit! #f) 
               (and (pevery? number? args) (quot (apply + args)))))
    (cons '* (lambda (args) (set-bbv-version-limit! #f) 
               (and (pevery? number? args) (quot (apply * args)))))
    (cons '- (lambda (args) (set-bbv-version-limit! #f) 
               (and (SFX> (Slength args) 0)
                    (pevery? number? args)
                    (quot (apply - args)))))
    (cons '/ (lambda (args) (set-bbv-version-limit! #f) 
               (and (SFX> (Slength args) 1)
                    (pevery? number? args)
                    (quot (apply / args)))))
    (cons '< (lambda (args) (set-bbv-version-limit! #f) 
               (and (SFX= (Slength args) 2)
                    (pevery? number? args)
                    (quot (< (Scar args) (Scadr args))))))
    (cons '= (lambda (args) (set-bbv-version-limit! #f) 
               (and (SFX= (Slength args) 2)
                    (pevery? number? args)
                    (quot (= (Scar args) (Scadr args))))))
    (cons '> (lambda (args) (set-bbv-version-limit! #f) 
               (and (SFX= (Slength args) 2)
                    (pevery? number? args)
                    (quot (> (Scar args) (Scadr args))))))
    (cons 'eq? (lambda (args) (set-bbv-version-limit! #f) 
                 (and (SFX= (Slength args) 2)
                      (quot (eq? (Scar args) (Scadr args))))))
    (cons 'not (lambda (args) (set-bbv-version-limit! #f) 
                 (and (SFX= (Slength args) 1)
                      (quot (not (Scar args))))))
    (cons 'null? (lambda (args) (set-bbv-version-limit! #f) 
                   (and (SFX= (Slength args) 1)
                        (quot (null? (Scar args))))))
    (cons 'pair? (lambda (args) (set-bbv-version-limit! #f) 
                   (and (SFX= (Slength args) 1)
                        (quot (pair? (Scar args))))))
    (cons 'symbol? (lambda (args) (set-bbv-version-limit! #f) 
                     (and (SFX= (Slength args) 1)
                          (quot (symbol? (Scar args))))))
    (cons 'length (lambda (args) (set-bbv-version-limit! #f) 
                    (and (SFX= (Slength args) 1)
                         (proper-list? (Scar args))
                         (quot (Slength (Scar args))))))
  )
)

(define (reduce-global name args) (set-bbv-version-limit! #f) 
  (let ((x (Sassq name *primitives*)))
    (and x ((Scdr x) args))))

(define (constant-fold-global name exprs) (set-bbv-version-limit! #f) 

  (define (flatten args op) (set-bbv-version-limit! #f) 
    (cond ((null? args)
           '())
          ((and (pair? (Scar args)) (eq? (caar args) op))
           (append (flatten (cdar args) op) (flatten (Scdr args) op)))
          (else
           (cons (Scar args) (flatten (Scdr args) op)))))

  (let ((args (if (or (eq? name '+) (eq? name '*)) ; associative ops
                (flatten exprs name)
                exprs)))
    (or (and (pevery? const-expr? args)
             (reduce-global name (Smap2 const-value args)))
        (let ((pattern (arg-pattern args)))
          (let ((non-const (remove-constant args pattern))
                (const (Smap2 const-value (extract-constant args pattern))))
            (cond ((eq? name '+) ; + is commutative
                   (let ((x (reduce-global '+ const)))
                     (if x
                       (let ((y (const-value x)))
                         (cons '+
                               (if (= y 0) non-const (cons x non-const))))
                       (cons name args))))
                  ((eq? name '*) ; * is commutative
                   (let ((x (reduce-global '* const)))
                     (if x
                       (let ((y (const-value x)))
                         (cons '*
                               (if (= y 1) non-const (cons x non-const))))
                       (cons name args))))
                  ((eq? name 'cons)
                   (cond ((and (const-expr? (Scadr args))
                               (null? (const-value (Scadr args))))
                          (list 'list (Scar args)))
                         ((and (pair? (Scadr args))
                               (eq? (Scar (Scadr args)) 'list))
                          (cons 'list (cons (Scar args) (Scdr (Scadr args)))))
                         (else
                          (cons name args))))
                  (else
                   (cons name args))))))))

;------------------------------------------------------------------------------
;
; Examples:

(define (my-try proc args) (set-bbv-version-limit! #f) 
  (partial-evaluate proc args))

; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

(define example1
  '(lambda (a b c)
     (if (null? a) b (+ (car a) c))))

;(my-try example1 (list '(10 11) not-constant '1))

; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

(define example2
  '(lambda (x y)
     (let ((q (lambda (a b) (if (< a 0) b (- 10 b)))))
       (if (< x 0) (q (- y) (- x)) (q y x)))))

;(my-try example2 (list not-constant '1))

; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

(define example3
  '(lambda (l n)
     (letrec ((add-list
               (lambda (l n)
                 (if (null? l)
                   '()
                   (cons (+ (car l) n) (add-list (cdr l) n))))))
       (add-list l n))))

;(my-try example3 (list not-constant '1))

;(my-try example3 (list '(1 2 3) not-constant))

; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

(define example4
  '(lambda (exp env)
     (letrec ((eval
               (lambda (exp env)
                 (letrec ((eval-list
                            (lambda (l env)
                              (if (null? l)
                                '()
                                (cons (eval (car l) env)
                                      (eval-list (cdr l) env))))))
                   (if (symbol? exp) (lookup exp env)
                     (if (not (pair? exp)) exp
                       (if (eq? (car exp) 'quote) (car (cdr exp))
                         (apply (eval (car exp) env)
                                (eval-list (cdr exp) env)))))))))
       (eval exp env))))

;(my-try example4 (list 'x not-constant))

;(my-try example4 (list '(f 1 2 3) not-constant))

; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

(define example5
  '(lambda (a b)
     (letrec ((funct
               (lambda (x)
                 (+ x b (if (< x 1) 0 (funct (- x 1)))))))
       (funct a))))

;(my-try example5 (list '5 not-constant))

; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

(define example6
  '(lambda ()
     (letrec ((fib
               (lambda (x)
                 (if (< x 2) x (+ (fib (- x 1)) (fib (- x 2)))))))
       (fib 10))))

;(my-try example6 '())

; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

(define example7
  '(lambda (input)
     (letrec ((copy (lambda (in)
		      (if (pair? in)
                        (cons (copy (car in))
			      (copy (cdr in)))
			in))))
       (copy input))))

;(my-try example7
;        (list '(a b c d e f g h i j k l m n o p q r s t u v w x y z)))

; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

(define example8
  '(lambda (input)
     (letrec ((reverse (lambda (in result)
			 (if (pair? in)
			   (reverse (cdr in) (cons (car in) result))
			   result))))
       (reverse input '()))))

;(my-try example8
;        (list '(a b c d e f g h i j k l m n o p q r s t u v w x y z)))

; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

(define-keys (run !key (n (unknown 3000 1)))
   (let loop ((n n) (result #f))
      (if (SFX> n 0)
	  (begin
	     (set! *current-num* 0)
	     (loop (SFX- n 1)
		(list (my-try example1 (list '(10 11) not-constant '1))
		   (my-try example2 (list not-constant '1))
		   (my-try example3 (list not-constant '1))
		   (my-try example3 (list '(1 2 3) not-constant))
		   (my-try example4 (list 'x not-constant))
		   (my-try example4 (list '(f 1 2 3) not-constant))
		   (my-try example5 (list '5 not-constant))
		   (my-try example6 '())
		   (my-try example7
		      (list '(a b c d e f g h i j k l m n o p q r s t u v w x y z)))
		   (my-try example8
		      (list '(a b c d e f g h i j k l m n o p q r s t u v w x y z))))))
	  result)))

(define expected-result
   '((lambda (b_2) (quote 11)) (lambda (x_4) (if (< x_4 (quote 0)) (- x_4) (- (quote 10) x_4))) (lambda (l_11) (letrec ((add-list_13_16 (lambda (l_14) (if (null? l_14) (quote ()) (cons (+ (quote 1) (car l_14)) (add-list_13_16 (cdr l_14))))))) (add-list_13_16 l_11))) (lambda (n_18) (list (+ (quote 1) n_18) (+ (quote 2) n_18) (+ (quote 3) n_18))) (lambda (env_27) (lookup (quote x) env_27)) (lambda (env_36) (apply (lookup (quote f) env_36) (list (quote 1) (quote 2) (quote 3)))) (lambda (b_53) (+ (quote 15) b_53 b_53 b_53 b_53 b_53 b_53)) (lambda () (quote 55)) (lambda () (list (quote a) (quote b) (quote c) (quote d) (quote e) (quote f) (quote g) (quote h) (quote i) (quote j) (quote k) (quote l) (quote m) (quote n) (quote o) (quote p) (quote q) (quote r) (quote s) (quote t) (quote u) (quote v) (quote w) (quote x) (quote y) (quote z))) (lambda () (list (quote z) (quote y) (quote x) (quote w) (quote v) (quote u) (quote t) (quote s) (quote r) (quote q) (quote p) (quote o) (quote n) (quote m) (quote l) (quote k) (quote j) (quote i) (quote h) (quote g) (quote f) (quote e) (quote d) (quote c) (quote b) (quote a)))))

(define (check result) (set-bbv-version-limit! #f) 
   (equal? result expected-result))
