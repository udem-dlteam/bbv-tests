;;; SCHEME -- A Scheme interpreter evaluating a sort, written by Marc Feeley.

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define (scheme-eval expr)
  (let ((code (scheme-comp expr scheme-global-environment)))
    (code #f)))

(define scheme-global-environment
  (cons '()   ; environment chain
        '())) ; macros

(define (scheme-add-macro name proc)
  (Sset-cdr! scheme-global-environment
    (cons (cons name proc) (Scdr scheme-global-environment)))
  name)

(define (scheme-error msg . args)
  (fatal-error msg args))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define (lst->vector l)
  (let* ((n (Slength l))
         (v (make-vector n)))
    (let loop ((l l) (i 0))
      (if (pair? l)
        (begin
          (Svector-set! v i (Scar l))
          (loop (Scdr l) (SFX+ i 1)))
        v))))

(define (vector->lst v)
  (let loop ((l '()) (i (SFX- (Svector-length v) 1)))
    (if (SFX< i 0)
      l
      (loop (cons (Svector-ref v i) l) (SFX- i 1)))))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define scheme-syntactic-keywords
  '(quote quasiquote unquote unquote-splicing
    lambda if set! cond => else and or
    case let let* letrec begin do define
    define-macro))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define (push-frame frame env)
  (if (null? frame)
    env
    (cons (cons (Scar env) frame) (Scdr env))))

(define (lookup-var name env)
  (let loop1 ((chain (Scar env)) (up 0))
    (if (null? chain)
      name
      (let loop2 ((chain chain)
                  (up up)
                  (frame (Scdr chain))
                  (over 1))
        (cond ((null? frame)
               (loop1 (Scar chain) (SFX+ up 1)))
              ((eq? (Scar frame) name)
               (cons up over))
              (else
               (loop2 chain up (Scdr frame) (SFX+ over 1))))))))

(define (macro? name env)
  (Sassq name (Scdr env)))

(define (push-macro name proc env)
  (cons (Scar env) (cons (cons name proc) (Scdr env))))

(define (lookup-macro name env)
  (Scdr (Sassq name (Scdr env))))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define (variable x)
  (if (not (symbol? x))
    (scheme-error "Identifier expected" x))
  (if (Smemq x scheme-syntactic-keywords)
    (scheme-error "Variable name can not be a syntactic keyword" x)))

(define (shape form n)
  (let loop ((form form) (n n) (l form))
    (cond ((SFX<= n 0))
          ((pair? l)
           (loop form (SFX- n 1) (Scdr l)))
          (else
           (scheme-error "Ill-constructed form" form)))))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define (macro-expand expr env)
  (apply (lookup-macro (Scar expr) env) (Scdr expr)))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define (comp-var expr env)
  (variable expr)
  (gen-var-ref (lookup-var expr env)))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define (comp-self-eval expr env)
  (gen-cst expr))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define (comp-quote expr env)
  (shape expr 2)
  (gen-cst (Scadr expr)))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define (comp-quasiquote expr env)
  (comp-quasiquotation (Scadr expr) 1 env))

(define (comp-quasiquotation form level env)
  (cond ((SFX= level 0)
         (scheme-comp form env))
        ((pair? form)
         (cond
           ((eq? (Scar form) 'quasiquote)
            (comp-quasiquotation-list form (SFX+ level 1) env))
           ((eq? (Scar form) 'unquote)
            (if (SFX= level 1)
              (scheme-comp (Scadr form) env)
              (comp-quasiquotation-list form (SFX- level 1) env)))
           ((eq? (Scar form) 'unquote-splicing)
            (if (SFX= level 1)
              (scheme-error "Ill-placed 'unquote-splicing'" form))
            (comp-quasiquotation-list form (SFX- level 1) env))
           (else
            (comp-quasiquotation-list form level env))))
        ((vector? form)
         (gen-vector-form
           (comp-quasiquotation-list (vector->lst form) level env)))
        (else
         (gen-cst form))))

(define (comp-quasiquotation-list l level env)
  (if (pair? l)
    (let ((first (Scar l)))
      (if (SFX= level 1)
        (if (unquote-splicing? first)
          (begin
            (shape first 2)
            (gen-append-form (scheme-comp (Scadr first) env)
                             (comp-quasiquotation (Scdr l) 1 env)))
          (gen-cons-form (comp-quasiquotation first level env)
                         (comp-quasiquotation (Scdr l) level env)))
        (gen-cons-form (comp-quasiquotation first level env)
                       (comp-quasiquotation (Scdr l) level env))))
    (comp-quasiquotation l level env)))

(define (unquote-splicing? x)
  (if (pair? x)
    (if (eq? (Scar x) 'unquote-splicing) #t #f)
    #f))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define (comp-unquote expr env)
  (scheme-error "Ill-placed 'unquote'" expr))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define (comp-unquote-splicing expr env)
  (scheme-error "Ill-placed 'unquote-splicing'" expr))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define (comp-set! expr env)
  (shape expr 3)
  (variable (Scadr expr))
  (gen-var-set (lookup-var (Scadr expr) env) (scheme-comp (Scaddr expr) env)))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define (comp-lambda expr env)
  (shape expr 3)
  (let ((parms (Scadr expr)))
    (let ((frame (parms->frame parms)))
      (let ((nb-vars (Slength frame))
            (code (comp-body (Scddr expr) (push-frame frame env))))
        (if (rest-param? parms)
          (gen-lambda-rest nb-vars code)
          (gen-lambda nb-vars code))))))

(define (parms->frame parms)
  (cond ((null? parms)
         '())
        ((pair? parms)
         (let ((x (Scar parms)))
           (variable x)
           (cons x (parms->frame (Scdr parms)))))
        (else
         (variable parms)
         (list parms))))

(define (rest-param? parms)
  (cond ((pair? parms)
         (rest-param? (Scdr parms)))
        ((null? parms)
         #f)
        (else
         #t)))

(define (comp-body body env)

  (define (letrec-defines vars vals body env)
    (if (pair? body)

      (let ((expr (Scar body)))
        (cond ((not (pair? expr))
               (letrec-defines* vars vals body env))
              ((macro? (Scar expr) env)
               (letrec-defines vars
                               vals
                               (cons (macro-expand expr env) (Scdr body))
                               env))
              (else
               (cond
                 ((eq? (Scar expr) 'begin)
                  (letrec-defines vars
                                  vals
                                  (append (Scdr expr) (Scdr body))
                                  env))
                 ((eq? (Scar expr) 'define)
                  (let ((x (definition-name expr)))
                    (variable x)
                    (letrec-defines (cons x vars)
                                    (cons (definition-value expr) vals)
                                    (Scdr body)
                                    env)))
                 ((eq? (Scar expr) 'define-macro)
                  (let ((x (definition-name expr)))
                    (letrec-defines vars
                                    vals
                                    (Scdr body)
                                    (push-macro
                                      x
                                      (scheme-eval (definition-value expr))
                                      env))))
                 (else
                  (letrec-defines* vars vals body env))))))

      (scheme-error "Body must contain at least one evaluable expression")))

  (define (letrec-defines* vars vals body env)
    (if (null? vars)
      (comp-sequence body env)
      (comp-letrec-aux vars vals body env)))

  (letrec-defines '() '() body env))

(define (definition-name expr)
  (shape expr 3)
  (let ((pattern (Scadr expr)))
    (let ((name (if (pair? pattern) (Scar pattern) pattern)))
      (if (not (symbol? name))
        (scheme-error "Identifier expected" name))
      name)))

(define (definition-value expr)
  (let ((pattern (Scadr expr)))
    (if (pair? pattern)
      (cons 'lambda (cons (Scdr pattern) (Scddr expr)))
      (Scaddr expr))))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define (comp-if expr env)
  (shape expr 3)
  (let ((code1 (scheme-comp (Scadr expr) env))
        (code2 (scheme-comp (Scaddr expr) env)))
    (if (pair? (cdddr expr))
      (gen-if code1 code2 (scheme-comp (cadddr expr) env))
      (gen-when code1 code2))))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define (comp-cond expr env)
  (comp-cond-aux (Scdr expr) env))

(define (comp-cond-aux clauses env)
  (if (pair? clauses)
    (let ((clause (Scar clauses)))
      (shape clause 1)
      (cond ((eq? (Scar clause) 'else)
             (shape clause 2)
             (comp-sequence (Scdr clause) env))
            ((not (pair? (Scdr clause)))
             (gen-or (scheme-comp (Scar clause) env)
                     (comp-cond-aux (Scdr clauses) env)))
            ((eq? (Scadr clause) '=>)
             (shape clause 3)
             (gen-cond-send (scheme-comp (Scar clause) env)
                            (scheme-comp (Scaddr clause) env)
                            (comp-cond-aux (Scdr clauses) env)))
            (else
             (gen-if (scheme-comp (Scar clause) env)
                     (comp-sequence (Scdr clause) env)
                     (comp-cond-aux (Scdr clauses) env)))))
    (gen-cst '())))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define (comp-and expr env)
  (let ((rest (Scdr expr)))
    (if (pair? rest) (comp-and-aux rest env) (gen-cst #t))))

(define (comp-and-aux l env)
  (let ((code (scheme-comp (Scar l) env))
        (rest (Scdr l)))
    (if (pair? rest) (gen-and code (comp-and-aux rest env)) code)))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define (comp-or expr env)
  (let ((rest (Scdr expr)))
    (if (pair? rest) (comp-or-aux rest env) (gen-cst #f))))

(define (comp-or-aux l env)
  (let ((code (scheme-comp (Scar l) env))
        (rest (Scdr l)))
    (if (pair? rest) (gen-or code (comp-or-aux rest env)) code)))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define (comp-case expr env)
  (shape expr 3)
  (gen-case (scheme-comp (Scadr expr) env)
            (comp-case-aux (Scddr expr) env)))

(define (comp-case-aux clauses env)
  (if (pair? clauses)
    (let ((clause (Scar clauses)))
      (shape clause 2)
      (if (eq? (Scar clause) 'else)
        (gen-case-else (comp-sequence (Scdr clause) env))
        (gen-case-clause (Scar clause)
                         (comp-sequence (Scdr clause) env)
                         (comp-case-aux (Scdr clauses) env))))
    (gen-case-else (gen-cst '()))))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define (comp-let expr env)
  (shape expr 3)
  (let ((x (Scadr expr)))
    (cond ((symbol? x)
           (shape expr 4)
           (let ((y (Scaddr expr)))
             (let ((proc (cons 'lambda (cons (bindings->vars y) (Scdddr expr)))))
               (scheme-comp (cons (list 'letrec (list (list x proc)) x)
                                  (bindings->vals y))
                            env))))
          ((pair? x)
           (scheme-comp (cons (cons 'lambda (cons (bindings->vars x) (Scddr expr)))
                              (bindings->vals x))
                        env))
          (else
           (comp-body (Scddr expr) env)))))

(define (bindings->vars bindings)
  (if (pair? bindings)
    (let ((binding (Scar bindings)))
      (shape binding 2)
      (let ((x (Scar binding)))
        (variable x)
        (cons x (bindings->vars (Scdr bindings)))))
    '()))

(define (bindings->vals bindings)
  (if (pair? bindings)
    (let ((binding (Scar bindings)))
      (cons (Scadr binding) (bindings->vals (Scdr bindings))))
    '()))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define (comp-let* expr env)
  (shape expr 3)
  (let ((bindings (Scadr expr)))
    (if (pair? bindings)
      (scheme-comp (list 'let
                         (list (Scar bindings))
                         (cons 'let* (cons (Scdr bindings) (Scddr expr))))
                   env)
      (comp-body (Scddr expr) env))))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define (comp-letrec expr env)
  (shape expr 3)
  (let ((bindings (Scadr expr)))
    (comp-letrec-aux (bindings->vars bindings)
                     (bindings->vals bindings)
                     (Scddr expr)
                     env)))

(define (comp-letrec-aux vars vals body env)
  (if (pair? vars)
    (let ((new-env (push-frame vars env)))
      (gen-letrec (comp-vals vals new-env)
                  (comp-body body new-env)))
    (comp-body body env)))

(define (comp-vals l env)
  (if (pair? l)
    (cons (scheme-comp (Scar l) env) (comp-vals (Scdr l) env))
    '()))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define (comp-begin expr env)
  (shape expr 2)
  (comp-sequence (Scdr expr) env))

(define (comp-sequence exprs env)
  (if (pair? exprs)
    (comp-sequence-aux exprs env)
    (gen-cst '())))

(define (comp-sequence-aux exprs env)
  (let ((code (scheme-comp (Scar exprs) env))
        (rest (Scdr exprs)))
    (if (pair? rest) (gen-sequence code (comp-sequence-aux rest env)) code)))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define (comp-do expr env)
  (shape expr 3)
  (let ((bindings (Scadr expr))
        (exit (Scaddr expr)))
    (shape exit 1)
    (let* ((vars (bindings->vars bindings))
           (new-env1 (push-frame '(#f) env))
           (new-env2 (push-frame vars new-env1)))
      (gen-letrec
        (list
          (gen-lambda
            (Slength vars)
            (gen-if
              (scheme-comp (Scar exit) new-env2)
              (comp-sequence (Scdr exit) new-env2)
              (gen-sequence
                (comp-sequence (Scdddr expr) new-env2)
                (gen-combination
                  (gen-var-ref '(1 . 1))
                  (comp-vals (bindings->steps bindings) new-env2))))))
        (gen-combination
          (gen-var-ref '(0 . 1))
          (comp-vals (bindings->vals bindings) new-env1))))))

(define (bindings->steps bindings)
  (if (pair? bindings)
    (let ((binding (Scar bindings)))
      (cons (if (pair? (Scddr binding)) (Scaddr binding) (Scar binding))
            (bindings->steps (Scdr bindings))))
    '()))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define (comp-define expr env)
  (shape expr 3)
  (let ((pattern (Scadr expr)))
    (let ((x (if (pair? pattern) (Scar pattern) pattern)))
      (variable x)
      (gen-sequence
        (gen-var-set (lookup-var x env)
                     (scheme-comp (if (pair? pattern)
                                    (cons 'lambda (cons (Scdr pattern) (Scddr expr)))
                                    (Scaddr expr))
                                  env))
        (gen-cst x)))))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define (comp-define-macro expr env)
  (let ((x (definition-name expr)))
    (gen-macro x (scheme-eval (definition-value expr)))))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define (comp-combination expr env)
  (gen-combination (scheme-comp (Scar expr) env) (comp-vals (Scdr expr) env)))

;------------------------------------------------------------------------------

(define (gen-var-ref var)
  (if (pair? var)
    (gen-rte-ref (Scar var) (Scdr var))
    (gen-glo-ref (scheme-global-var var))))

(define (gen-rte-ref up over)
  (case up
    ((0)  (gen-slot-ref-0 over))
    ((1)  (gen-slot-ref-1 over))
    (else (gen-slot-ref-up-2 (gen-rte-ref (- up 2) over)))))

(define (gen-slot-ref-0 i)
  (case i
    ((0)  (lambda (rte) (Svector-ref rte 0)))
    ((1)  (lambda (rte) (Svector-ref rte 1)))
    ((2)  (lambda (rte) (Svector-ref rte 2)))
    ((3)  (lambda (rte) (Svector-ref rte 3)))
    (else (lambda (rte) (Svector-ref rte i)))))

(define (gen-slot-ref-1 i)
  (case i
    ((0)  (lambda (rte) (Svector-ref (Svector-ref rte 0) 0)))
    ((1)  (lambda (rte) (Svector-ref (Svector-ref rte 0) 1)))
    ((2)  (lambda (rte) (Svector-ref (Svector-ref rte 0) 2)))
    ((3)  (lambda (rte) (Svector-ref (Svector-ref rte 0) 3)))
    (else (lambda (rte) (Svector-ref (Svector-ref rte 0) i)))))

(define (gen-slot-ref-up-2 code)
  (lambda (rte) (code (Svector-ref (Svector-ref rte 0) 0))))

(define (gen-glo-ref i)
  (lambda (rte) (scheme-global-var-ref i)))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define (gen-cst val)
  (case val
    ((()) (lambda (rte) '()))
    ((#f) (lambda (rte) #f))
    ((#t) (lambda (rte) #t))
    ((-2) (lambda (rte) -2))
    ((-1) (lambda (rte) -1))
    ((0)  (lambda (rte) 0))
    ((1)  (lambda (rte) 1))
    ((2)  (lambda (rte) 2))
    (else (lambda (rte) val))))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define (gen-append-form code1 code2)
  (lambda (rte) (append (code1 rte) (code2 rte))))

(define (gen-cons-form code1 code2)
  (lambda (rte) (cons (code1 rte) (code2 rte))))

(define (gen-vector-form code)
  (lambda (rte) (lst->vector (code rte))))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define (gen-var-set var code)
  (if (pair? var)
    (gen-rte-set (Scar var) (Scdr var) code)
    (gen-glo-set (scheme-global-var var) code)))

(define (gen-rte-set up over code)
  (case up
    ((0)  (gen-slot-set-0 over code))
    ((1)  (gen-slot-set-1 over code))
    (else (gen-slot-set-n (gen-rte-ref (SFX- up 2) 0) over code))))

(define (gen-slot-set-0 i code)
  (case i
    ((0)  (lambda (rte) (Svector-set! rte 0 (code rte))))
    ((1)  (lambda (rte) (Svector-set! rte 1 (code rte))))
    ((2)  (lambda (rte) (Svector-set! rte 2 (code rte))))
    ((3)  (lambda (rte) (Svector-set! rte 3 (code rte))))
    (else (lambda (rte) (Svector-set! rte i (code rte))))))

(define (gen-slot-set-1 i code)
  (case i
    ((0)  (lambda (rte) (Svector-set! (Svector-ref rte 0) 0 (code rte))))
    ((1)  (lambda (rte) (Svector-set! (Svector-ref rte 0) 1 (code rte))))
    ((2)  (lambda (rte) (Svector-set! (Svector-ref rte 0) 2 (code rte))))
    ((3)  (lambda (rte) (Svector-set! (Svector-ref rte 0) 3 (code rte))))
    (else (lambda (rte) (Svector-set! (Svector-ref rte 0) i (code rte))))))

(define (gen-slot-set-n up i code)
  (case i
    ((0)  (lambda (rte) (Svector-set! (up (Svector-ref rte 0)) 0 (code rte))))
    ((1)  (lambda (rte) (Svector-set! (up (Svector-ref rte 0)) 1 (code rte))))
    ((2)  (lambda (rte) (Svector-set! (up (Svector-ref rte 0)) 2 (code rte))))
    ((3)  (lambda (rte) (Svector-set! (up (Svector-ref rte 0)) 3 (code rte))))
    (else (lambda (rte) (Svector-set! (up (Svector-ref rte 0)) i (code rte))))))

(define (gen-glo-set i code)
  (lambda (rte) (scheme-global-var-set! i (code rte))))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define (gen-lambda-rest nb-vars body)
  (case nb-vars
    ((1)  (gen-lambda-1-rest body))
    ((2)  (gen-lambda-2-rest body))
    ((3)  (gen-lambda-3-rest body))
    (else (gen-lambda-n-rest nb-vars body))))

(define (gen-lambda-1-rest body)
  (lambda (rte)
    (lambda a
      (body (vector rte a)))))

(define (gen-lambda-2-rest body)
  (lambda (rte)
    (lambda (a . b)
      (body (vector rte a b)))))

(define (gen-lambda-3-rest body)
  (lambda (rte)
    (lambda (a b . c)
      (body (vector rte a b c)))))

(define (gen-lambda-n-rest nb-vars body)
  (lambda (rte)
    (lambda (a b c . d)
      (let ((x (make-vector (SFX+ nb-vars 1))))
        (Svector-set! x 0 rte)
        (Svector-set! x 1 a)
        (Svector-set! x 2 b)
        (Svector-set! x 3 c)
        (let loop ((n nb-vars) (x x) (i 4) (l d))
          (if (SFX< i n)
            (begin (Svector-set! x i (Scar l)) (loop n x (SFX+ i 1) (Scdr l)))
            (Svector-set! x i l)))
        (body x)))))

(define (gen-lambda nb-vars body)
  (case nb-vars
    ((0)  (gen-lambda-0 body))
    ((1)  (gen-lambda-1 body))
    ((2)  (gen-lambda-2 body))
    ((3)  (gen-lambda-3 body))
    (else (gen-lambda-n nb-vars body))))

(define (gen-lambda-0 body)
  (lambda (rte)
    (lambda ()
      (body rte))))

(define (gen-lambda-1 body)
  (lambda (rte)
    (lambda (a)
      (body (vector rte a)))))

(define (gen-lambda-2 body)
  (lambda (rte)
    (lambda (a b)
      (body (vector rte a b)))))

(define (gen-lambda-3 body)
  (lambda (rte)
    (lambda (a b c)
      (body (vector rte a b c)))))

(define (gen-lambda-n nb-vars body)
  (lambda (rte)
    (lambda (a b c . d)
      (let ((x (make-vector (SFX+ nb-vars 1))))
        (Svector-set! x 0 rte)
        (Svector-set! x 1 a)
        (Svector-set! x 2 b)
        (Svector-set! x 3 c)
        (let loop ((n nb-vars) (x x) (i 4) (l d))
          (if (SFX<= i n)
            (begin (Svector-set! x i (Scar l)) (loop n x (SFX+ i 1) (Scdr l)))))
        (body x)))))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define (gen-sequence code1 code2)
  (lambda (rte) (code1 rte) (code2 rte)))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define (gen-when code1 code2)
  (lambda (rte)
    (if (code1 rte)
      (code2 rte)
      '())))

(define (gen-if code1 code2 code3)
  (lambda (rte)
    (if (code1 rte)
      (code2 rte)
      (code3 rte))))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define (gen-cond-send code1 code2 code3)
  (lambda (rte)
    (let ((temp (code1 rte)))
      (if temp
        ((code2 rte) temp)
        (code3 rte)))))
              
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define (gen-and code1 code2)
  (lambda (rte)
    (let ((temp (code1 rte)))
      (if temp
        (code2 rte)
        temp))))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define (gen-or code1 code2)
  (lambda (rte)
    (let ((temp (code1 rte)))
      (if temp
        temp
        (code2 rte)))))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define (gen-case code1 code2)
  (lambda (rte) (code2 rte (code1 rte))))

(define (gen-case-clause datums code1 code2)
  (lambda (rte key) (if (memv key datums) (code1 rte) (code2 rte key))))

(define (gen-case-else code)
  (lambda (rte key) (code rte)))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define (gen-letrec vals body)
  (let ((nb-vals (Slength vals)))
    (case nb-vals
      ((1)  (gen-letrec-1 (Scar vals) body))
      ((2)  (gen-letrec-2 (Scar vals) (Scadr vals) body))
      ((3)  (gen-letrec-3 (Scar vals) (Scadr vals) (Scaddr vals) body))
      (else (gen-letrec-n nb-vals vals body)))))

(define (gen-letrec-1 val1 body)
  (lambda (rte)
    (let ((x (vector rte #f)))
      (Svector-set! x 1 (val1 x))
      (body x))))

(define (gen-letrec-2 val1 val2 body)
  (lambda (rte)
    (let ((x (vector rte #f #f)))
      (Svector-set! x 1 (val1 x))
      (Svector-set! x 2 (val2 x))
      (body x))))

(define (gen-letrec-3 val1 val2 val3 body)
  (lambda (rte)
    (let ((x (vector rte #f #f #f)))
      (Svector-set! x 1 (val1 x))
      (Svector-set! x 2 (val2 x))
      (Svector-set! x 3 (val3 x))
      (body x))))

(define (gen-letrec-n nb-vals vals body)
  (lambda (rte)
    (let ((x (make-vector (SFX+ nb-vals 1))))
      (Svector-set! x 0 rte)
      (let loop ((x x) (i 1) (l vals))
        (if (pair? l)
          (begin (Svector-set! x i ((Scar l) x)) (loop x (SFX+ i 1) (Scdr l)))))
      (body x))))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define (gen-macro name proc)
  (lambda (rte) (scheme-add-macro name proc)))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define (gen-combination oper args)
  (case (Slength args)
    ((0)  (gen-combination-0 oper))
    ((1)  (gen-combination-1 oper (Scar args)))
    ((2)  (gen-combination-2 oper (Scar args) (Scadr args)))
    ((3)  (gen-combination-3 oper (Scar args) (Scadr args) (Scaddr args)))
    (else (gen-combination-n oper args))))

(define (gen-combination-0 oper)
  (lambda (rte) ((oper rte))))

(define (gen-combination-1 oper arg1)
  (lambda (rte) ((oper rte) (arg1 rte))))

(define (gen-combination-2 oper arg1 arg2)
  (lambda (rte) ((oper rte) (arg1 rte) (arg2 rte))))

(define (gen-combination-3 oper arg1 arg2 arg3)
  (lambda (rte) ((oper rte) (arg1 rte) (arg2 rte) (arg3 rte))))

(define (gen-combination-n oper args)
  (lambda (rte)
    (define (evaluate l rte)
      (if (pair? l)
        (cons ((Scar l) rte) (evaluate (Scdr l) rte))
        '()))
    (apply (oper rte) (evaluate args rte))))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define (scheme-comp expr env)
  (cond ((symbol? expr)
         (comp-var expr env))
        ((not (pair? expr))
         (comp-self-eval expr env))
        ((macro? (Scar expr) env)
         (scheme-comp (macro-expand expr env) env))
        (else
         (cond
           ((eq? (Scar expr) 'quote)            (comp-quote expr env))
           ((eq? (Scar expr) 'quasiquote)       (comp-quasiquote expr env))
           ((eq? (Scar expr) 'unquote)          (comp-unquote expr env))
           ((eq? (Scar expr) 'unquote-splicing) (comp-unquote-splicing expr env))
           ((eq? (Scar expr) 'set!)             (comp-set! expr env))
           ((eq? (Scar expr) 'lambda)           (comp-lambda expr env))
           ((eq? (Scar expr) 'if)               (comp-if expr env))
           ((eq? (Scar expr) 'cond)             (comp-cond expr env))
           ((eq? (Scar expr) 'and)              (comp-and expr env))
           ((eq? (Scar expr) 'or)               (comp-or expr env))
           ((eq? (Scar expr) 'case)             (comp-case expr env))
           ((eq? (Scar expr) 'let)              (comp-let expr env))
           ((eq? (Scar expr) 'let*)             (comp-let* expr env))
           ((eq? (Scar expr) 'letrec)           (comp-letrec expr env))
           ((eq? (Scar expr) 'begin)            (comp-begin expr env))
           ((eq? (Scar expr) 'do)               (comp-do expr env))
           ((eq? (Scar expr) 'define)           (comp-define expr env))
           ((eq? (Scar expr) 'define-macro)     (comp-define-macro expr env))
           (else                               (comp-combination expr env))))))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define (scheme-global-var name)
  (let ((x (assq name scheme-global-variables)))
    (if x
      x
      (let ((y (cons name '())))
        (set! scheme-global-variables (cons y scheme-global-variables))
        y))))

(define (scheme-global-var-ref i)
  (Scdr i))

(define (scheme-global-var-set! i val)
  (set-cdr! i val)
  '())

(define scheme-global-variables '())

(define (def-proc name value)
  (scheme-global-var-set!
    (scheme-global-var name)
    value))

(def-proc 'not                            (lambda (x) (not x)))
(def-proc 'boolean?                       boolean?)
(def-proc 'eqv?                           eqv?)
(def-proc 'eq?                            eq?)
(def-proc 'equal?                         equal?)
(def-proc 'pair?                          (lambda (obj) (pair? obj)))
(def-proc 'cons                           (lambda (x y) (cons x y)))
(def-proc 'car                            car)
(def-proc 'cdr                            cdr)
(def-proc 'set-car!                       set-car!)
(def-proc 'set-cdr!                       set-cdr!)
(def-proc 'caar                           caar)
(def-proc 'cadr                           cadr)
(def-proc 'cdar                           cdar)
(def-proc 'cddr                           cddr)
(def-proc 'caaar                          caaar)
(def-proc 'caadr                          caadr)
(def-proc 'cadar                          cadar)
(def-proc 'caddr                          caddr)
(def-proc 'cdaar                          cdaar)
(def-proc 'cdadr                          cdadr)
(def-proc 'cddar                          cddar)
(def-proc 'cdddr                          cdddr)
(def-proc 'caaaar                         caaaar)
(def-proc 'caaadr                         caaadr)
(def-proc 'caadar                         caadar)
(def-proc 'caaddr                         caaddr)
(def-proc 'cadaar                         cadaar)
(def-proc 'cadadr                         cadadr)
(def-proc 'caddar                         caddar)
(def-proc 'cadddr                         cadddr)
(def-proc 'cdaaar                         cdaaar)
(def-proc 'cdaadr                         cdaadr)
(def-proc 'cdadar                         cdadar)
(def-proc 'cdaddr                         cdaddr)
(def-proc 'cddaar                         cddaar)
(def-proc 'cddadr                         cddadr)
(def-proc 'cdddar                         cdddar)
(def-proc 'cddddr                         cddddr)
(def-proc 'null?                          (lambda (x) (null? x)))
(def-proc 'list?                          list?)
(def-proc 'list                           list)
(def-proc 'length                         length)
(def-proc 'append                         append)
(def-proc 'reverse                        reverse)
(def-proc 'list-ref                       list-ref)
(def-proc 'memq                           memq)
(def-proc 'memv                           memv)
(def-proc 'member                         member)
(def-proc 'assq                           assq)
(def-proc 'assv                           assv)
(def-proc 'assoc                          assoc)
(def-proc 'symbol?                        symbol?)
(def-proc 'symbol->string                 symbol->string)
(def-proc 'string->symbol                 string->symbol)
(def-proc 'number?                        number?)
(def-proc 'complex?                       complex?)
(def-proc 'real?                          real?)
(def-proc 'rational?                      rational?)
(def-proc 'integer?                       integer?)
(def-proc 'exact?                         exact?)
(def-proc 'inexact?                       inexact?)
;(def-proc '=                              =)
;(def-proc '<                              <)
;(def-proc '>                              >)
;(def-proc '<=                             <=)
;(def-proc '>=                             >=)
;(def-proc 'zero?                          zero?)
;(def-proc 'positive?                      positive?)
;(def-proc 'negative?                      negative?)
;(def-proc 'odd?                           odd?)
;(def-proc 'even?                          even?)
(def-proc 'max                            max)
(def-proc 'min                            min)
;(def-proc '+                              +)
;(def-proc '*                              *)
;(def-proc '-                              -)
(def-proc '/                              /)
(def-proc 'abs                            abs)
;(def-proc 'quotient                       quotient)
;(def-proc 'remainder                      remainder)
;(def-proc 'modulo                         modulo)
(def-proc 'gcd                            gcd)
(def-proc 'lcm                            lcm)
;(def-proc 'numerator                      numerator)
;(def-proc 'denominator                    denominator)
(def-proc 'floor                          floor)
(def-proc 'ceiling                        ceiling)
(def-proc 'truncate                       truncate)
(def-proc 'round                          round)
;(def-proc 'rationalize                    rationalize)
(def-proc 'exp                            exp)
(def-proc 'log                            log)
(def-proc 'sin                            sin)
(def-proc 'cos                            cos)
(def-proc 'tan                            tan)
(def-proc 'asin                           asin)
(def-proc 'acos                           acos)
(def-proc 'atan                           atan)
(def-proc 'sqrt                           sqrt)
(def-proc 'expt                           expt)
;(def-proc 'make-rectangular               make-rectangular)
;(def-proc 'make-polar                     make-polar)
;(def-proc 'real-part                      real-part)
;(def-proc 'imag-part                      imag-part)
;(def-proc 'magnitude                      magnitude)
;(def-proc 'angle                          angle)
(def-proc 'exact->inexact                 exact->inexact)
(def-proc 'inexact->exact                 inexact->exact)
(def-proc 'number->string                 number->string)
(def-proc 'string->number                 string->number)
(def-proc 'char?                          char?)
(def-proc 'char=?                         char=?)
(def-proc 'char<?                         char<?)
(def-proc 'char>?                         char>?)
(def-proc 'char<=?                        char<=?)
(def-proc 'char>=?                        char>=?)
(def-proc 'char-ci=?                      char-ci=?)
(def-proc 'char-ci<?                      char-ci<?)
(def-proc 'char-ci>?                      char-ci>?)
(def-proc 'char-ci<=?                     char-ci<=?)
(def-proc 'char-ci>=?                     char-ci>=?)
(def-proc 'char-alphabetic?               char-alphabetic?)
(def-proc 'char-numeric?                  char-numeric?)
(def-proc 'char-whitespace?               char-whitespace?)
(def-proc 'char-lower-case?               char-lower-case?)
(def-proc 'char->integer                  char->integer)
(def-proc 'integer->char                  integer->char)
(def-proc 'char-upcase                    char-upcase)
(def-proc 'char-downcase                  char-downcase)
(def-proc 'string?                        string?)
(def-proc 'make-string                    make-string)
(def-proc 'string                         string)
(def-proc 'string-length                  string-length)
(def-proc 'string-ref                     string-ref)
(def-proc 'string-set!                    string-set!)
(def-proc 'string=?                       string=?)
(def-proc 'string<?                       string<?)
(def-proc 'string>?                       string>?)
(def-proc 'string<=?                      string<=?)
(def-proc 'string>=?                      string>=?)
(def-proc 'string-ci=?                    string-ci=?)
(def-proc 'string-ci<?                    string-ci<?)
(def-proc 'string-ci>?                    string-ci>?)
(def-proc 'string-ci<=?                   string-ci<=?)
(def-proc 'string-ci>=?                   string-ci>=?)
(def-proc 'substring                      substring)
(def-proc 'string-append                  string-append)
(def-proc 'vector?                        vector?)
(def-proc 'make-vector                    make-vector)
(def-proc 'vector                         vector)
(def-proc 'vector-length                  vector-length)
(def-proc 'vector-ref                     vector-ref)
(def-proc 'vector-set!                    vector-set!)
(def-proc 'procedure?                     procedure?)
(def-proc 'apply                          apply)
(def-proc 'map                            map)
(def-proc 'for-each                       for-each)
; Removed due to being unused amd making the GVM interpreter crash
;(def-proc 'call-with-current-continuation call-with-current-continuation)
;(def-proc 'call-with-input-file           call-with-input-file)
;(def-proc 'call-with-output-file          call-with-output-file)
;(def-proc 'input-port?                    input-port?)
;(def-proc 'output-port?                   output-port?)
;(def-proc 'current-input-port             current-input-port)
;(def-proc 'current-output-port            current-output-port)
;(def-proc 'open-input-file                open-input-file)
;(def-proc 'open-output-file               open-output-file)
;(def-proc 'close-input-port               close-input-port)
;(def-proc 'close-output-port              close-output-port)
;(def-proc 'eof-object?                    eof-object?)
;(def-proc 'read                           read)
;(def-proc 'read-char                      read-char)
;(def-proc 'peek-char                      peek-char)
;(def-proc 'write                          write)
;(def-proc 'display                        display)
;(def-proc 'newline                        newline)
;(def-proc 'write-char                     write-char)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define (run-benchmark)
   (scheme-eval
      '(let ()
	
	(define (sort-list obj pred)
	   
	   (define (loop l)
	      (if (and (pair? l) (pair? (cdr l)))
		  (split l '() '())
		  l))
	   
	   (define (split l one two)
	      (if (pair? l)
		  (split (cdr l) two (cons (car l) one))
		  (merge (loop one) (loop two))))
	   
	   (define (merge one two)
	      (cond ((null? one) two)
		    ((pred (car two) (car one))
		     (cons (car two)
                        (merge (cdr two) one)))
		    (else
		     (cons (car one)
                        (merge (cdr one) two)))))
	   
	   (loop obj))
	
	(sort-list '("one" "two" "three" "four" "five" "six"
		     "seven" "eight" "nine" "ten" "eleven" "twelve")
	   string<?))))

(define-keys (run !key (n (unknown 100000 1)))
   (let loop ((n n) (result #f))
      (if (SFX> n 0)
	  (begin
	     (loop (SFX- n 1)
		(run-benchmark)))
	  result)))

(define expected-result
   '("eight" "eleven" "five" "four" "nine" "one"
     "seven" "six" "ten" "three" "twelve" "two"))

(define (check result)
   (equal? result expected-result))
