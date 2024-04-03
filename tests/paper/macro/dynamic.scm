;;; DYNAMIC -- Obtained from Andrew Wright.

;; Fritz's dynamic type inferencer, set up to run on itself
;; (see the end of this file).


;;----------------------------------------------------------------------------
;; Environment management
;;----------------------------------------------------------------------------

;; environments are lists of pairs, the first component being the key

;; general environment operations
;;
;; empty-env: Env
;; gen-binding: Key x Value -> Binding
;; binding-key: Binding -> Key
;; binding-value: Binding -> Value
;; binding-show: Binding -> Symbol*
;; extend-env-with-binding: Env x Binding -> Env
;; extend-env-with-env: Env x Env -> Env
;; lookup: Key x Env -> (Binding + False)
;; env->list: Env -> Binding*
;; env-show: Env -> Symbol*

;; bindings

(define gen-binding cons)
;; generates a type binding, binding a symbol to a type variable

(define binding-key car)
;; returns the key of a type binding

(define binding-value cdr)
;; returns the tvariable of a type binding

(define (key-show key) (set-bbv-version-limit! #f) 
  ;; default show procedure for keys
  key)

(define (value-show value) (set-bbv-version-limit! #f) 
  ;; default show procedure for values
  value)

(define (binding-show binding) (set-bbv-version-limit! #f) 
  ;; returns a printable representation of a type binding
  (cons (key-show (binding-key binding))
        (cons ': (value-show (binding-value binding)))))


;; environments

(define dynamic-empty-env '())
;; returns the empty environment

(define (extend-env-with-binding env binding) (set-bbv-version-limit! #f) 
  ;; extends env with a binding, which hides any other binding in env
  ;; for the same key (see dynamic-lookup)
  ;; returns the extended environment
  (cons binding env))

(define (extend-env-with-env env ext-env) (set-bbv-version-limit! #f) 
  ;; extends environment env with environment ext-env
  ;; a binding for a key in ext-env hides any binding in env for
  ;; the same key (see dynamic-lookup)
  ;; returns the extended environment
  (Sappend ext-env env))

(define dynamic-lookup (lambda (x l) (set-bbv-version-limit! #f)  (Sassv x l)))
;; returns the first pair in env that matches the key; returns #f
;; if no such pair exists

(define (env->list e) (set-bbv-version-limit! #f) 
  ;; converts an environment to a list of bindings
  e)

(define (env-show env) (set-bbv-version-limit! #f) 
  ;; returns a printable list representation of a type environment
  (Smap2 binding-show env))
;;----------------------------------------------------------------------------
;;       Parsing for Scheme
;;----------------------------------------------------------------------------


;; Needed packages: environment management

;;(load "env-mgmt.ss")
;;(load "pars-act.ss")

;; Lexical notions

(define syntactic-keywords
;;; source: IEEE Scheme, 7.1, <expression keyword>, <syntactic keyword>
  '(lambda if set! begin cond and or case let let* letrec do
           quasiquote else => define unquote unquote-splicing))


;; Parse routines

;; Datum

;; dynamic-parse-datum: parses nonterminal <datum>

(define (dynamic-parse-datum e) (set-bbv-version-limit! #f) 
;;; Source: IEEE Scheme, sect. 7.2, <datum>
;;; Note: "'" is parsed as 'quote, "`" as 'quasiquote, "," as
;;; 'unquote, ",@" as 'unquote-splicing (see sect. 4.2.5, p. 18)
;;; ***Note***: quasi-quotations are not permitted! (It would be
;;; necessary to pass the environment to dynamic-parse-datum.)
  (cond
   ((null? e)
    (dynamic-parse-action-null-const))
   ((boolean? e)
    (dynamic-parse-action-boolean-const e))
   ((char? e)
    (dynamic-parse-action-char-const e))
   ((number? e)
    (dynamic-parse-action-number-const e))
   ((string? e)
    (dynamic-parse-action-string-const e))
   ((symbol? e)
    (dynamic-parse-action-symbol-const e))
   ((vector? e)
    (dynamic-parse-action-vector-const (Smap2 dynamic-parse-datum (Svector->list e))))
   ((pair? e)
    (dynamic-parse-action-pair-const (dynamic-parse-datum (Scar e))
                                     (dynamic-parse-datum (Scdr e))))
   (else (fatal-error 'dynamic-parse-datum "Unknown datum: ~s" e))))


;; VarDef

;; dynamic-parse-formal: parses nonterminal <variable> in defining occurrence position

(define (dynamic-parse-formal f-env e) (set-bbv-version-limit! #f) 
  ;; e is an arbitrary object, f-env is a forbidden environment;
  ;; returns: a variable definition (a binding for the symbol), plus
  ;; the value of the binding as a result
  (if (symbol? e)
      (cond
       ((Smemq e syntactic-keywords)
        (fatal-error 'dynamic-parse-formal "Illegal identifier (keyword): ~s" e))
       ((dynamic-lookup e f-env)
        (fatal-error 'dynamic-parse-formal "Duplicate variable definition: ~s" e))
       (else (let ((dynamic-parse-action-result (dynamic-parse-action-var-def e)))
               (cons (gen-binding e dynamic-parse-action-result)
                     dynamic-parse-action-result))))
      (fatal-error 'dynamic-parse-formal "Not an identifier: ~s" e)))

;; dynamic-parse-formal*

(define (dynamic-parse-formal* formals) (set-bbv-version-limit! #f) 
;;; parses a list of formals and returns a pair consisting of generated
;;; environment and list of parsing action results
  (letrec
      ((pf*
        (lambda (f-env results formals) (set-bbv-version-limit! #f) 
;;; f-env: "forbidden" environment (to avoid duplicate defs)
;;; results: the results of the parsing actions
;;; formals: the unprocessed formals
;;; Note: generates the results of formals in reverse order!
          (cond
           ((null? formals)
            (cons f-env results))
           ((pair? formals)
            (let* ((fst-formal (car formals))
                   (binding-result (dynamic-parse-formal f-env fst-formal))
                   (binding (car binding-result))
                   (var-result (cdr binding-result)))
              (pf*
               (extend-env-with-binding f-env binding)
               (cons var-result results)
               (Scdr formals))))
           (else (fatal-error 'dynamic-parse-formal* "Illegal formals: ~s" formals))))))
    (let ((renv-rres (pf* dynamic-empty-env '() formals)))
      (cons (Scar renv-rres) (Sreverse (Scdr renv-rres))))))


;; dynamic-parse-formals: parses <formals>

(define (dynamic-parse-formals formals) (set-bbv-version-limit! #f) 
;;; parses <formals>; see IEEE Scheme, sect. 7.3
;;; returns a pair: env and result
  (letrec ((pfs (lambda (f-env formals) (set-bbv-version-limit! #f) 
                  (cond
                   ((null? formals)
                    (cons dynamic-empty-env (dynamic-parse-action-null-formal)))
                   ((pair? formals)
                    (let* ((fst-formal (Scar formals))
                           (rem-formals (Scdr formals))
                           (bind-res (dynamic-parse-formal f-env fst-formal))
                           (bind (Scar bind-res))
                           (res (Scdr bind-res))
                           (nf-env (extend-env-with-binding f-env bind))
                           (renv-res* (pfs nf-env rem-formals))
                           (renv (Scar renv-res*))
                           (res* (Scdr renv-res*)))
                      (cons
                       (extend-env-with-binding renv bind)
                       (dynamic-parse-action-pair-formal res res*))))
                   (else
                    (let* ((bind-res (dynamic-parse-formal f-env formals))
                           (bind (Scar bind-res))
                           (res (Scdr bind-res)))
                      (cons
                       (extend-env-with-binding dynamic-empty-env bind)
                       res)))))))
    (pfs dynamic-empty-env formals)))


;; Expr

;; dynamic-parse-expression: parses nonterminal <expression>

(define (dynamic-parse-expression env e) (set-bbv-version-limit! #f) 
  (cond
   ((symbol? e)
    (dynamic-parse-variable env e))
   ((pair? e)
    (let ((op (Scar e)) (args (Scdr e)))
      (case op
        ((quote) (dynamic-parse-quote env args))
        ((lambda) (dynamic-parse-lambda env args))
        ((if) (dynamic-parse-if env args))
        ((set!) (dynamic-parse-set env args))
        ((begin) (dynamic-parse-begin env args))
        ((cond) (dynamic-parse-cond env args))
        ((case) (dynamic-parse-case env args))
        ((and) (dynamic-parse-and env args))
        ((or) (dynamic-parse-or env args))
        ((let) (dynamic-parse-let env args))
        ((let*) (dynamic-parse-let* env args))
        ((letrec) (dynamic-parse-letrec env args))
        ((do) (dynamic-parse-do env args))
        ((quasiquote) (dynamic-parse-quasiquote env args))
        (else (dynamic-parse-procedure-call env op args)))))
   (else (dynamic-parse-datum e))))

;; dynamic-parse-expression*

(define (dynamic-parse-expression* env exprs) (set-bbv-version-limit! #f) 
;;; Parses lists of expressions (returns them in the right order!)
  (letrec ((pe*
            (lambda (results es) (set-bbv-version-limit! #f) 
              (cond
               ((null? es) results)
               ((pair? es) (pe* (cons (dynamic-parse-expression env (Scar es)) results) (Scdr es)))
               (else (fatal-error 'dynamic-parse-expression* "Not a list of expressions: ~s" es))))))
    (Sreverse (pe* '() exprs))))


;; dynamic-parse-expressions

(define (dynamic-parse-expressions env exprs) (set-bbv-version-limit! #f) 
;;; parses lists of arguments of a procedure call
  (cond
   ((null? exprs) (dynamic-parse-action-null-arg))
   ((pair? exprs) (let* ((fst-expr (Scar exprs))
                         (rem-exprs (Scdr exprs))
                         (fst-res (dynamic-parse-expression env fst-expr))
                         (rem-res (dynamic-parse-expressions env rem-exprs)))
                    (dynamic-parse-action-pair-arg fst-res rem-res)))
   (else (fatal-error 'dynamic-parse-expressions "Illegal expression list: ~s"
                exprs))))


;; dynamic-parse-variable: parses variables (applied occurrences)

(define (dynamic-parse-variable env e) (set-bbv-version-limit! #f) 
  (if (symbol? e)
      (if (Smemq e syntactic-keywords)
          (fatal-error 'dynamic-parse-variable "Illegal identifier (keyword): ~s" e)
          (let ((assoc-var-def (dynamic-lookup e env)))
            (if assoc-var-def
                (dynamic-parse-action-variable (binding-value assoc-var-def))
                (dynamic-parse-action-identifier e))))
      (fatal-error 'dynamic-parse-variable "Not an identifier: ~s" e)))


;; dynamic-parse-procedure-call

(define (dynamic-parse-procedure-call env op args) (set-bbv-version-limit! #f) 
  (dynamic-parse-action-procedure-call
   (dynamic-parse-expression env op)
   (dynamic-parse-expressions env args)))


;; dynamic-parse-quote

(define (dynamic-parse-quote env args) (set-bbv-version-limit! #f) 
  (if (list-of-1? args)
      (dynamic-parse-datum (Scar args))
      (fatal-error 'dynamic-parse-quote "Not a datum (multiple arguments): ~s" args)))


;; dynamic-parse-lambda

(define (dynamic-parse-lambda env args) (set-bbv-version-limit! #f) 
  (if (pair? args)
      (let* ((formals (Scar args))
             (body (Scdr args))
             (nenv-fresults (dynamic-parse-formals formals))
             (nenv (Scar nenv-fresults))
             (fresults (Scdr nenv-fresults)))
        (dynamic-parse-action-lambda-expression
         fresults
         (dynamic-parse-body (extend-env-with-env env nenv) body)))
      (fatal-error 'dynamic-parse-lambda "Illegal formals/body: ~s" args)))


;; dynamic-parse-body

(define (dynamic-parse-body env body) (set-bbv-version-limit! #f) 
  ;; <body> = <definition>* <expression>+
  (define (def-var* f-env body) (set-bbv-version-limit! #f) 
    ;; finds the defined variables in a body and returns an
    ;; environment containing them
    (if (pair? body)
        (let ((n-env (def-var f-env (Scar body))))
          (if n-env
              (def-var* n-env (Scdr body))
              f-env))
        f-env))
  (define (def-var f-env clause) (set-bbv-version-limit! #f) 
    ;; finds the defined variables in a single clause and extends
    ;; f-env accordingly; returns false if it's not a definition
    (if (pair? clause)
        (case (Scar clause)
          ((define) (if (pair? (Scdr clause))
                        (let ((pattern (Scadr clause)))
                          (cond
                           ((symbol? pattern)
                            (extend-env-with-binding
                             f-env
                             (gen-binding pattern
                                          (dynamic-parse-action-var-def pattern))))
                           ((and (pair? pattern) (symbol? (Scar pattern)))
                            (extend-env-with-binding
                             f-env
                             (gen-binding (Scar pattern)
                                          (dynamic-parse-action-var-def
                                           (Scar pattern)))))
                           (else f-env)))
                        f-env))
          ((begin) (def-var* f-env (Scdr clause)))
          (else #f))
        #f))
  (if (pair? body)
      (dynamic-parse-command* (def-var* env body) body)
      (fatal-error 'dynamic-parse-body "Illegal body: ~s" body)))

;; dynamic-parse-if

(define (dynamic-parse-if env args) (set-bbv-version-limit! #f) 
  (cond
   ((list-of-3? args)
    (dynamic-parse-action-conditional
     (dynamic-parse-expression env (Scar args))
     (dynamic-parse-expression env (Scadr args))
     (dynamic-parse-expression env (Scaddr args))))
   ((list-of-2? args)
    (dynamic-parse-action-conditional
     (dynamic-parse-expression env (Scar args))
     (dynamic-parse-expression env (Scadr args))
     (dynamic-parse-action-empty)))
   (else (fatal-error 'dynamic-parse-if "Not an if-expression: ~s" args))))


;; dynamic-parse-set

(define (dynamic-parse-set env args) (set-bbv-version-limit! #f) 
  (if (list-of-2? args)
      (dynamic-parse-action-assignment
       (dynamic-parse-variable env (Scar args))
       (dynamic-parse-expression env (Scadr args)))
      (fatal-error 'dynamic-parse-set "Not a variable/expression pair: ~s" args)))


;; dynamic-parse-begin

(define (dynamic-parse-begin env args) (set-bbv-version-limit! #f) 
  (dynamic-parse-action-begin-expression
   (dynamic-parse-body env args)))


;; dynamic-parse-cond

(define (dynamic-parse-cond env args) (set-bbv-version-limit! #f) 
  (if (and (pair? args) (Slist? args))
      (dynamic-parse-action-cond-expression
       (Smap2 (lambda (e) (set-bbv-version-limit! #f) 
              (dynamic-parse-cond-clause env e))
            args))
      (fatal-error 'dynamic-parse-cond "Not a list of cond-clauses: ~s" args)))

;; dynamic-parse-cond-clause

(define (dynamic-parse-cond-clause env e) (set-bbv-version-limit! #f) 
;;; ***Note***: Only (<test> <sequence>) is permitted!
  (if (pair? e)
      (cons
       (if (Sequal? (Scar e) 'else)
           (dynamic-parse-action-empty)
           (dynamic-parse-expression env (Scar e)))
       (dynamic-parse-body env (Scdr e)))
      (fatal-error 'dynamic-parse-cond-clause "Not a cond-clause: ~s" e)))


;; dynamic-parse-and

(define (dynamic-parse-and env args) (set-bbv-version-limit! #f) 
  (if (Slist? args)
      (dynamic-parse-action-and-expression
       (dynamic-parse-expression* env args))
      (fatal-error 'dynamic-parse-and "Not a list of arguments: ~s" args)))


;; dynamic-parse-or

(define (dynamic-parse-or env args) (set-bbv-version-limit! #f) 
  (if (Slist? args)
      (dynamic-parse-action-or-expression
       (dynamic-parse-expression* env args))
      (fatal-error 'dynamic-parse-or "Not a list of arguments: ~s" args)))


;; dynamic-parse-case

(define (dynamic-parse-case env args) (set-bbv-version-limit! #f) 
  (if (and (Slist? args) (> (Slength args) 1))
      (dynamic-parse-action-case-expression
       (dynamic-parse-expression env (Scar args))
       (Smap2 (lambda (e) (set-bbv-version-limit! #f) 
              (dynamic-parse-case-clause env e))
            (Scdr args)))
      (fatal-error 'dynamic-parse-case "Not a list of clauses: ~s" args)))

;; dynamic-parse-case-clause

(define (dynamic-parse-case-clause env e) (set-bbv-version-limit! #f) 
  (if (pair? e)
      (cons
       (cond
        ((Sequal? (Scar e) 'else)
         (list (dynamic-parse-action-empty)))
        ((Slist? (Scar e))
         (Smap2 dynamic-parse-datum (Scar e)))
        (else (fatal-error 'dynamic-parse-case-clause "Not a datum list: ~s" (Scar e))))
       (dynamic-parse-body env (Scdr e)))
      (fatal-error 'dynamic-parse-case-clause "Not case clause: ~s" e)))


;; dynamic-parse-let

(define (dynamic-parse-let env args) (set-bbv-version-limit! #f) 
  (if (pair? args)
      (if (symbol? (car args))
          (dynamic-parse-named-let env args)
          (dynamic-parse-normal-let env args))
      (fatal-error 'dynamic-parse-let "Illegal bindings/body: ~s" args)))


;; dynamic-parse-normal-let

(define (dynamic-parse-normal-let env args) (set-bbv-version-limit! #f) 
;;; parses "normal" let-expressions
  (let* ((bindings (Scar args))
         (body (Scdr args))
         (env-ast (dynamic-parse-parallel-bindings env bindings))
         (nenv (Scar env-ast))
         (bresults (Scdr env-ast)))
    (dynamic-parse-action-let-expression
     bresults
     (dynamic-parse-body (extend-env-with-env env nenv) body))))

;; dynamic-parse-named-let

(define (dynamic-parse-named-let env args) (set-bbv-version-limit! #f) 
;;; parses a named let-expression
  (if (pair? (Scdr args))
      (let* ((variable (Scar args))
             (bindings (Scadr args))
             (body (Scddr args))
             (vbind-vres (dynamic-parse-formal dynamic-empty-env variable))
             (vbind (Scar vbind-vres))
             (vres (Scdr vbind-vres))
             (env-ast (dynamic-parse-parallel-bindings env bindings))
             (nenv (Scar env-ast))
             (bresults (Scdr env-ast)))
        (dynamic-parse-action-named-let-expression
         vres bresults
         (dynamic-parse-body (extend-env-with-env
                              (extend-env-with-binding env vbind)
                              nenv) body)))
      (fatal-error 'dynamic-parse-named-let "Illegal named let-expression: ~s" args)))


;; dynamic-parse-parallel-bindings

(define (dynamic-parse-parallel-bindings env bindings) (set-bbv-version-limit! #f) 
  ;; returns a pair consisting of an environment
  ;; and a list of pairs (variable . asg)
  ;; ***Note***: the list of pairs is returned in reverse unzipped form!
  (if (list-of-list-of-2s? bindings)
      (let* ((env-formals-asg
              (dynamic-parse-formal* (Smap2 (lambda (p) (set-bbv-version-limit! #f)  (Scar p)) bindings)))
             (nenv (Scar env-formals-asg))
             (bresults (Scdr env-formals-asg))
             (exprs-asg
              (dynamic-parse-expression* env (Smap2 (lambda (l) (set-bbv-version-limit! #f)  (Scadr l)) bindings))))
        (cons nenv (cons bresults exprs-asg)))
      (fatal-error 'dynamic-parse-parallel-bindings
             "Not a list of bindings: ~s" bindings)))


;; dynamic-parse-let*

(define (dynamic-parse-let* env args) (set-bbv-version-limit! #f) 
  (if (pair? args)
      (let* ((bindings (Scar args))
             (body (Scdr args))
             (env-ast (dynamic-parse-sequential-bindings env bindings))
             (nenv (Scar env-ast))
             (bresults (Scdr env-ast)))
        (dynamic-parse-action-let*-expression
         bresults
         (dynamic-parse-body (extend-env-with-env env nenv) body)))
      (fatal-error 'dynamic-parse-let* "Illegal bindings/body: ~s" args)))

;; dynamic-parse-sequential-bindings

(define (dynamic-parse-sequential-bindings env bindings) (set-bbv-version-limit! #f) 
  ;; returns a pair consisting of an environment
  ;; and a list of pairs (variable . asg)
;;; ***Note***: the list of pairs is returned in reverse unzipped form!
  (letrec
      ((psb
        (lambda (f-env c-env var-defs expr-asgs binds) (set-bbv-version-limit! #f) 
;;; f-env: forbidden environment
;;; c-env: constructed environment
;;; var-defs: results of formals
;;; expr-asgs: results of corresponding expressions
;;; binds: reminding bindings to process
          (cond
           ((null? binds)
            (cons f-env (cons var-defs expr-asgs)))
           ((pair? binds)
            (let ((fst-bind (Scar binds)))
              (if (list-of-2? fst-bind)
                  (let* ((fbinding-bres
                          (dynamic-parse-formal f-env (Scar fst-bind)))
                         (fbind (Scar fbinding-bres))
                         (bres (Scdr fbinding-bres))
                         (new-expr-asg
                          (dynamic-parse-expression c-env (Scadr fst-bind))))
                    (psb
                     (extend-env-with-binding f-env fbind)
                     (extend-env-with-binding c-env fbind)
                     (cons bres var-defs)
                     (cons new-expr-asg expr-asgs)
                     (Scdr binds)))
                  (fatal-error 'dynamic-parse-sequential-bindings
                         "Illegal binding: ~s" fst-bind))))
           (else (fatal-error 'dynamic-parse-sequential-bindings
                        "Illegal bindings: ~s" binds))))))
    (let ((env-vdefs-easgs (psb dynamic-empty-env env '() '() bindings)))
      (cons (Scar env-vdefs-easgs)
            (cons (Sreverse (Scadr env-vdefs-easgs))
                  (Sreverse (Scddr env-vdefs-easgs)))))))


;; dynamic-parse-letrec

(define (dynamic-parse-letrec env args) (set-bbv-version-limit! #f) 
  (if (pair? args)
      (let* ((bindings (Scar args))
             (body (Scdr args))
             (env-ast (dynamic-parse-recursive-bindings env bindings))
             (nenv (Scar env-ast))
             (bresults (Scdr env-ast)))
        (dynamic-parse-action-letrec-expression
         bresults
         (dynamic-parse-body (extend-env-with-env env nenv) body)))
      (fatal-error 'dynamic-parse-letrec "Illegal bindings/body: ~s" args)))

;; dynamic-parse-recursive-bindings

(define (dynamic-parse-recursive-bindings env bindings) (set-bbv-version-limit! #f) 
;;; ***Note***: the list of pairs is returned in reverse unzipped form!
  (if (list-of-list-of-2s? bindings)
      (let* ((env-formals-asg
              (dynamic-parse-formal* (Smap2 (lambda (p) (set-bbv-version-limit! #f)  (Scar p)) bindings)))
             (formals-env
              (Scar env-formals-asg))
             (formals-res
              (Scdr env-formals-asg))
             (exprs-asg
              (dynamic-parse-expression*
               (extend-env-with-env env formals-env)
               (Smap2 (lambda (p) (set-bbv-version-limit! #f)  (Scadr p)) bindings))))
        (cons
         formals-env
         (cons formals-res exprs-asg)))
      (fatal-error 'dynamic-parse-recursive-bindings "Illegal bindings: ~s" bindings)))


;; dynamic-parse-do

(define (dynamic-parse-do env args) (set-bbv-version-limit! #f) 
;;; parses do-expressions
;;; ***Note***: Not implemented!
  (fatal-error 'dynamic-parse-do "Nothing yet..."))

;; dynamic-parse-quasiquote

(define (dynamic-parse-quasiquote env args) (set-bbv-version-limit! #f) 
;;; ***Note***: Not implemented!
  (fatal-error 'dynamic-parse-quasiquote "Nothing yet..."))


;; Command

;; dynamic-parse-command

(define (dynamic-parse-command env c) (set-bbv-version-limit! #f) 
  (if (pair? c)
      (let ((op (Scar c))
            (args (Scdr c)))
        (case op
          ((define) (dynamic-parse-define env args))
          ;;        ((begin) (dynamic-parse-command* env args));;; AKW
          ((begin) (dynamic-parse-action-begin-expression (dynamic-parse-command* env args)))
          (else (dynamic-parse-expression env c))))
      (dynamic-parse-expression env c)))


;; dynamic-parse-command*

(define (dynamic-parse-command* env commands) (set-bbv-version-limit! #f) 
;;; parses a sequence of commands
  (if (Slist? commands)
      (Smap2 (lambda (command) (set-bbv-version-limit! #f)  (dynamic-parse-command env command)) commands)
      (fatal-error 'dynamic-parse-command* "Invalid sequence of commands: ~s" commands)))


;; dynamic-parse-define

(define (dynamic-parse-define env args) (set-bbv-version-limit! #f) 
;;; three cases -- see IEEE Scheme, sect. 5.2
;;; ***Note***: the parser admits forms (define (x . y) (set-bbv-version-limit! #f)  ...)
;;; ***Note***: Variables are treated as applied occurrences!
  (if (pair? args)
      (let ((pattern (Scar args))
            (exp-or-body (Scdr args)))
        (cond
         ((symbol? pattern)
          (if (list-of-1? exp-or-body)
              (dynamic-parse-action-definition
               (dynamic-parse-variable env pattern)
               (dynamic-parse-expression env (Scar exp-or-body)))
              (fatal-error 'dynamic-parse-define "Not a single expression: ~s" exp-or-body)))
         ((pair? pattern)
          (let* ((function-name (Scar pattern))
                 (function-arg-names (Scdr pattern))
                 (env-ast (dynamic-parse-formals function-arg-names))
                 (formals-env (Scar env-ast))
                 (formals-ast (Scdr env-ast)))
            (dynamic-parse-action-function-definition
             (dynamic-parse-variable env function-name)
             formals-ast
             (dynamic-parse-body (extend-env-with-env env formals-env) exp-or-body))))
         (else (fatal-error 'dynamic-parse-define "Not a valid pattern: ~s" pattern))))
      (fatal-error 'dynamic-parse-define "Not a valid definition: ~s" args)))

;; Auxiliary routines

;; forall?

(define (forall? pred list) (set-bbv-version-limit! #f) 
  (if (null? list)
      #t
      (and (pred (Scar list)) (forall? pred (Scdr list)))))

;; list-of-1?

(define (list-of-1? l) (set-bbv-version-limit! #f) 
  (and (pair? l) (null? (Scdr l))))

;; list-of-2?

(define (list-of-2? l) (set-bbv-version-limit! #f) 
  (and (pair? l) (pair? (Scdr l)) (null? (Scddr l))))

;; list-of-3?

(define (list-of-3? l) (set-bbv-version-limit! #f) 
  (and (pair? l) (pair? (Scdr l)) (pair? (Scddr l)) (null? (Scdddr l))))

;; list-of-list-of-2s?

(define (list-of-list-of-2s? e) (set-bbv-version-limit! #f) 
  (cond
   ((null? e)
    #t)
   ((pair? e)
    (and (list-of-2? (Scar e)) (list-of-list-of-2s? (Scdr e))))
   (else #f)))


;; File processing

;; dynamic-parse-from-port

(define (dynamic-parse-from-port) (set-bbv-version-limit! #f) 
  (let ((next-input (my-read)))
    (if (eof-object? next-input)
        '()
        (dynamic-parse-action-commands
         (dynamic-parse-command dynamic-empty-env next-input)
         (dynamic-parse-from-port)))))

;; dynamic-parse-file

(define (dynamic-parse-file) (set-bbv-version-limit! #f) 
  (dynamic-parse-from-port))
;;----------------------------------------------------------------------------
;; Implementation of Union/find data structure in Scheme
;;----------------------------------------------------------------------------

;; for union/find the following attributes are necessary: rank, parent
;; (see Tarjan, "Data structures and network algorithms", 1983)
;; In the Scheme realization an element is represented as a single
;; cons cell; its address is the element itself; the car field contains
;; the parent, the cdr field is an address for a cons
;; cell containing the rank (car field) and the information (cdr field)


;; general union/find data structure
;;
;; gen-element: Info -> Elem
;; find: Elem -> Elem
;; link: Elem! x Elem! -> Elem
;; asymm-link: Elem! x Elem! -> Elem
;; info: Elem -> Info
;; set-info!: Elem! x Info -> Void


(define (gen-element info) (set-bbv-version-limit! #f) 
  ;; generates a new element: the parent field is initialized to '(),
  ;; the rank field to 0
  (cons '() (cons 0 info)))

(define info (lambda (l) (set-bbv-version-limit! #f)  (Scddr l)))
;; returns the information stored in an element

(define (set-info! elem info) (set-bbv-version-limit! #f) 
  ;; sets the info-field of elem to info
  (Sset-cdr! (Scdr elem) info))

;; (define (find! x) (set-bbv-version-limit! #f) 
;;;; finds the class representative of x and sets the parent field
;;;; directly to the class representative (a class representative has
;;;; '() as its parent) (uses path halving)
;;;;(display "Find!: ")
;;;;(display (pretty-print (info x)))
;;;;(newline)
;;   (let ((px (car x)))
;;     (if (null? px)
;;       x
;;       (let ((ppx (car px)))
;;         (if (null? ppx)
;;             px
;;             (begin
;;               (set-car! x ppx)
;;               (find! ppx)))))))

(define (find! elem) (set-bbv-version-limit! #f) 
  ;; finds the class representative of elem and sets the parent field
  ;; directly to the class representative (a class representative has
  ;; '() as its parent)
  ;;(display "Find!: ")
  ;;(display (pretty-print (info elem)))
  ;;(newline)
  (let ((p-elem (Scar elem)))
    (if (null? p-elem)
        elem
        (let ((rep-elem (find! p-elem)))
          (Sset-car! elem rep-elem)
          rep-elem))))

(define (link! elem-1 elem-2) (set-bbv-version-limit! #f) 
  ;; links class elements by rank
  ;; they must be distinct class representatives
  ;; returns the class representative of the merged equivalence classes
  ;;(display "Link!: ")
  ;;(display (pretty-print (list (info elem-1) (info elem-2))))
  ;;(newline)
  (let ((rank-1 (Scadr elem-1))
        (rank-2 (Scadr elem-2)))
    (cond
     ((= rank-1 rank-2)
      (Sset-car! (Scdr elem-2) (+ rank-2 1))
      (Sset-car! elem-1 elem-2)
      elem-2)
     ((> rank-1 rank-2)
      (Sset-car! elem-2 elem-1)
      elem-1)
     (else
      (Sset-car! elem-1 elem-2)
      elem-2))))

(define asymm-link! (lambda (l x) (set-bbv-version-limit! #f)  (Sset-car! l x)))

;;(define (asymm-link! elem-1 elem-2) (set-bbv-version-limit! #f) 
;; links elem-1 onto elem-2 no matter what rank;
;; does not update the rank of elem-2 and does not return a value
;; the two arguments must be distinct
;;(display "AsymmLink: ")
;;(display (pretty-print (list (info elem-1) (info elem-2))))
;;(newline)
;;(set-car! elem-1 elem-2))

;;----------------------------------------------------------------------------
;; Type management
;;----------------------------------------------------------------------------

;; introduces type variables and types for Scheme,


;; type TVar (type variables)
;;
;; gen-tvar:          () -> TVar
;; gen-type:          TCon x TVar* -> TVar
;; dynamic:           TVar
;; tvar-id:           TVar -> Symbol
;; tvar-def:          TVar -> Type + Null
;; tvar-show:         TVar -> Symbol*
;;
;; set-def!:          !TVar x TCon x TVar* -> Null
;; equiv!:            !TVar x !TVar -> Null
;;
;;
;; type TCon (type constructors)
;;
;; ...
;;
;; type Type (types)
;;
;; gen-type:          TCon x TVar* -> Type
;; type-con:          Type -> TCon
;; type-args:         Type -> TVar*
;;
;; boolean:           TVar
;; character:         TVar
;; null:              TVar
;; pair:              TVar x TVar -> TVar
;; procedure:         TVar x TVar* -> TVar
;; charseq:           TVar
;; symbol:            TVar
;; array:             TVar -> TVar


;; Needed packages: union/find

;;(load "union-fi.so")

;; TVar

(define counter 0)
;; counter for generating tvar id's

(define (gen-id) (set-bbv-version-limit! #f) 
  ;; generates a new id (for printing purposes)
  (set! counter (+ counter 1))
  counter)

(define (gen-tvar) (set-bbv-version-limit! #f) 
  ;; generates a new type variable from a new symbol
  ;; uses union/find elements with two info fields
  ;; a type variable has exactly four fields:
  ;; car:     TVar (the parent field; initially null)
  ;; cadr:    Number (the rank field; is always nonnegative)
  ;; caddr:   Symbol (the type variable identifier; used only for printing)
  ;; cdddr:   Type (the leq field; initially null)
  (gen-element (cons (gen-id) '())))

(define (gen-type tcon targs) (set-bbv-version-limit! #f) 
  ;; generates a new type variable with an associated type definition
  (gen-element (cons (gen-id) (cons tcon targs))))

(define dynamic (gen-element (cons 0 '())))
;; the special type variable dynamic
;; Generic operations

(define (tvar-id tvar) (set-bbv-version-limit! #f) 
  ;; returns the (printable) symbol representing the type variable
  (Scar (info tvar)))

(define (tvar-def tvar) (set-bbv-version-limit! #f) 
  ;; returns the type definition (if any) of the type variable
  (Scdr (info tvar)))

(define (set-def! tvar tcon targs) (set-bbv-version-limit! #f) 
  ;; sets the type definition part of tvar to type
  (Sset-cdr! (info tvar) (cons tcon targs))
  '())

(define (reset-def! tvar) (set-bbv-version-limit! #f) 
  ;; resets the type definition part of tvar to nil
  (Sset-cdr! (info tvar) '()))

(define type-con (lambda (l) (set-bbv-version-limit! #f)  (Scar l)))
;; returns the type constructor of a type definition

(define type-args (lambda (l) (set-bbv-version-limit! #f)  (Scdr l)))
;; returns the type variables of a type definition

(define (tvar->string tvar) (set-bbv-version-limit! #f) 
  ;; converts a tvar's id to a string
  (if (Sequal? (tvar-id tvar) 0)
      "Dynamic"
      (Sstring-append "t#" (SFXnumber->string (tvar-id tvar)))))

(define (tvar-show tv) (set-bbv-version-limit! #f) 
  ;; returns a printable list representation of type variable tv
  (let* ((tv-rep (find! tv))
         (tv-def (tvar-def tv-rep)))
    (cons (tvar->string tv-rep)
          (if (null? tv-def)
              '()
              (cons 'is (type-show tv-def))))))

(define (type-show type) (set-bbv-version-limit! #f) 
  ;; returns a printable list representation of type definition type
  (cond
   ((Sequal? (type-con type) ptype-con)
    (let ((new-tvar (gen-tvar)))
      (cons ptype-con
            (cons (tvar-show new-tvar)
                  (tvar-show ((type-args type) new-tvar))))))
   (else
    (cons (type-con type)
          (Smap2 (lambda (tv) (set-bbv-version-limit! #f) 
                 (tvar->string (find! tv)))
               (type-args type))))))



;; Special type operations

;; type constructor literals

(define boolean-con 'boolean)
(define char-con 'char)
(define null-con 'null)
(define number-con 'number)
(define pair-con 'pair)
(define procedure-con 'procedure)
(define string-con 'string)
(define symbol-con 'symbol)
(define vector-con 'vector)

;; type constants and type constructors

(define (null) (set-bbv-version-limit! #f) 
  ;; ***Note***: Temporarily changed to be a pair!
  ;; (gen-type null-con '())
  (pair (gen-tvar) (gen-tvar)))
(define (boolean) (set-bbv-version-limit! #f) 
  (gen-type boolean-con '()))
(define (character) (set-bbv-version-limit! #f) 
  (gen-type char-con '()))
(define (number) (set-bbv-version-limit! #f) 
  (gen-type number-con '()))
(define (charseq) (set-bbv-version-limit! #f) 
  (gen-type string-con '()))
(define (symbol) (set-bbv-version-limit! #f) 
  (gen-type symbol-con '()))
(define (pair tvar-1 tvar-2) (set-bbv-version-limit! #f) 
  (gen-type pair-con (list tvar-1 tvar-2)))
(define (array tvar) (set-bbv-version-limit! #f) 
  (gen-type vector-con (list tvar)))
(define (procedure arg-tvar res-tvar) (set-bbv-version-limit! #f) 
  (gen-type procedure-con (list arg-tvar res-tvar)))


;; equivalencing of type variables

(define (equiv! tv1 tv2) (set-bbv-version-limit! #f) 
  (let* ((tv1-rep (find! tv1))
         (tv2-rep (find! tv2))
         (tv1-def (tvar-def tv1-rep))
         (tv2-def (tvar-def tv2-rep)))
    (cond
     ((Sequal? tv1-rep tv2-rep)
      '())
     ((Sequal? tv2-rep dynamic)
      (equiv-with-dynamic! tv1-rep))
     ((Sequal? tv1-rep dynamic)
      (equiv-with-dynamic! tv2-rep))
     ((null? tv1-def)
      (if (null? tv2-def)
          ;; both tv1 and tv2 are distinct type variables
          (link! tv1-rep tv2-rep)
          ;; tv1 is a type variable, tv2 is a (nondynamic) type
          (asymm-link! tv1-rep tv2-rep)))
     ((null? tv2-def)
      ;; tv1 is a (nondynamic) type, tv2 is a type variable
      (asymm-link! tv2-rep tv1-rep))
     ((Sequal? (type-con tv1-def) (type-con tv2-def))
      ;; both tv1 and tv2 are (nondynamic) types with equal numbers of
      ;; arguments
      (link! tv1-rep tv2-rep)
      (Smap3 equiv! (type-args tv1-def) (type-args tv2-def)))
     (else
      ;; tv1 and tv2 are types with distinct type constructors or different
      ;; numbers of arguments
      (equiv-with-dynamic! tv1-rep)
      (equiv-with-dynamic! tv2-rep))))
  '())

(define (equiv-with-dynamic! tv) (set-bbv-version-limit! #f) 
  (let ((tv-rep (find! tv)))
    (if (not (Sequal? tv-rep dynamic))
        (let ((tv-def (tvar-def tv-rep)))
          (asymm-link! tv-rep dynamic)
          (if (not (null? tv-def))
              (Smap2 equiv-with-dynamic! (type-args tv-def))))))
  '())
;;----------------------------------------------------------------------------
;; Polymorphic type management
;;----------------------------------------------------------------------------

;; introduces parametric polymorphic types


;; forall: (Tvar -> Tvar) -> TVar
;; fix: (Tvar -> Tvar) -> Tvar
;;
;; instantiate-type: TVar -> TVar

;; type constructor literal for polymorphic types

(define ptype-con 'forall)

(define (forall tv-func) (set-bbv-version-limit! #f) 
  (gen-type ptype-con tv-func))

(define (forall2 tv-func2) (set-bbv-version-limit! #f) 
  (forall (lambda (tv1) (set-bbv-version-limit! #f) 
            (forall (lambda (tv2) (set-bbv-version-limit! #f) 
                      (tv-func2 tv1 tv2))))))

(define (forall3 tv-func3) (set-bbv-version-limit! #f) 
  (forall (lambda (tv1) (set-bbv-version-limit! #f) 
            (forall2 (lambda (tv2 tv3) (set-bbv-version-limit! #f) 
                       (tv-func3 tv1 tv2 tv3))))))

(define (forall4 tv-func4) (set-bbv-version-limit! #f) 
  (forall (lambda (tv1) (set-bbv-version-limit! #f) 
            (forall3 (lambda (tv2 tv3 tv4) (set-bbv-version-limit! #f) 
                       (tv-func4 tv1 tv2 tv3 tv4))))))

(define (forall5 tv-func5) (set-bbv-version-limit! #f) 
  (forall (lambda (tv1) (set-bbv-version-limit! #f) 
            (forall4 (lambda (tv2 tv3 tv4 tv5) (set-bbv-version-limit! #f) 
                       (tv-func5 tv1 tv2 tv3 tv4 tv5))))))


;; (polymorphic) instantiation

(define (instantiate-type tv) (set-bbv-version-limit! #f) 
  ;; instantiates type tv and returns a generic instance
  (let* ((tv-rep (find! tv))
         (tv-def (tvar-def tv-rep)))
    (cond
     ((null? tv-def)
      tv-rep)
     ((Sequal? (type-con tv-def) ptype-con)
      (instantiate-type ((type-args tv-def) (gen-tvar))))
     (else
      tv-rep))))

(define (fix tv-func) (set-bbv-version-limit! #f) 
  ;; forms a recursive type: the fixed point of type mapping tv-func
  (let* ((new-tvar (gen-tvar))
         (inst-tvar (tv-func new-tvar))
         (inst-def (tvar-def inst-tvar)))
    (if (null? inst-def)
        (fatal-error 'fix "Illegal recursive type: ~s"
               (list (tvar-show new-tvar) '= (tvar-show inst-tvar)))
        (begin
          (set-def! new-tvar
                    (type-con inst-def)
                    (type-args inst-def))
          new-tvar))))


;;----------------------------------------------------------------------------
;;       Constraint management
;;----------------------------------------------------------------------------


;; constraints

(define gen-constr (lambda (a b) (set-bbv-version-limit! #f)  (cons a b)))
;; generates an equality between tvar1 and tvar2

(define constr-lhs (lambda (c) (set-bbv-version-limit! #f)  (Scar c)))
;; returns the left-hand side of a constraint

(define constr-rhs (lambda (c) (set-bbv-version-limit! #f)  (Scdr c)))
;; returns the right-hand side of a constraint

(define (constr-show c) (set-bbv-version-limit! #f) 
  (cons (tvar-show (Scar c))
        (cons '=
              (cons (tvar-show (Scdr c)) '()))))


;; constraint set management

(define global-constraints '())

(define (init-global-constraints!) (set-bbv-version-limit! #f) 
  (set! global-constraints '()))

(define (add-constr! lhs rhs) (set-bbv-version-limit! #f) 
  (set! global-constraints
        (cons (gen-constr lhs rhs) global-constraints))
  '())

(define (glob-constr-show) (set-bbv-version-limit! #f) 
  ;; returns printable version of global constraints
  (Smap2 constr-show global-constraints))


;; constraint normalization

;; Needed packages: type management

;;(load "typ-mgmt.so")

(define (normalize-global-constraints!) (set-bbv-version-limit! #f) 
  (normalize! global-constraints)
  (init-global-constraints!))

(define (normalize! constraints) (set-bbv-version-limit! #f) 
  (Smap2 (lambda (c) (set-bbv-version-limit! #f) 
         (equiv! (constr-lhs c) (constr-rhs c))) constraints))
;; ----------------------------------------------------------------------------
;; Abstract syntax definition and parse actions
;; ----------------------------------------------------------------------------

;; Needed packages: ast-gen.ss
;;(load "ast-gen.ss")

;; Abstract syntax
;;
;; VarDef
;;
;; Identifier =         Symbol - SyntacticKeywords
;; SyntacticKeywords =  { ... } (see Section 7.1, IEEE Scheme Standard)
;;
;; Datum
;;
;; null-const:          Null            -> Datum
;; boolean-const:       Bool            -> Datum
;; char-const:          Char            -> Datum
;; number-const:        Number          -> Datum
;; string-const:        String          -> Datum
;; vector-const:        Datum*          -> Datum
;; pair-const:          Datum x Datum   -> Datum
;;
;; Expr
;;
;; Datum <              Expr
;;
;; var-def:             Identifier              -> VarDef
;; variable:            VarDef                  -> Expr
;; identifier:          Identifier              -> Expr
;; procedure-call:      Expr x Expr*            -> Expr
;; lambda-expression:   Formals x Body          -> Expr
;; conditional:         Expr x Expr x Expr      -> Expr
;; assignment:          Variable x Expr         -> Expr
;; cond-expression:     CondClause+             -> Expr
;; case-expression:     Expr x CaseClause*      -> Expr
;; and-expression:      Expr*                   -> Expr
;; or-expression:       Expr*                   -> Expr
;; let-expression:      (VarDef* x Expr*) x Body -> Expr
;; named-let-expression: VarDef x (VarDef* x Expr*) x Body -> Expr
;; let*-expression:     (VarDef* x Expr*) x Body -> Expr
;; letrec-expression:   (VarDef* x Expr*) x Body -> Expr
;; begin-expression:    Expr+                   -> Expr
;; do-expression:       IterDef* x CondClause x Expr* -> Expr
;; empty:                                       -> Expr
;;
;; VarDef* <            Formals
;;
;; simple-formal:       VarDef                  -> Formals
;; dotted-formals:      VarDef* x VarDef        -> Formals
;;
;; Body =               Definition* x Expr+     (reversed)
;; CondClause =         Expr x Expr+
;; CaseClause =         Datum* x Expr+
;; IterDef =            VarDef x Expr x Expr
;;
;; Definition
;;
;; definition:          Identifier x Expr       -> Definition
;; function-definition: Identifier x Formals x Body -> Definition
;; begin-command:       Definition*             -> Definition
;;
;; Expr <               Command
;; Definition <         Command
;;
;; Program =            Command*


;; Abstract syntax operators

;; Datum

(define null-const 0)
(define boolean-const 1)
(define char-const 2)
(define number-const 3)
(define string-const 4)
(define symbol-const 5)
(define vector-const 6)
(define pair-const 7)

;; Bindings

(define var-def 8)
(define null-def 29)
(define pair-def 30)

;; Expr

(define variable 9)
(define identifier 10)
(define procedure-call 11)
(define lambda-expression 12)
(define conditional 13)
(define assignment 14)
(define cond-expression 15)
(define case-expression 16)
(define and-expression 17)
(define or-expression 18)
(define let-expression 19)
(define named-let-expression 20)
(define let*-expression 21)
(define letrec-expression 22)
(define begin-expression 23)
(define do-expression 24)
(define empty 25)
(define null-arg 31)
(define pair-arg 32)

;; Command

(define definition 26)
(define function-definition 27)
(define begin-command 28)


;; Parse actions for abstract syntax construction

(define (dynamic-parse-action-null-const) (set-bbv-version-limit! #f) 
;;; dynamic-parse-action for '()
  (ast-gen null-const '()))

(define (dynamic-parse-action-boolean-const e) (set-bbv-version-limit! #f) 
;;; dynamic-parse-action for #f and #t
  (ast-gen boolean-const e))

(define (dynamic-parse-action-char-const e) (set-bbv-version-limit! #f) 
;;; dynamic-parse-action for character constants
  (ast-gen char-const e))

(define (dynamic-parse-action-number-const e) (set-bbv-version-limit! #f) 
;;; dynamic-parse-action for number constants
  (ast-gen number-const e))

(define (dynamic-parse-action-string-const e) (set-bbv-version-limit! #f) 
;;; dynamic-parse-action for string literals
  (ast-gen string-const e))

(define (dynamic-parse-action-symbol-const e) (set-bbv-version-limit! #f) 
;;; dynamic-parse-action for symbol constants
  (ast-gen symbol-const e))

(define (dynamic-parse-action-vector-const e) (set-bbv-version-limit! #f) 
;;; dynamic-parse-action for vector literals
  (ast-gen vector-const e))

(define (dynamic-parse-action-pair-const e1 e2) (set-bbv-version-limit! #f) 
;;; dynamic-parse-action for pairs
  (ast-gen pair-const (cons e1 e2)))

(define (dynamic-parse-action-var-def e) (set-bbv-version-limit! #f) 
;;; dynamic-parse-action for defining occurrences of variables;
;;; e is a symbol
  (ast-gen var-def e))

(define (dynamic-parse-action-null-formal) (set-bbv-version-limit! #f) 
;;; dynamic-parse-action for null-list of formals
  (ast-gen null-def '()))

(define (dynamic-parse-action-pair-formal d1 d2) (set-bbv-version-limit! #f) 
;;; dynamic-parse-action for non-null list of formals;
;;; d1 is the result of parsing the first formal,
;;; d2 the result of parsing the remaining formals
  (ast-gen pair-def (cons d1 d2)))

(define (dynamic-parse-action-variable e) (set-bbv-version-limit! #f) 
;;; dynamic-parse-action for applied occurrences of variables
;;; ***Note***: e is the result of a dynamic-parse-action on the
;;; corresponding variable definition!
  (ast-gen variable e))

(define (dynamic-parse-action-identifier e) (set-bbv-version-limit! #f) 
;;; dynamic-parse-action for undeclared identifiers (free variable
;;; occurrences)
;;; ***Note***: e is a symbol (legal identifier)
  (ast-gen identifier e))

(define (dynamic-parse-action-null-arg) (set-bbv-version-limit! #f) 
;;; dynamic-parse-action for a null list of arguments in a procedure call
  (ast-gen null-arg '()))

(define (dynamic-parse-action-pair-arg a1 a2) (set-bbv-version-limit! #f) 
;;; dynamic-parse-action for a non-null list of arguments in a procedure call
;;; a1 is the result of parsing the first argument,
;;; a2 the result of parsing the remaining arguments
  (ast-gen pair-arg (cons a1 a2)))

(define (dynamic-parse-action-procedure-call op args) (set-bbv-version-limit! #f) 
;;; dynamic-parse-action for procedure calls: op function, args list of arguments
  (ast-gen procedure-call (cons op args)))

(define (dynamic-parse-action-lambda-expression formals body) (set-bbv-version-limit! #f) 
;;; dynamic-parse-action for lambda-abstractions
  (ast-gen lambda-expression (cons formals body)))

(define (dynamic-parse-action-conditional test then-branch else-branch) (set-bbv-version-limit! #f) 
;;; dynamic-parse-action for conditionals (if-then-else expressions)
  (ast-gen conditional (cons test (cons then-branch else-branch))))

(define (dynamic-parse-action-empty) (set-bbv-version-limit! #f) 
;;; dynamic-parse-action for missing or empty field
  (ast-gen empty '()))

(define (dynamic-parse-action-assignment lhs rhs) (set-bbv-version-limit! #f) 
;;; dynamic-parse-action for assignment
  (ast-gen assignment (cons lhs rhs)))

(define (dynamic-parse-action-begin-expression body) (set-bbv-version-limit! #f) 
;;; dynamic-parse-action for begin-expression
  (ast-gen begin-expression body))

(define (dynamic-parse-action-cond-expression clauses) (set-bbv-version-limit! #f) 
;;; dynamic-parse-action for cond-expressions
  (ast-gen cond-expression clauses))

(define (dynamic-parse-action-and-expression args) (set-bbv-version-limit! #f) 
;;; dynamic-parse-action for and-expressions
  (ast-gen and-expression args))

(define (dynamic-parse-action-or-expression args) (set-bbv-version-limit! #f) 
;;; dynamic-parse-action for or-expressions
  (ast-gen or-expression args))

(define (dynamic-parse-action-case-expression key clauses) (set-bbv-version-limit! #f) 
;;; dynamic-parse-action for case-expressions
  (ast-gen case-expression (cons key clauses)))

(define (dynamic-parse-action-let-expression bindings body) (set-bbv-version-limit! #f) 
;;; dynamic-parse-action for let-expressions
  (ast-gen let-expression (cons bindings body)))

(define (dynamic-parse-action-named-let-expression variable bindings body) (set-bbv-version-limit! #f) 
;;; dynamic-parse-action for named-let expressions
  (ast-gen named-let-expression (cons variable (cons bindings body))))

(define (dynamic-parse-action-let*-expression bindings body) (set-bbv-version-limit! #f) 
;;; dynamic-parse-action for let-expressions
  (ast-gen let*-expression (cons bindings body)))

(define (dynamic-parse-action-letrec-expression bindings body) (set-bbv-version-limit! #f) 
;;; dynamic-parse-action for let-expressions
  (ast-gen letrec-expression (cons bindings body)))

(define (dynamic-parse-action-definition variable expr) (set-bbv-version-limit! #f) 
;;; dynamic-parse-action for simple definitions
  (ast-gen definition (cons variable expr)))

(define (dynamic-parse-action-function-definition variable formals body) (set-bbv-version-limit! #f) 
;;; dynamic-parse-action for function definitions
  (ast-gen function-definition (cons variable (cons formals body))))


(define dynamic-parse-action-commands (lambda (a b) (set-bbv-version-limit! #f)  (cons a b)))
;; dynamic-parse-action for processing a command result followed by a the
;; result of processing the remaining commands


;; Pretty-printing abstract syntax trees

(define (ast-show ast) (set-bbv-version-limit! #f) 
;;; converts abstract syntax tree to list representation (Scheme program)
;;; ***Note***: check translation of constructors to numbers at the top of the file
  (let ((syntax-op (ast-con ast))
        (syntax-arg (ast-arg ast)))
    (case syntax-op
      ((0 1 2 3 4 8 10) syntax-arg)
      ((29 31) '())
      ((30 32) (cons (ast-show (Scar syntax-arg)) (ast-show (Scdr syntax-arg))))
      ((5) (list 'quote syntax-arg))
      ((6) (Slist->vector (Smap2 ast-show syntax-arg)))
      ((7) (list 'cons (ast-show (Scar syntax-arg)) (ast-show (Scdr syntax-arg))))
      ((9) (ast-arg syntax-arg))
      ((11) (cons (ast-show (Scar syntax-arg)) (ast-show (Scdr syntax-arg))))
      ((12) (cons 'lambda (cons (ast-show (Scar syntax-arg))
                                (Smap2 ast-show (Scdr syntax-arg)))))
      ((13) (cons 'if (cons (ast-show (Scar syntax-arg))
                            (cons (ast-show (Scadr syntax-arg))
                                  (let ((alt (Scddr syntax-arg)))
                                    (if (Sequal? (ast-con alt) empty)
                                        '()
                                        (list (ast-show alt))))))))
      ((14) (list 'set! (ast-show (Scar syntax-arg)) (ast-show (Scdr syntax-arg))))
      ((15) (cons 'cond
                  (Smap2 (lambda (cc) (set-bbv-version-limit! #f) 
                         (let ((guard (Scar cc))
                               (body (Scdr cc)))
                           (cons
                            (if (Sequal? (ast-con guard) empty)
                                'else
                                (ast-show guard))
                            (Smap2 ast-show body))))
                       syntax-arg)))
      ((16) (cons 'case
                  (cons (ast-show (Scar syntax-arg))
                        (Smap2 (lambda (cc) (set-bbv-version-limit! #f) 
                               (let ((data (Scar cc)))
                                 (if (and (pair? data)
                                          (Sequal? (ast-con (Scar data)) empty))
                                     (cons 'else
                                           (Smap2 ast-show (Scdr cc)))
                                     (cons (Smap2 datum-show data)
                                           (Smap2 ast-show (Scdr cc))))))
                             (Scdr syntax-arg)))))
      ((17) (cons 'and (Smap2 ast-show syntax-arg)))
      ((18) (cons 'or (Smap2 ast-show syntax-arg)))
      ((19) (cons 'let
                  (cons (Smap3
                         (lambda (vd e) (set-bbv-version-limit! #f) 
                           (list (ast-show vd) (ast-show e)))
                         (Scaar syntax-arg)
                         (Scdar syntax-arg))
                        (Smap2 ast-show (Scdr syntax-arg)))))
      ((20) (cons 'let
                  (cons (ast-show (Scar syntax-arg))
                        (cons (Smap3
                               (lambda (vd e) (set-bbv-version-limit! #f) 
                                 (list (ast-show vd) (ast-show e)))
                               (Scaadr syntax-arg)
                               (Scdadr syntax-arg))
                              (Smap2 ast-show (Scddr syntax-arg))))))
      ((21) (cons 'let*
                  (cons (Smap3
                         (lambda (vd e) (set-bbv-version-limit! #f) 
                           (list (ast-show vd) (ast-show e)))
                         (Scaar syntax-arg)
                         (Scdar syntax-arg))
                        (Smap2 ast-show (Scdr syntax-arg)))))
      ((22) (cons 'letrec
                  (cons (Smap3
                         (lambda (vd e) (set-bbv-version-limit! #f) 
                           (list (ast-show vd) (ast-show e)))
                         (Scaar syntax-arg)
                         (Scdar syntax-arg))
                        (Smap2 ast-show (cdr syntax-arg)))))
      ((23) (cons 'begin
                  (Smap2 ast-show syntax-arg)))
      ((24) (fatal-error 'ast-show "Do expressions not handled! (~s)" syntax-arg))
      ((25) (fatal-error 'ast-show "This can't happen: empty encountered!"))
      ((26) (list 'define
                  (ast-show (Scar syntax-arg))
                  (ast-show (Scdr syntax-arg))))
      ((27) (cons 'define
                  (cons
                   (cons (ast-show (Scar syntax-arg))
                         (ast-show (Scadr syntax-arg)))
                   (Smap2 ast-show (Scddr syntax-arg)))))
      ((28) (cons 'begin
                  (Smap2 ast-show syntax-arg)))
      (else (fatal-error 'ast-show "Unknown abstract syntax operator: ~s"
                   syntax-op)))))


;; ast*-show

(define (ast*-show p) (set-bbv-version-limit! #f) 
;;; shows a list of abstract syntax trees
  (Smap2 ast-show p))


;; datum-show

(define (datum-show ast) (set-bbv-version-limit! #f) 
;;; prints an abstract syntax tree as a datum
  (case (ast-con ast)
    ((0 1 2 3 4 5) (ast-arg ast))
    ((6) (Slist->vector (Smap2 datum-show (ast-arg ast))))
    ((7) (cons (datum-show (Scar (ast-arg ast))) (datum-show (Scdr (ast-arg ast)))))
    (else (fatal-error 'datum-show "This should not happen!"))))

;; write-to-port

(define (write-to-port prog port) (set-bbv-version-limit! #f) 
  ;; writes a program to a port
  (for-each
   (lambda (command) (set-bbv-version-limit! #f) 
     (write command port)
     (newline port))
   prog)
  '())

;; write-file

(define (write-to-file prog filename) (set-bbv-version-limit! #f) 
  ;; write a program to a file
  (let ((port (open-output-file filename)))
    (write-to-port prog port)
    (close-output-port port)
    '()))

;; ----------------------------------------------------------------------------
;; Typed abstract syntax tree management: constraint generation, display, etc.
;; ----------------------------------------------------------------------------


;; Abstract syntax operations, incl. constraint generation

(define (ast-gen syntax-op arg) (set-bbv-version-limit! #f) 
  ;; generates all attributes and performs semantic side effects
  (let ((ntvar
         (case syntax-op
           ((0 29 31) (null))
           ((1) (boolean))
           ((2) (character))
           ((3) (number))
           ((4) (charseq))
           ((5) (symbol))
           ((6) (let ((aux-tvar (gen-tvar)))
                  (Sfor-each2 (lambda (t) (set-bbv-version-limit! #f) 
                              (add-constr! t aux-tvar))
                            (Smap2 ast-tvar arg))
                  (array aux-tvar)))
           ((7 30 32) (let ((t1 (ast-tvar (Scar arg)))
                            (t2 (ast-tvar (Scdr arg))))
                        (pair t1 t2)))
           ((8) (gen-tvar))
           ((9) (ast-tvar arg))
           ((10) (let ((in-env (dynamic-lookup arg dynamic-top-level-env)))
                   (if in-env
                       (instantiate-type (binding-value in-env))
                       (let ((new-tvar (gen-tvar)))
                         (set! dynamic-top-level-env (extend-env-with-binding
                                                      dynamic-top-level-env
                                                      (gen-binding arg new-tvar)))
                         new-tvar))))
           ((11) (let ((new-tvar (gen-tvar)))
                   (add-constr! (procedure (ast-tvar (Scdr arg)) new-tvar)
                                (ast-tvar (Scar arg)))
                   new-tvar))
           ((12) (procedure (ast-tvar (Scar arg))
                            (ast-tvar (tail (Scdr arg)))))
           ((13) (let ((t-test (ast-tvar (Scar arg)))
                       (t-consequent (ast-tvar (Scadr arg)))
                       (t-alternate (ast-tvar (Scddr arg))))
                   (add-constr! (boolean) t-test)
                   (add-constr! t-consequent t-alternate)
                   t-consequent))
           ((14) (let ((var-tvar (ast-tvar (Scar arg)))
                       (exp-tvar (ast-tvar (Scdr arg))))
                   (add-constr! var-tvar exp-tvar)
                   var-tvar))
           ((15) (let ((new-tvar (gen-tvar)))
                   (Sfor-each2 (lambda (body) (set-bbv-version-limit! #f) 
                               (add-constr! (ast-tvar (tail body)) new-tvar))
                             (Smap2 (lambda (p) (set-bbv-version-limit! #f)  (Scdr p)) arg))
                   (Sfor-each2 (lambda (e) (set-bbv-version-limit! #f) 
                               (add-constr! (boolean) (ast-tvar e)))
                             (Smap2 (lambda (p) (set-bbv-version-limit! #f)  (Scar p)) arg))
                   new-tvar))
           ((16) (let* ((new-tvar (gen-tvar))
                        (t-key (ast-tvar (Scar arg)))
                        (case-clauses (Scdr arg)))
                   (Sfor-each2 (lambda (exprs) (set-bbv-version-limit! #f) 
                               (Sfor-each2 (lambda (e) (set-bbv-version-limit! #f) 
                                           (add-constr! (ast-tvar e) t-key))
                                         exprs))
                             (Smap2 (lambda (p) (set-bbv-version-limit! #f)  (Scar p)) case-clauses))
                   (Sfor-each2 (lambda (body) (set-bbv-version-limit! #f) 
                               (add-constr! (ast-tvar (tail body)) new-tvar))
                             (Smap2 (lambda (p) (set-bbv-version-limit! #f)  (Scdr p)) case-clauses))
                   new-tvar))
           ((17 18) (Sfor-each2 (lambda (e) (set-bbv-version-limit! #f) 
                                (add-constr! (boolean) (ast-tvar e)))
                              arg)
            (boolean))
           ((19 21 22) (let ((var-def-tvars (Smap2 ast-tvar (Scaar arg)))
                             (def-expr-types (Smap2 ast-tvar (Scdar arg)))
                             (body-type (ast-tvar (tail (Scdr arg)))))
                         (Sfor-each3 add-constr! var-def-tvars def-expr-types)
                         body-type))
           ((20) (let ((var-def-tvars (Smap2 ast-tvar (Scaadr arg)))
                       (def-expr-types (Smap2 ast-tvar (Scdadr arg)))
                       (body-type (ast-tvar (tail (Scddr arg))))
                       (named-var-type (ast-tvar (Scar arg))))
                   (Sfor-each3 add-constr! var-def-tvars def-expr-types)
                   (add-constr! (procedure (convert-tvars var-def-tvars) body-type)
                                named-var-type)
                   body-type))
           ((23) (ast-tvar (tail arg)))
           ((24) (fatal-error 'ast-gen
                        "Do-expressions not handled! (Argument: ~s) arg"))
           ((25) (gen-tvar))
           ((26) (let ((t-var (ast-tvar (Scar arg)))
                       (t-exp (ast-tvar (Scdr arg))))
                   (add-constr! t-var t-exp)
                   t-var))
           ((27) (let ((t-var (ast-tvar (Scar arg)))
                       (t-formals (ast-tvar (Scadr arg)))
                       (t-body (ast-tvar (tail (Scddr arg)))))
                   (add-constr! (procedure t-formals t-body) t-var)
                   t-var))
           ((28) (gen-tvar))
           (else (fatal-error 'ast-gen "Can't handle syntax operator: ~s" syntax-op)))))
    (cons syntax-op (cons ntvar arg))))

(define ast-con car)
;; extracts the ast-constructor from an abstract syntax tree

(define ast-arg cddr)
;; extracts the ast-argument from an abstract syntax tree

(define ast-tvar cadr)
;; extracts the tvar from an abstract syntax tree


;; tail

(define (tail l) (set-bbv-version-limit! #f) 
;;; returns the tail of a nonempty list
  (if (null? (Scdr l))
      (Scar l)
      (tail (Scdr l))))

;; convert-tvars

(define (convert-tvars tvar-list) (set-bbv-version-limit! #f) 
;;; converts a list of tvars to a single tvar
  (cond
   ((null? tvar-list) (null))
   ((pair? tvar-list) (pair (Scar tvar-list)
                            (convert-tvars (Scdr tvar-list))))
   (else (fatal-error 'convert-tvars "Not a list of tvars: ~s" tvar-list))))


;; Pretty-printing abstract syntax trees

(define (tast-show ast) (set-bbv-version-limit! #f) 
;;; converts abstract syntax tree to list representation (Scheme program)
  (let ((syntax-op (ast-con ast))
        (syntax-tvar (tvar-show (ast-tvar ast)))
        (syntax-arg (ast-arg ast)))
    (cons
     (case syntax-op
       ((0 1 2 3 4 8 10) syntax-arg)
       ((29 31) '())
       ((30 32) (cons (tast-show (Scar syntax-arg))
                      (tast-show (Scdr syntax-arg))))
       ((5) (list 'quote syntax-arg))
       ((6) (Slist->vector (Smap2 tast-show syntax-arg)))
       ((7) (list 'cons (tast-show (Scar syntax-arg))
                  (tast-show (Scdr syntax-arg))))
       ((9) (ast-arg syntax-arg))
       ((11) (cons (tast-show (Scar syntax-arg)) (tast-show (Scdr syntax-arg))))
       ((12) (cons 'lambda (cons (tast-show (Scar syntax-arg))
                                 (Smap2 tast-show (Scdr syntax-arg)))))
       ((13) (cons 'if (cons (tast-show (Scar syntax-arg))
                             (cons (tast-show (Scadr syntax-arg))
                                   (let ((alt (Scddr syntax-arg)))
                                     (if (Sequal? (ast-con alt) empty)
                                         '()
                                         (list (tast-show alt))))))))
       ((14) (list 'set! (tast-show (Scar syntax-arg))
                   (tast-show (Scdr syntax-arg))))
       ((15) (cons 'cond
                   (Smap2 (lambda (cc) (set-bbv-version-limit! #f) 
                          (let ((guard (Scar cc))
                                (body (Scdr cc)))
                            (cons
                             (if (Sequal? (ast-con guard) empty)
                                 'else
                                 (tast-show guard))
                             (Smap2 tast-show body))))
                        syntax-arg)))
       ((16) (cons 'case
                   (cons (tast-show (Scar syntax-arg))
                         (Smap2 (lambda (cc) (set-bbv-version-limit! #f) 
                                (let ((data (Scar cc)))
                                  (if (and (pair? data)
                                           (Sequal? (ast-con (Scar data)) empty))
                                      (cons 'else
                                            (Smap2 tast-show (Scdr cc)))
                                      (cons (Smap2 datum-show data)
                                            (Smap2 tast-show (Scdr cc))))))
                              (Scdr syntax-arg)))))
       ((17) (cons 'and (Smap2 tast-show syntax-arg)))
       ((18) (cons 'or (Smap2 tast-show syntax-arg)))
       ((19) (cons 'let
                   (cons (Smap3
                          (lambda (vd e) (set-bbv-version-limit! #f) 
                            (list (tast-show vd) (tast-show e)))
                          (Scaar syntax-arg)
                          (Scdar syntax-arg))
                         (Smap2 tast-show (cdr syntax-arg)))))
       ((20) (cons 'let
                   (cons (tast-show (Scar syntax-arg))
                         (cons (Smap3
                                (lambda (vd e) (set-bbv-version-limit! #f) 
                                  (list (tast-show vd) (tast-show e)))
                                (Scaadr syntax-arg)
                                (Scdadr syntax-arg))
                               (Smap2 tast-show (Scddr syntax-arg))))))
       ((21) (cons 'let*
                   (cons (Smap3
                          (lambda (vd e) (set-bbv-version-limit! #f) 
                            (list (tast-show vd) (tast-show e)))
                          (Scaar syntax-arg)
                          (Scdar syntax-arg))
                         (Smap2 tast-show (Scdr syntax-arg)))))
       ((22) (cons 'letrec
                   (cons (Smap3
                          (lambda (vd e) (set-bbv-version-limit! #f) 
                            (list (tast-show vd) (tast-show e)))
                          (Scaar syntax-arg)
                          (Scdar syntax-arg))
                         (Smap2 tast-show (Scdr syntax-arg)))))
       ((23) (cons 'begin
                   (Smap2 tast-show syntax-arg)))
       ((24) (fatal-error 'tast-show "Do expressions not handled! (~s)" syntax-arg))
       ((25) (fatal-error 'tast-show "This can't happen: empty encountered!"))
       ((26) (list 'define
                   (tast-show (Scar syntax-arg))
                   (tast-show (Scdr syntax-arg))))
       ((27) (cons 'define
                   (cons
                    (cons (tast-show (Scar syntax-arg))
                          (tast-show (Scadr syntax-arg)))
                    (Smap2 tast-show (Scddr syntax-arg)))))
       ((28) (cons 'begin
                   (Smap2 tast-show syntax-arg)))
       (else (fatal-error 'tast-show "Unknown abstract syntax operator: ~s"
                    syntax-op)))
     syntax-tvar)))

;; tast*-show

(define (tast*-show p) (set-bbv-version-limit! #f) 
;;; shows a list of abstract syntax trees
  (Smap2 tast-show p))


;; counters for tagging/untagging

(define untag-counter 0)
(define no-untag-counter 0)
(define tag-counter 0)
(define no-tag-counter 0)
(define may-untag-counter 0)
(define no-may-untag-counter 0)

(define (reset-counters!) (set-bbv-version-limit! #f) 
  (set! untag-counter 0)
  (set! no-untag-counter 0)
  (set! tag-counter 0)
  (set! no-tag-counter 0)
  (set! may-untag-counter 0)
  (set! no-may-untag-counter 0))

(define (counters-show) (set-bbv-version-limit! #f) 
  (list
   (cons tag-counter no-tag-counter)
   (cons untag-counter no-untag-counter)
   (cons may-untag-counter no-may-untag-counter)))


;; tag-show

(define (tag-show tvar-rep prog) (set-bbv-version-limit! #f) 
  ;; display prog with tagging operation
  (if (Sequal? tvar-rep dynamic)
      (begin
        (set! tag-counter (SFX+ tag-counter 1))
        (list 'tag prog))
      (begin
        (set! no-tag-counter (SFX+ no-tag-counter 1))
        (list 'no-tag prog))))


;; untag-show

(define (untag-show tvar-rep prog) (set-bbv-version-limit! #f) 
  ;; display prog with untagging operation
  (if (Sequal? tvar-rep dynamic)
      (begin
        (set! untag-counter (SFX+ untag-counter 1))
        (list 'untag prog))
      (begin
        (set! no-untag-counter (SFX+ no-untag-counter 1))
        (list 'no-untag prog))))

(define (may-untag-show tvar-rep prog) (set-bbv-version-limit! #f) 
  ;; display possible untagging in actual arguments
  (if (Sequal? tvar-rep dynamic)
      (begin
        (set! may-untag-counter (SFX+ may-untag-counter 1))
        (list 'may-untag prog))
      (begin
        (set! no-may-untag-counter (SFX+ no-may-untag-counter 1))
        (list 'no-may-untag prog))))


;; tag-ast-show

(define (tag-ast-show ast) (set-bbv-version-limit! #f) 
;;; converts typed and normalized abstract syntax tree to
;;; a Scheme program with explicit tagging and untagging operations
  (let ((syntax-op (ast-con ast))
        (syntax-tvar (find! (ast-tvar ast)))
        (syntax-arg (ast-arg ast)))
    (case syntax-op
      ((0 1 2 3 4)
       (tag-show syntax-tvar syntax-arg))
      ((8 10) syntax-arg)
      ((29 31) '())
      ((30) (cons (tag-ast-show (Scar syntax-arg))
                  (tag-ast-show (Scdr syntax-arg))))
      ((32) (cons (may-untag-show (find! (ast-tvar (Scar syntax-arg)))
                                  (tag-ast-show (Scar syntax-arg)))
                  (tag-ast-show (Scdr syntax-arg))))
      ((5) (tag-show syntax-tvar (list 'quote syntax-arg)))
      ((6) (tag-show syntax-tvar (Slist->vector (Smap2 tag-ast-show syntax-arg))))
      ((7) (tag-show syntax-tvar (list 'cons (tag-ast-show (Scar syntax-arg))
                                       (tag-ast-show (Scdr syntax-arg)))))
      ((9) (ast-arg syntax-arg))
      ((11) (let ((proc-tvar (find! (ast-tvar (Scar syntax-arg)))))
              (cons (untag-show proc-tvar
                                (tag-ast-show (Scar syntax-arg)))
                    (tag-ast-show (Scdr syntax-arg)))))
      ((12) (tag-show syntax-tvar
                      (cons 'lambda (cons (tag-ast-show (Scar syntax-arg))
                                          (Smap2 tag-ast-show (cdr syntax-arg))))))
      ((13) (let ((test-tvar (find! (ast-tvar (Scar syntax-arg)))))
              (cons 'if (cons (untag-show test-tvar
                                          (tag-ast-show (Scar syntax-arg)))
                              (cons (tag-ast-show (Scadr syntax-arg))
                                    (let ((alt (Scddr syntax-arg)))
                                      (if (Sequal? (ast-con alt) empty)
                                          '()
                                          (list (tag-ast-show alt)))))))))
      ((14) (list 'set! (tag-ast-show (Scar syntax-arg))
                  (tag-ast-show (cdr syntax-arg))))
      ((15) (cons 'cond
                  (Smap2 (lambda (cc) (set-bbv-version-limit! #f) 
                         (let ((guard (Scar cc))
                               (body (cdr cc)))
                           (cons
                            (if (Sequal? (ast-con guard) empty)
                                'else
                                (untag-show (find! (ast-tvar guard))
                                            (tag-ast-show guard)))
                            (Smap2 tag-ast-show body))))
                       syntax-arg)))
      ((16) (cons 'case
                  (cons (tag-ast-show (car syntax-arg))
                        (Smap2 (lambda (cc) (set-bbv-version-limit! #f) 
                               (let ((data (car cc)))
                                 (if (and (pair? data)
                                          (Sequal? (ast-con (car data)) empty))
                                     (cons 'else
                                           (Smap2 tag-ast-show (cdr cc)))
                                     (cons (Smap2 datum-show data)
                                           (Smap2 tag-ast-show (cdr cc))))))
                             (cdr syntax-arg)))))
      ((17) (cons 'and (Smap2
                        (lambda (ast) (set-bbv-version-limit! #f) 
                          (let ((bool-tvar (find! (ast-tvar ast))))
                            (untag-show bool-tvar (tag-ast-show ast))))
                        syntax-arg)))
      ((18) (cons 'or (Smap2
                       (lambda (ast) (set-bbv-version-limit! #f) 
                         (let ((bool-tvar (find! (ast-tvar ast))))
                           (untag-show bool-tvar (tag-ast-show ast))))
                       syntax-arg)))
      ((19) (cons 'let
                  (cons (Smap3
                         (lambda (vd e) (set-bbv-version-limit! #f) 
                           (list (tag-ast-show vd) (tag-ast-show e)))
                         (Scaar syntax-arg)
                         (Scdar syntax-arg))
                        (Smap2 tag-ast-show (cdr syntax-arg)))))
      ((20) (cons 'let
                  (cons (tag-ast-show (Scar syntax-arg))
                        (cons (Smap3
                               (lambda (vd e) (set-bbv-version-limit! #f) 
                                 (list (tag-ast-show vd) (tag-ast-show e)))
                               (Scaadr syntax-arg)
                               (Scdadr syntax-arg))
                              (Smap2 tag-ast-show (Scddr syntax-arg))))))
      ((21) (cons 'let*
                  (cons (Smap3
                         (lambda (vd e) (set-bbv-version-limit! #f) 
                           (list (tag-ast-show vd) (tag-ast-show e)))
                         (Scaar syntax-arg)
                         (Scdar syntax-arg))
                        (Smap2 tag-ast-show (Scdr syntax-arg)))))
      ((22) (cons 'letrec
                  (cons (Smap3
                         (lambda (vd e) (set-bbv-version-limit! #f) 
                           (list (tag-ast-show vd) (tag-ast-show e)))
                         (Scaar syntax-arg)
                         (Scdar syntax-arg))
                        (Smap2 tag-ast-show (Scdr syntax-arg)))))
      ((23) (cons 'begin
                  (Smap2 tag-ast-show syntax-arg)))
      ((24) (fatal-error 'tag-ast-show "Do expressions not handled! (~s)" syntax-arg))
      ((25) (fatal-error 'tag-ast-show "This can't happen: empty encountered!"))
      ((26) (list 'define
                  (tag-ast-show (Scar syntax-arg))
                  (tag-ast-show (Scdr syntax-arg))))
      ((27) (let ((func-tvar (find! (ast-tvar (Scar syntax-arg)))))
              (list 'define
                    (tag-ast-show (Scar syntax-arg))
                    (tag-show func-tvar
                              (cons 'lambda
                                    (cons (tag-ast-show (Scadr syntax-arg))
                                          (Smap2 tag-ast-show (Scddr syntax-arg))))))))
      ((28) (cons 'begin
                  (Smap2 tag-ast-show syntax-arg)))
      (else (fatal-error 'tag-ast-show "Unknown abstract syntax operator: ~s"
                   syntax-op)))))


;; tag-ast*-show

(define (tag-ast*-show p) (set-bbv-version-limit! #f) 
  ;; display list of commands/expressions with tagging/untagging
  ;; operations
  (Smap2 tag-ast-show p))
;; ----------------------------------------------------------------------------
;; Top level type environment
;; ----------------------------------------------------------------------------


;; Needed packages: type management (monomorphic and polymorphic)

;;(load "typ-mgmt.ss")
;;(load "ptyp-mgm.ss")


;; type environment for miscellaneous

(define misc-env
  (list
   (cons 'quote (forall (lambda (tv) (set-bbv-version-limit! #f)  tv)))
   (cons 'Sequal? (forall (lambda (tv) (set-bbv-version-limit! #f)  (procedure (convert-tvars (list tv tv))
                                               (boolean)))))
   (cons 'eq? (forall (lambda (tv) (set-bbv-version-limit! #f)  (procedure (convert-tvars (list tv tv))
                                              (boolean)))))
   (cons 'equal? (forall (lambda (tv) (set-bbv-version-limit! #f)  (procedure (convert-tvars (list tv tv))
                                                 (boolean)))))
   ))

;; type environment for input/output

(define io-env
  (list
   (cons 'open-input-file (procedure (convert-tvars (list (charseq))) dynamic))
   (cons 'eof-object? (procedure (convert-tvars (list dynamic)) (boolean)))
   (cons 'read (forall (lambda (tv) (set-bbv-version-limit! #f) 
                         (procedure (convert-tvars (list tv)) dynamic))))
   (cons 'write (forall (lambda (tv) (set-bbv-version-limit! #f) 
                          (procedure (convert-tvars (list tv)) dynamic))))
   (cons 'display (forall (lambda (tv) (set-bbv-version-limit! #f) 
                            (procedure (convert-tvars (list tv)) dynamic))))
   (cons 'newline (procedure (null) dynamic))
   (cons 'pretty-print (forall (lambda (tv) (set-bbv-version-limit! #f) 
                                 (procedure (convert-tvars (list tv)) dynamic))))))


;; type environment for Booleans

(define boolean-env
  (list
   (cons 'boolean? (forall (lambda (tv) (set-bbv-version-limit! #f) 
                             (procedure (convert-tvars (list tv)) (boolean)))))
   ;;(cons #f (boolean))
   ;; #f doesn't exist in Chez Scheme, but gets mapped to null!
   (cons #t (boolean))
   (cons 'not (procedure (convert-tvars (list (boolean))) (boolean)))
   ))


;; type environment for pairs and lists

(define (list-type tv) (set-bbv-version-limit! #f) 
  (fix (lambda (tv2) (set-bbv-version-limit! #f)  (pair tv tv2))))

(define list-env
  (list
   (cons 'pair? (forall2 (lambda (tv1 tv2) (set-bbv-version-limit! #f) 
                           (procedure (convert-tvars (list (pair tv1 tv2)))
                                      (boolean)))))
   (cons 'null? (forall2 (lambda (tv1 tv2) (set-bbv-version-limit! #f) 
                           (procedure (convert-tvars (list (pair tv1 tv2)))
                                      (boolean)))))
   (cons 'list? (forall2 (lambda (tv1 tv2) (set-bbv-version-limit! #f) 
                           (procedure (convert-tvars (list (pair tv1 tv2)))
                                      (boolean)))))
   (cons 'cons (forall2 (lambda (tv1 tv2) (set-bbv-version-limit! #f) 
                          (procedure (convert-tvars (list tv1 tv2))
                                     (pair tv1 tv2)))))
   (cons 'car (forall2 (lambda (tv1 tv2) (set-bbv-version-limit! #f) 
                         (procedure (convert-tvars (list (pair tv1 tv2)))
                                    tv1))))
   (cons 'cdr (forall2 (lambda (tv1 tv2) (set-bbv-version-limit! #f) 
                         (procedure (convert-tvars (list (pair tv1 tv2)))
                                    tv2))))
   (cons 'set-car! (forall2 (lambda (tv1 tv2) (set-bbv-version-limit! #f) 
                              (procedure (convert-tvars (list (pair tv1 tv2)
                                                              tv1))
                                         dynamic))))
   (cons 'set-cdr! (forall2 (lambda (tv1 tv2) (set-bbv-version-limit! #f) 
                              (procedure (convert-tvars (list (pair tv1 tv2)
                                                              tv2))
                                         dynamic))))
   (cons 'caar (forall3 (lambda (tv1 tv2 tv3) (set-bbv-version-limit! #f) 
                          (procedure (convert-tvars
                                      (list (pair (pair tv1 tv2) tv3)))
                                     tv1))))
   (cons 'cdar (forall3 (lambda (tv1 tv2 tv3) (set-bbv-version-limit! #f) 
                          (procedure (convert-tvars
                                      (list (pair (pair tv1 tv2) tv3)))
                                     tv2))))

   (cons 'cadr (forall3 (lambda (tv1 tv2 tv3) (set-bbv-version-limit! #f) 
                          (procedure (convert-tvars
                                      (list (pair tv1 (pair tv2 tv3))))
                                     tv2))))
   (cons 'cddr (forall3 (lambda (tv1 tv2 tv3) (set-bbv-version-limit! #f) 
                          (procedure (convert-tvars
                                      (list (pair tv1 (pair tv2 tv3))))
                                     tv3))))
   (cons 'caaar (forall4
                 (lambda (tv1 tv2 tv3 tv4) (set-bbv-version-limit! #f) 
                   (procedure (convert-tvars
                               (list (pair (pair (pair tv1 tv2) tv3) tv4)))
                              tv1))))
   (cons 'cdaar (forall4
                 (lambda (tv1 tv2 tv3 tv4) (set-bbv-version-limit! #f) 
                   (procedure (convert-tvars
                               (list (pair (pair (pair tv1 tv2) tv3) tv4)))
                              tv2))))
   (cons 'cadar (forall4
                 (lambda (tv1 tv2 tv3 tv4) (set-bbv-version-limit! #f) 
                   (procedure (convert-tvars
                               (list (pair (pair tv1 (pair tv2 tv3)) tv4)))
                              tv2))))
   (cons 'cddar (forall4
                 (lambda (tv1 tv2 tv3 tv4) (set-bbv-version-limit! #f) 
                   (procedure (convert-tvars
                               (list (pair (pair tv1 (pair tv2 tv3)) tv4)))
                              tv3))))
   (cons 'caadr (forall4
                 (lambda (tv1 tv2 tv3 tv4) (set-bbv-version-limit! #f) 
                   (procedure (convert-tvars
                               (list (pair tv1 (pair (pair tv2 tv3) tv4))))
                              tv2))))
   (cons 'cdadr (forall4
                 (lambda (tv1 tv2 tv3 tv4) (set-bbv-version-limit! #f) 
                   (procedure (convert-tvars
                               (list (pair tv1 (pair (pair tv2 tv3) tv4))))
                              tv3))))
   (cons 'caddr (forall4
                 (lambda (tv1 tv2 tv3 tv4) (set-bbv-version-limit! #f) 
                   (procedure (convert-tvars
                               (list (pair tv1 (pair tv2 (pair tv3 tv4)))))
                              tv3))))
   (cons 'cdddr (forall4
                 (lambda (tv1 tv2 tv3 tv4) (set-bbv-version-limit! #f) 
                   (procedure (convert-tvars
                               (list (pair tv1 (pair tv2 (pair tv3 tv4)))))
                              tv4))))
   (cons 'cadddr
         (forall5 (lambda (tv1 tv2 tv3 tv4 tv5) (set-bbv-version-limit! #f) 
                    (procedure (convert-tvars
                                (list (pair tv1
                                            (pair tv2
                                                  (pair tv3
                                                        (pair tv4 tv5))))))
                               tv4))))
   (cons 'cddddr
         (forall5 (lambda (tv1 tv2 tv3 tv4 tv5) (set-bbv-version-limit! #f) 
                    (procedure (convert-tvars
                                (list (pair tv1
                                            (pair tv2
                                                  (pair tv3
                                                        (pair tv4 tv5))))))
                               tv5))))
   (cons 'list (forall (lambda (tv) (set-bbv-version-limit! #f) 
                         (procedure tv tv))))
   (cons 'length (forall (lambda (tv) (set-bbv-version-limit! #f) 
                           (procedure (convert-tvars (list (list-type tv)))
                                      (number)))))
   (cons 'append (forall (lambda (tv) (set-bbv-version-limit! #f) 
                           (procedure (convert-tvars (list (list-type tv)
                                                           (list-type tv)))
                                      (list-type tv)))))
   (cons 'reverse (forall (lambda (tv) (set-bbv-version-limit! #f) 
                            (procedure (convert-tvars (list (list-type tv)))
                                       (list-type tv)))))
   (cons 'list-ref (forall (lambda (tv) (set-bbv-version-limit! #f) 
                             (procedure (convert-tvars (list (list-type tv)
                                                             (number)))
                                        tv))))
   (cons 'memq (forall (lambda (tv) (set-bbv-version-limit! #f) 
                         (procedure (convert-tvars (list tv
                                                         (list-type tv)))
                                    (boolean)))))
   (cons 'memv (forall (lambda (tv) (set-bbv-version-limit! #f) 
                         (procedure (convert-tvars (list tv
                                                         (list-type tv)))
                                    (boolean)))))
   (cons 'member (forall (lambda (tv) (set-bbv-version-limit! #f) 
                           (procedure (convert-tvars (list tv
                                                           (list-type tv)))
                                      (boolean)))))
   (cons 'assq (forall2 (lambda (tv1 tv2) (set-bbv-version-limit! #f) 
                          (procedure (convert-tvars
                                      (list tv1
                                            (list-type (pair tv1 tv2))))
                                     (pair tv1 tv2)))))
   (cons 'assv (forall2 (lambda (tv1 tv2) (set-bbv-version-limit! #f) 
                          (procedure (convert-tvars
                                      (list tv1
                                            (list-type (pair tv1 tv2))))
                                     (pair tv1 tv2)))))
   (cons 'assoc (forall2 (lambda (tv1 tv2) (set-bbv-version-limit! #f) 
                           (procedure (convert-tvars
                                       (list tv1
                                             (list-type (pair tv1 tv2))))
                                      (pair tv1 tv2)))))
   ))


(define symbol-env
  (list
   (cons 'symbol? (forall (lambda (tv) (set-bbv-version-limit! #f) 
                            (procedure (convert-tvars (list tv)) (boolean)))))
   (cons 'symbol->string (procedure (convert-tvars (list (symbol))) (charseq)))
   (cons 'string->symbol (procedure (convert-tvars (list (charseq))) (symbol)))
   ))

(define number-env
  (list
   (cons 'number? (forall (lambda (tv) (set-bbv-version-limit! #f) 
                            (procedure (convert-tvars (list tv)) (boolean)))))
   (cons '+ (procedure (convert-tvars (list (number) (number))) (number)))
   (cons '- (procedure (convert-tvars (list (number) (number))) (number)))
   (cons '* (procedure (convert-tvars (list (number) (number))) (number)))
   (cons '/ (procedure (convert-tvars (list (number) (number))) (number)))
   (cons 'number->string (procedure (convert-tvars (list (number))) (charseq)))
   (cons 'string->number (procedure (convert-tvars (list (charseq))) (number)))
   ))

(define char-env
  (list
   (cons 'char? (forall (lambda (tv) (set-bbv-version-limit! #f) 
                          (procedure (convert-tvars (list tv)) (boolean)))))
   (cons 'char->integer (procedure (convert-tvars (list (character)))
                                   (number)))
   (cons 'integer->char (procedure (convert-tvars (list (number)))
                                   (character)))
   ))

(define string-env
  (list
   (cons 'string? (forall (lambda (tv) (set-bbv-version-limit! #f) 
                            (procedure (convert-tvars (list tv)) (boolean)))))
   ))

(define vector-env
  (list
   (cons 'vector? (forall (lambda (tv) (set-bbv-version-limit! #f) 
                            (procedure (convert-tvars (list tv)) (boolean)))))
   (cons 'make-vector (forall (lambda (tv) (set-bbv-version-limit! #f) 
                                (procedure (convert-tvars (list (number)))
                                           (array tv)))))
   (cons 'vector-length (forall (lambda (tv) (set-bbv-version-limit! #f) 
                                  (procedure (convert-tvars (list (array tv)))
                                             (number)))))
   (cons 'vector-ref (forall (lambda (tv) (set-bbv-version-limit! #f) 
                               (procedure (convert-tvars (list (array tv)
                                                               (number)))
                                          tv))))
   (cons 'vector-set! (forall (lambda (tv) (set-bbv-version-limit! #f) 
                                (procedure (convert-tvars (list (array tv)
                                                                (number)
                                                                tv))
                                           dynamic))))
   ))

(define procedure-env
  (list
   (cons 'procedure? (forall (lambda (tv) (set-bbv-version-limit! #f) 
                               (procedure (convert-tvars (list tv)) (boolean)))))
   (cons 'map (forall2 (lambda (tv1 tv2) (set-bbv-version-limit! #f) 
                         (procedure (convert-tvars
                                     (list (procedure (convert-tvars
                                                       (list tv1)) tv2)
                                           (list-type tv1)))
                                    (list-type tv2)))))
   (cons 'foreach (forall2 (lambda (tv1 tv2) (set-bbv-version-limit! #f) 
                             (procedure (convert-tvars
                                         (list (procedure (convert-tvars
                                                           (list tv1)) tv2)
                                               (list-type tv1)))
                                        (list-type tv2)))))
   (cons 'call-with-current-continuation
         (forall2 (lambda (tv1 tv2) (set-bbv-version-limit! #f) 
                    (procedure (convert-tvars
                                (list (procedure
                                       (convert-tvars
                                        (list (procedure (convert-tvars
                                                          (list tv1)) tv2)))
                                       tv2)))
                               tv2))))
   ))


;; global top level environment

(define (global-env) (set-bbv-version-limit! #f) 
  (LIBconcatenate
          (list
          misc-env
          io-env
          boolean-env
          symbol-env
          number-env
          char-env
          string-env
          vector-env
          procedure-env
          list-env)))

(define dynamic-top-level-env (global-env))

(define (init-dynamic-top-level-env!) (set-bbv-version-limit! #f) 
  (set! dynamic-top-level-env (global-env))
  '())

(define (dynamic-top-level-env-show) (set-bbv-version-limit! #f) 
  ;; displays the top level environment
  (Smap2 (lambda (binding) (set-bbv-version-limit! #f) 
         (cons (key-show (binding-key binding))
               (cons ': (tvar-show (binding-value binding)))))
       (env->list dynamic-top-level-env)))
;; ----------------------------------------------------------------------------
;; Dynamic type inference for Scheme
;; ----------------------------------------------------------------------------

;; Needed packages:

(define (ic!) (set-bbv-version-limit! #f)  (init-global-constraints!))
(define (pc) (set-bbv-version-limit! #f)  (glob-constr-show))
(define (lc) (set-bbv-version-limit! #f)  (Slength global-constraints))
(define (n!) (set-bbv-version-limit! #f)  (normalize-global-constraints!))
(define (pt) (set-bbv-version-limit! #f)  (dynamic-top-level-env-show))
(define (it!) (set-bbv-version-limit! #f)  (init-dynamic-top-level-env!))
(define (io!) (set-bbv-version-limit! #f)  (set! tag-ops 0) (set! no-ops 0))
(define (ib!) (set-bbv-version-limit! #f)  (set! read-buffer read-content))
(define (i!) (set-bbv-version-limit! #f)  (ic!) (it!) (io!) (ib!) '())

(define tag-ops 0)
(define no-ops 0)

(define-macro (init-content)
  (define (show x)
    (if #f (pp x))
    x)
  (define path
    (cond-expand
      (bigloo "../tests/paper/macro/dynamic.scm")
      (else "./dynamic.scm")))
  (show
    `(quote
      ,(let ((port (open-input-file path)))
        (let loop ()
          (let ((next (read port)))
            (cond
              ((eof-object? next) '())
              ((eq? (car next) 'define-macro) '()) ;; skip everything after this macro, parser fails on #!key
              (else (cons next (loop))))))))))

(define read-content (unknown (init-content)))
(define read-buffer read-content)
(define (my-read) (set-bbv-version-limit! #f) 
  (if (null? read-buffer)
      (eof-object)
      (let ((next (car read-buffer)))
        (set! read-buffer (cdr read-buffer))
        next)))

(define run1
  (lambda ()
    (i!)
    (let ((foo (dynamic-parse-file)))
      (normalize-global-constraints!)
      (reset-counters!)
      (tag-ast*-show foo)
      (counters-show))))

(define-keys (run !key (n (unknown 200 1)))
  (let loop ((n n) (result #f))
    (if (SFX> n 0)
        (loop (SFX- n 1) (run1))
	      result)))

(define (check result) (set-bbv-version-limit! #f) 
  (Sequal? result '((181 . 480) (6 . 1892) (2219 . 427))))
