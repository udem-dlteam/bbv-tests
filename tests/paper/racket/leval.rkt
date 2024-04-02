;*=====================================================================*/
;*    serrano/prgm/project/bbv-tests/tests/paper/macro/leval.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Dec 26 08:30:11 1998                          */
;*    Last change :  Thu Mar 21 14:45:54 2024 (serrano)                */
;*    -------------------------------------------------------------    */
;*    An interpreter with lambda (from M. Feeley's one).               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    Les environments ...                                             */
;*---------------------------------------------------------------------*/

(require racket/mpair)
(define cons mcons)
(define pair? mpair?)
(define list? mlist?)
(define car mcar)
(define cdr mcdr)
(define set-car! set-mcar!)
(define set-cdr! set-mcdr!)
(define list mlist)

(define-macro (mquote x)
  (cond 
	((pair? x) `(mlist ,@(map (lambda (sexp) `(mquote ,sexp)) x)))
	((vector? x) `(vector ,@(map (lambda (sexp) `(mquote ,sexp)) x)))
    (else `(quote ,x))))

(define the-global-environment '())

(define (errow . l)
   (for-each display l)
   (newline))

(define (local-assq obj alist)
   (let loop ((alist alist))
      (if (null? alist)
          #f
          (if (eq? (Smcar (Smcar alist)) obj)
              (Smcar alist)
              (loop (Smcdr alist))))))


;*---------------------------------------------------------------------*/
;*    ewal ...                                                         */
;*    sexp x env --> sexp                                              */
;*---------------------------------------------------------------------*/
(define (ewal exp)
   (meaning (comp exp '() #f) '()))

;*---------------------------------------------------------------------*/
;*    meaning ...                                                      */
;*---------------------------------------------------------------------*/
(define (meaning pre-compd-expression dynamic-env)
   (pre-compd-expression dynamic-env))

;*---------------------------------------------------------------------*/
;*    comp ...                                                         */
;*    s-exp x env x { t, f } --> (lambda () ...)                       */
;*    -------------------------------------------------------------    */
;*    La phase d'expansion a genere une syntaxe correcte. On n'a donc  */
;*    plus du tout a la tester maintenant.                             */
;*---------------------------------------------------------------------*/
(define (comp exp env tail?)
   (cond
      ((not (pair? exp))
       (let ((atom exp))
	  (cond
	     ((symbol? atom)
	      (comp-ref (variable atom env) tail?))
	     (else
	      (comp-cnst atom tail?)))))
      ((eq? (Smcar exp) 'module)
       (lambda (dynamic-env) #f))
      ((eq? (Smcar exp) 'quote)
       (let ((cnst (Smcadr exp)))
	  (comp-cnst cnst tail?)))
      ((eq? (Smcar exp) 'if)
       (let ((si (Smcadr exp))
	     (alors (Smcaddr exp))
	     (sinon (Smcadddr exp)))
	  (comp-if (comp si env #f)
	     (comp alors env tail?)
	     (comp sinon env tail?))))
      ((eq? (Smcar exp) 'begin)
       (let ((rest (Smcdr exp)))
	  (comp-begin rest env)))
      ((eq? (Smcar exp) 'define)
       (let ((var (Smcadr exp))
	     (val (Smcaddr exp)))
	  (comp-define var (comp val '() #f))))
      ((eq? (Smcar exp) 'set!)
       (let ((var (Smcadr exp))
	     (val (Smcaddr exp)))
	  (comp-set (variable var env) (comp val env #f))))
      ((eq? (Smcar exp) 'lambda)
       (let ((formals (Smcadr exp))
	     (body    (Smcaddr exp)))
	  (comp-lambda formals
	     (comp body (extend-env! formals env) #t)
	     tail?)))
      ((not (pair? (Smcar exp)))
       (let ((fun (Smcar exp))
	     (args (Smcdr exp)))
	  (let ((actuals (Smap2 (lambda (a) (comp a env #f)) args)))
	     (cond
		((symbol? fun)
		 (let ((proc (variable fun env)))
		    (cond
		       ((global? proc)
			(comp-global-application proc
			   actuals
			   tail?))
		       (else
			(comp-application (comp-ref proc #f)
			   actuals
			   tail?)))))
		((procedure? fun)
		 (comp-compd-application fun actuals tail?))
		(else
		 (errow "ewal" "Not a procedure" fun))))))
      (else
       (let ((fun (Smcar exp))
	     (args (Smcdr exp)))
	  (let ((actuals (Smap2 (lambda (a) (comp a env #f)) args))
		(proc    (comp fun env #f)))
	     (comp-application proc actuals tail?))))))

;*---------------------------------------------------------------------*/
;*    variable ...                                                     */
;*---------------------------------------------------------------------*/
(define (variable symbol env)
   (let ((offset (let loop ((env   env)
			    (count 0))
		    (cond
		       ((null? env)
			#f)
		       ((eq? (Smcar env) symbol)
			count)
		       (else
			(loop (Smcdr env) (SFX+ count 1)))))))
      (if offset
	  offset
	  (let ((global (local-assq symbol the-global-environment)))
	     (if (not global)
		 (vector symbol)
		 global)))))

;*---------------------------------------------------------------------*/
;*    global? ...                                                      */
;*---------------------------------------------------------------------*/
(define (global? variable)
   (pair? variable))

;*---------------------------------------------------------------------*/
;*    dynamic? ...                                                     */
;*---------------------------------------------------------------------*/
(define (dynamic? variable)
   (vector? variable))

;*---------------------------------------------------------------------*/
;*    comp-ref ...                                                     */
;*---------------------------------------------------------------------*/
(define (comp-ref variable tail?)
   (cond
      ((global? variable)
       (lambda (dynamic-env) (Smcdr variable)))
      ((dynamic? variable)
       (lambda (dynamic-env) (let ((global (local-assq (Svector-ref variable 0)
					      the-global-environment)))
				(if (not global)
				    (errow "ewal"
				       "Unbound variable"
				       (Svector-ref variable 0))
				    (Smcdr global)))))
      (else
       (case variable
	  ((0)
	   (lambda (dynamic-env) (Smcar dynamic-env)))
	  ((1)
	   (lambda (dynamic-env) (Smcadr dynamic-env)))
	  ((2)
	   (lambda (dynamic-env) (Smcaddr dynamic-env)))
	  ((3)
	   (lambda (dynamic-env) (Smcadddr dynamic-env)))
	  (else
	   (lambda (dynamic-env)
	      (do ((i 0 (SFX+ i 1))
		   (env dynamic-env (Smcdr env)))
		  ((SFX= i variable) (Smcar env)))))))))

;*---------------------------------------------------------------------*/
;*    comp-set ...                                                     */
;*---------------------------------------------------------------------*/
(define (comp-set variable value)
   (cond
      ((global? variable)
       (lambda (dynamic-env) (update! variable value dynamic-env)))
      ((dynamic? variable)
       (lambda (dynamic-env)
	  (let ((global (local-assq (Svector-ref variable 0)
			   the-global-environment)))
	     (if (not global)
		 (errow "ewal"
		    "Unbound variable"
		    (Svector-ref variable 0))
		 (update! global value dynamic-env)))))
      (else
       (case variable
	  ((0)
	   (lambda (dynamic-env) (Sset-mcar! dynamic-env
				    (value dynamic-env))))
	  ((1)
	   (lambda (dynamic-env) (Sset-mcar! (Smcdr dynamic-env)
				    (value dynamic-env))))
	  ((2)
	   (lambda (dynamic-env) (Sset-mcar! (Smcddr dynamic-env)
				    (value dynamic-env))))
	  ((3)
	   (lambda (dynamic-env) (Sset-mcar! (Smcdddr dynamic-env)
				    (value dynamic-env))))
	  (else
	   (lambda (dynamic-env)
	      (do ((i 0 (SFX+ i 1))
		   (env dynamic-env (Smcdr env)))
		  ((SFX= i variable) (Sset-mcar! env
					(value dynamic-env))))))))))
	
;*---------------------------------------------------------------------*/
;*    comp-cnst ...                                                    */
;*---------------------------------------------------------------------*/
(define (comp-cnst cnst tail?)
   (lambda (dynamic-env) cnst))

;*---------------------------------------------------------------------*/
;*    comp-if ...                                                      */
;*---------------------------------------------------------------------*/
(define (comp-if test then sinon)
   (lambda (dynamic-env)
      (if (test dynamic-env)
	  (then dynamic-env)
	  (sinon dynamic-env))))

;*---------------------------------------------------------------------*/
;*    comp-begin ...                                                   */
;*---------------------------------------------------------------------*/
(define (comp-begin body env)
   (cond
      ((and (pair? body) (and (null? (Smcdr body))))
       ;; le cas degenere
       (let ((rest (comp (Smcar body) env #t)))
	  (lambda (dynamic-env) (rest dynamic-env))))
      (else
       (let ((body (let loop ((rest body))
		      (cond
			 ((null? rest)
			  (errow "ewal" "Illegal form" body))
			 ((null? (Smcdr rest))
			  (cons (comp (Smcar rest) env #t) '()))
			 (else
			  (cons (comp (Smcar rest) env #f)
			     (loop (Smcdr rest))))))))
	  (lambda (dynamic-env) (let _loop_ ((body body))
				   (if (null? (Smcdr body))
				       ((Smcar body) dynamic-env)
				       (begin
					  ((Smcar body) dynamic-env)
					  (_loop_ (Smcdr body))))))))))

;*---------------------------------------------------------------------*/
;*    init-the-global-environment! ...                                 */
;*---------------------------------------------------------------------*/
(define (linit-the-global-environment!)
   (if (pair? the-global-environment)
       'done
       ;; je ne peux pas utiliser de constante car quand cette fonction
       ;; sera appelle, je ne suis pas qu'elles soient initialisee.
       (set! the-global-environment (mcons (mcons #f #f) '()))))

;*---------------------------------------------------------------------*/
;*    comp-define ...                                                  */
;*    -------------------------------------------------------------    */
;*    On ne rajoute pas en tete car elle contient la definition de     */
;*    `the-module-environment'. On rajoute donc en deuxieme.           */
;*---------------------------------------------------------------------*/
(define (comp-define var val)
   (lambda (dynamic-env)
      (let ((cell (local-assq var the-global-environment)))
	 (if (pair? cell)
	     (update! cell val dynamic-env)
	     (begin
		(Sset-mcdr! the-global-environment
		   (mcons (Smcar the-global-environment)
		      (Smcdr the-global-environment)))
		(Sset-mcar! the-global-environment
		   (mcons var (val dynamic-env)))
		var)))))

;*---------------------------------------------------------------------*/
;*    define-primitive! ...                                            */
;*    -------------------------------------------------------------    */
;*    Cette fonction est juste une forme abregee de la precedente, qui */
;*    construit le `(lambda () ...)' absent                            */
;*---------------------------------------------------------------------*/
(define (ldefine-primitive! var val)
   (Sset-mcdr! the-global-environment
      (mcons (Smcar the-global-environment)
	 (Smcdr the-global-environment)))
   (Sset-mcar! the-global-environment (mcons var val)))

;*---------------------------------------------------------------------*/
;*      update! ...                                                    */
;*---------------------------------------------------------------------*/
(define (update! variable val dynamic-env)
   (Sset-mcdr! variable (val dynamic-env))
   (Smcar variable))

;*---------------------------------------------------------------------*/
;*    extend-env! ...                                                  */
;*---------------------------------------------------------------------*/
(define (extend-env! extend old-env)
   (let _loop_ ((extend extend))
      (cond
	 ((null? extend)
	  old-env)
	 ((not (pair? extend))
	  (mcons extend old-env))
	 (else
	  (mcons (Smcar extend) (_loop_ (Smcdr extend)))))))

;*---------------------------------------------------------------------*/
;*    pair ...                                                         */
;*---------------------------------------------------------------------*/
(define (pair n l)
   (if (SFX< n 0)
       (let loop ((n n)
		  (l l))
	  (cond
	     ((SFX= -1 n)
	      #t)
	     ((not (pair? l))
	      #f)
	     (else
	      (loop (SFX+ 1 n) (Smcdr l)))))
       (let loop ((n n)
		  (l l))
	  (cond
	     ((SFX= 0 n)
	      (null? l))
	     ((not (pair? l))
	      #f)
	     (else
	      (loop (SFX- n 1) (Smcdr l)))))))
	      
;*---------------------------------------------------------------------*/
;*    comp-lambda ...                                                  */
;*---------------------------------------------------------------------*/
(define (comp-lambda formals body tail?)
   (cond
      ((null? formals)
       (lambda (dynamic-env)
	  (lambda ()
	     (body dynamic-env))))
      ((pair 1 formals)
       (lambda (dynamic-env)
	  (lambda (x)
	     (body (mcons x dynamic-env)))))
      ((pair 2 formals)
       (lambda (dynamic-env)
	  (lambda (x y)
	     (body (mcons x (mcons y dynamic-env))))))
      ((pair 3 formals)
       (lambda (dynamic-env)
	  (lambda (x y z)
	     (body (mcons x (mcons y (mcons z dynamic-env)))))))
      ((pair 4 formals)
       (lambda (dynamic-env)
	  (lambda (x y z t)
	     (body (mcons x (mcons y (mcons z (mcons z dynamic-env))))))))
      ((symbol? formals)
       (lambda (dynamic-env)
	  (lambda x
	     (body (mcons x dynamic-env)))))
      ((pair -1 formals)
       (lambda (dynamic-env)
	  (lambda (x . y)
	     (body (mcons x (mcons y dynamic-env))))))
      ((pair -2 formals)
       (lambda (dynamic-env)
	  (lambda (x y . z)
	     (body (mcons x (mcons y (mcons z dynamic-env)))))))
      ((pair -3 formals)
       (lambda (dynamic-env)
	  (lambda (x y z . t)
	     (body (mcons x (mcons y (mcons z (mcons z dynamic-env))))))))
      (else
       (lambda (dynamic-env)
	  (lambda x
	     (let ((new-env (let _loop_ ((actuals x)
					 (formals formals))
			       (cond
				  ((null? formals)
				   (if (not (null? actuals))
				       (errow "ewal"
					  "Too many arguments provided"
					  actuals)
				       dynamic-env))
				  ((null? actuals)
				   (errow "ewal"
				      "Too fee arguments provided"
				      formals))
				  ((not (pair? formals))
				   (mcons actuals dynamic-env))
				  (else
				   (mcons (Smcar actuals)
				      (_loop_ (Smcdr actuals)
					 (Smcdr formals))))))))
		(body new-env)))))))

;*---------------------------------------------------------------------*/
;*    comp-global-application ...                                      */
;*---------------------------------------------------------------------*/
(define (comp-global-application proc actuals tail?)
   (case (Slength actuals)
      ((0)
       (lambda (dynamic-env) ((Smcdr proc))))
      ((1)
       (lambda (dynamic-env) ((Smcdr proc) ((Smcar actuals) dynamic-env))))
      ((2)
       (lambda (dynamic-env) ((Smcdr proc) ((Smcar actuals) dynamic-env)
					  ((Smcadr actuals) dynamic-env))))
      ((3)
       (lambda (dynamic-env) ((Smcdr proc) ((Smcar actuals) dynamic-env)
					  ((Smcadr actuals) dynamic-env)
					  ((Smcaddr actuals) dynamic-env))))
      ((4)
       (lambda (dynamic-env) ((Smcdr proc) ((Smcar actuals) dynamic-env)
					  ((Smcadr actuals) dynamic-env)
					  ((Smcaddr actuals) dynamic-env)
					  ((Smcadddr actuals) dynamic-env))))
      (else
       (lambda (dynamic-env)
	  (apply (Smcdr proc) (Smap2 (lambda (v) (v dynamic-env)) actuals))))))

;*---------------------------------------------------------------------*/
;*    comp-compd-application ...                                       */
;*---------------------------------------------------------------------*/
(define (comp-compd-application proc actuals tail?)
   (case (Slength actuals)
      ((0)
       (lambda (dynamic-env) (proc)))
      ((1)
       (lambda (dynamic-env) (proc ((Smcar actuals) dynamic-env))))
      ((2)
       (lambda (dynamic-env) (proc ((Smcar actuals) dynamic-env)
				((Smcadr actuals) dynamic-env))))
      ((3)
       (lambda (dynamic-env) (proc ((Smcar actuals) dynamic-env)
				((Smcadr actuals) dynamic-env)
				((Smcaddr actuals) dynamic-env))))
      ((4)
       (lambda (dynamic-env) (proc ((Smcar actuals) dynamic-env)
				((Smcadr actuals) dynamic-env)
				((Smcaddr actuals) dynamic-env)
				((Smcadddr actuals) dynamic-env))))
      (else
       (lambda (dynamic-env)
	  (apply proc (Smap2 (lambda (v) (v dynamic-env)) actuals))))))
   
;*---------------------------------------------------------------------*/
;*    comp-application ...                                             */
;*---------------------------------------------------------------------*/
(define (comp-application proc actuals tail?)
   (case (Slength actuals)
      ((0)
       (lambda (dynamic-env) ((proc dynamic-env))))
      ((1)
       (lambda (dynamic-env) ((proc dynamic-env)
			      ((Smcar actuals) dynamic-env))))
      ((2)
       (lambda (dynamic-env) ((proc dynamic-env)
			      ((Smcar actuals) dynamic-env)
			      ((Smcadr actuals) dynamic-env))))
      ((3)
       (lambda (dynamic-env) ((proc dynamic-env)
			      ((Smcar actuals) dynamic-env)
			      ((Smcadr actuals) dynamic-env)
			      ((Smcaddr actuals) dynamic-env))))
      ((4)
       (lambda (dynamic-env) ((proc dynamic-env)
			      ((Smcar actuals) dynamic-env)
			      ((Smcadr actuals) dynamic-env)
			      ((Smcaddr actuals) dynamic-env)
			      ((Smcadddr actuals) dynamic-env))))
      (else
       (lambda (dynamic-env)
	  (apply (proc dynamic-env) (Smap2 (lambda (v) (v dynamic-env))
				       actuals))))))

;*---------------------------------------------------------------------*/
;*    Les inits                                                        */
;*---------------------------------------------------------------------*/
(linit-the-global-environment!)

(ldefine-primitive! '+ (lambda (x y) (SFX+ x y)))
(ldefine-primitive! '- (lambda (x y) (SFX- x y)))
(ldefine-primitive! '< (lambda (x y) (SFX< x y)))
(ldefine-primitive! '>= (lambda (x y) (SFX>= x y)))

;*---------------------------------------------------------------------*/
;*    cnt                                                              */
;*---------------------------------------------------------------------*/
(define cnt (mquote (define cnt 0)))
(define cntref 'cnt)

;*---------------------------------------------------------------------*/
;*    tak ...                                                          */
;*---------------------------------------------------------------------*/
(define tak
   (mquote (define tak (lambda (x y z)
		   (begin
		      (set! cnt (+ cnt 1))
		      (if (>= y x)
			  z
			  (tak (tak (- x 1) y z)
			     (tak (- y 1) z x)
			     (tak (- z 1) x y))))))))
(define takcall
   (unknown (mquote (tak 20 10 3)) (mquote (tak 10 5 3))))

;*---------------------------------------------------------------------*/
;*    fib ...                                                          */
;*---------------------------------------------------------------------*/
(define fib
   (mquote (define fib (lambda (x)
		   (begin
		      (set! cnt (+ cnt 1))
		      (if (< x 2)
			  1
			  (+ (fib (- x 1)) (fib (- x 2)))))))))
(define fibcall
	(unknown (mquote (fib 20)) (mquote (fib 10))))

;*---------------------------------------------------------------------*/
;*    run                                                              */
;*---------------------------------------------------------------------*/
(define-keys (run !key (n (unknown 60 1)))
   
   (define (test)
      (ewal cnt)
      (ewal tak)
      (ewal fib)
      (vector (ewal takcall) (ewal fibcall) (ewal cntref)))
   
   (let loop ((n n) (result #f))
      (if (SFX> n 0)
	  (loop (SFX- n 1) (test))
	  result)))

;*---------------------------------------------------------------------*/
;*    check ...                                                        */
;*---------------------------------------------------------------------*/
(define (check result)
   ;(print "result=" result)
   (equal? result (unknown (vector 4 10946 1452256) (vector 5 89 366))))
