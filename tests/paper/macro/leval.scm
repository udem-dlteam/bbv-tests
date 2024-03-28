;*=====================================================================*/
;*    serrano/prgm/project/bbv-tests/tests/paper/macro/leval.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Dec 26 08:30:11 1998                          */
;*    Last change :  Mon Mar 25 15:04:03 2024 (serrano)                */
;*    -------------------------------------------------------------    */
;*    An interpreter with lambda (from M. Feeley's one).               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    Les environments ...                                             */
;*---------------------------------------------------------------------*/
(define the-global-environment '())

(define (errow . l) (set-bbv-version-limit! #f) 
   (for-each display l)
   (newline))

(define (local-assq obj alist) (set-bbv-version-limit! #f) 
   (let loop ((alist alist))
      (if (null? alist)
          #f
          (if (eq? (Scar (Scar alist)) obj)
              (Scar alist)
              (loop (Scdr alist))))))


;*---------------------------------------------------------------------*/
;*    ewal ...                                                         */
;*    sexp x env --> sexp                                              */
;*---------------------------------------------------------------------*/
(define (ewal exp) (set-bbv-version-limit! #f) 
   (meaning (comp exp '() #f) '()))

;*---------------------------------------------------------------------*/
;*    meaning ...                                                      */
;*---------------------------------------------------------------------*/
(define (meaning pre-compd-expression dynamic-env) (set-bbv-version-limit! #f) 
   (pre-compd-expression dynamic-env))

;*---------------------------------------------------------------------*/
;*    comp ...                                                         */
;*    s-exp x env x { t, f } --> (lambda () ...)                       */
;*    -------------------------------------------------------------    */
;*    La phase d'expansion a genere une syntaxe correcte. On n'a donc  */
;*    plus du tout a la tester maintenant.                             */
;*---------------------------------------------------------------------*/
(define (comp exp env tail?) (set-bbv-version-limit! #f) 
   (cond
      ((not (pair? exp))
       (let ((atom exp))
	  (cond
	     ((symbol? atom)
	      (comp-ref (variable atom env) tail?))
	     (else
	      (comp-cnst atom tail?)))))
      ((eq? (Scar exp) 'module)
       (lambda (dynamic-env) (set-bbv-version-limit! #f)  #f))
      ((eq? (Scar exp) 'quote)
       (let ((cnst (Scadr exp)))
	  (comp-cnst cnst tail?)))
      ((eq? (Scar exp) 'if)
       (let ((si (Scadr exp))
	     (alors (Scaddr exp))
	     (sinon (Scadddr exp)))
	  (comp-if (comp si env #f)
	     (comp alors env tail?)
	     (comp sinon env tail?))))
      ((eq? (Scar exp) 'begin)
       (let ((rest (Scdr exp)))
	  (comp-begin rest env)))
      ((eq? (Scar exp) 'define)
       (let ((var (Scadr exp))
	     (val (Scaddr exp)))
	  (comp-define var (comp val '() #f))))
      ((eq? (Scar exp) 'set!)
       (let ((var (Scadr exp))
	     (val (Scaddr exp)))
	  (comp-set (variable var env) (comp val env #f))))
      ((eq? (Scar exp) 'lambda)
       (let ((formals (Scadr exp))
	     (body    (Scaddr exp)))
	  (comp-lambda formals
	     (comp body (extend-env! formals env) #t)
	     tail?)))
      ((not (pair? (Scar exp)))
       (let ((fun (Scar exp))
	     (args (Scdr exp)))
	  (let ((actuals (Smap2 (lambda (a) (set-bbv-version-limit! #f)  (comp a env #f)) args)))
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
       (let ((fun (Scar exp))
	     (args (Scdr exp)))
	  (let ((actuals (Smap2 (lambda (a) (set-bbv-version-limit! #f)  (comp a env #f)) args))
		(proc    (comp fun env #f)))
	     (comp-application proc actuals tail?))))))

;*---------------------------------------------------------------------*/
;*    variable ...                                                     */
;*---------------------------------------------------------------------*/
(define (variable symbol env) (set-bbv-version-limit! #f) 
   (let ((offset (let loop ((env   env)
			    (count 0))
		    (cond
		       ((null? env)
			#f)
		       ((eq? (Scar env) symbol)
			count)
		       (else
			(loop (Scdr env) (SFX+ count 1)))))))
      (if offset
	  offset
	  (let ((global (local-assq symbol the-global-environment)))
	     (if (not global)
		 `#(,symbol)
		 global)))))

;*---------------------------------------------------------------------*/
;*    global? ...                                                      */
;*---------------------------------------------------------------------*/
(define (global? variable) (set-bbv-version-limit! #f) 
   (pair? variable))

;*---------------------------------------------------------------------*/
;*    dynamic? ...                                                     */
;*---------------------------------------------------------------------*/
(define (dynamic? variable) (set-bbv-version-limit! #f) 
   (vector? variable))

;*---------------------------------------------------------------------*/
;*    comp-ref ...                                                     */
;*---------------------------------------------------------------------*/
(define (comp-ref variable tail?) (set-bbv-version-limit! #f) 
   (cond
      ((global? variable)
       (lambda (dynamic-env) (set-bbv-version-limit! #f)  (Scdr variable)))
      ((dynamic? variable)
       (lambda (dynamic-env) (set-bbv-version-limit! #f)  (let ((global (local-assq (Svector-ref variable 0)
					      the-global-environment)))
				(if (not global)
				    (errow "ewal"
				       "Unbound variable"
				       (Svector-ref variable 0))
				    (Scdr global)))))
      (else
       (case variable
	  ((0)
	   (lambda (dynamic-env) (set-bbv-version-limit! #f)  (Scar dynamic-env)))
	  ((1)
	   (lambda (dynamic-env) (set-bbv-version-limit! #f)  (Scadr dynamic-env)))
	  ((2)
	   (lambda (dynamic-env) (set-bbv-version-limit! #f)  (Scaddr dynamic-env)))
	  ((3)
	   (lambda (dynamic-env) (set-bbv-version-limit! #f)  (Scadddr dynamic-env)))
	  (else
	   (lambda (dynamic-env) (set-bbv-version-limit! #f) 
	      (do ((i 0 (SFX+ i 1))
		   (env dynamic-env (Scdr env)))
		  ((SFX= i variable) (Scar env)))))))))

;*---------------------------------------------------------------------*/
;*    comp-set ...                                                     */
;*---------------------------------------------------------------------*/
(define (comp-set variable value) (set-bbv-version-limit! #f) 
   (cond
      ((global? variable)
       (lambda (dynamic-env) (set-bbv-version-limit! #f)  (update! variable value dynamic-env)))
      ((dynamic? variable)
       (lambda (dynamic-env) (set-bbv-version-limit! #f) 
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
	   (lambda (dynamic-env) (set-bbv-version-limit! #f)  (Sset-car! dynamic-env
				    (value dynamic-env))))
	  ((1)
	   (lambda (dynamic-env) (set-bbv-version-limit! #f)  (Sset-car! (Scdr dynamic-env)
				    (value dynamic-env))))
	  ((2)
	   (lambda (dynamic-env) (set-bbv-version-limit! #f)  (Sset-car! (Scddr dynamic-env)
				    (value dynamic-env))))
	  ((3)
	   (lambda (dynamic-env) (set-bbv-version-limit! #f)  (Sset-car! (Scdddr dynamic-env)
				    (value dynamic-env))))
	  (else
	   (lambda (dynamic-env) (set-bbv-version-limit! #f) 
	      (do ((i 0 (SFX+ i 1))
		   (env dynamic-env (Scdr env)))
		  ((SFX= i variable) (Sset-car! env
					(value dynamic-env))))))))))
	
;*---------------------------------------------------------------------*/
;*    comp-cnst ...                                                    */
;*---------------------------------------------------------------------*/
(define (comp-cnst cnst tail?) (set-bbv-version-limit! #f) 
   (lambda (dynamic-env) (set-bbv-version-limit! #f)  cnst))

;*---------------------------------------------------------------------*/
;*    comp-if ...                                                      */
;*---------------------------------------------------------------------*/
(define (comp-if test then sinon) (set-bbv-version-limit! #f) 
   (lambda (dynamic-env) (set-bbv-version-limit! #f) 
      (if (test dynamic-env)
	  (then dynamic-env)
	  (sinon dynamic-env))))

;*---------------------------------------------------------------------*/
;*    comp-begin ...                                                   */
;*---------------------------------------------------------------------*/
(define (comp-begin body env) (set-bbv-version-limit! #f) 
   (cond
      ((and (pair? body) (and (null? (Scdr body))))
       ;; le cas degenere
       (let ((rest (comp (Scar body) env #t)))
	  (lambda (dynamic-env) (set-bbv-version-limit! #f)  (rest dynamic-env))))
      (else
       (let ((body (let loop ((rest body))
		      (cond
			 ((null? rest)
			  (errow "ewal" "Illegal form" body))
			 ((null? (Scdr rest))
			  (cons (comp (Scar rest) env #t) '()))
			 (else
			  (cons (comp (Scar rest) env #f)
			     (loop (Scdr rest))))))))
	  (lambda (dynamic-env) (set-bbv-version-limit! #f)  (let _loop_ ((body body))
				   (if (null? (Scdr body))
				       ((Scar body) dynamic-env)
				       (begin
					  ((Scar body) dynamic-env)
					  (_loop_ (Scdr body))))))))))

;*---------------------------------------------------------------------*/
;*    init-the-global-environment! ...                                 */
;*---------------------------------------------------------------------*/
(define (linit-the-global-environment!) (set-bbv-version-limit! #f) 
   (if (pair? the-global-environment)
       'done
       ;; je ne peux pas utiliser de constante car quand cette fonction
       ;; sera appelle, je ne suis pas qu'elles soient initialisee.
       (set! the-global-environment (cons (cons #f #f) '()))))

;*---------------------------------------------------------------------*/
;*    comp-define ...                                                  */
;*    -------------------------------------------------------------    */
;*    On ne rajoute pas en tete car elle contient la definition de     */
;*    `the-module-environment'. On rajoute donc en deuxieme.           */
;*---------------------------------------------------------------------*/
(define (comp-define var val) (set-bbv-version-limit! #f) 
   (lambda (dynamic-env) (set-bbv-version-limit! #f) 
      (let ((cell (local-assq var the-global-environment)))
	 (if (pair? cell)
	     (update! cell val dynamic-env)
	     (begin
		(Sset-cdr! the-global-environment
		   (cons (Scar the-global-environment)
		      (Scdr the-global-environment)))
		(Sset-car! the-global-environment
		   (cons var (val dynamic-env)))
		var)))))

;*---------------------------------------------------------------------*/
;*    define-primitive! ...                                            */
;*    -------------------------------------------------------------    */
;*    Cette fonction est juste une forme abregee de la precedente, qui */
;*    construit le `(lambda () ...)' absent                            */
;*---------------------------------------------------------------------*/
(define (ldefine-primitive! var val) (set-bbv-version-limit! #f) 
   (Sset-cdr! the-global-environment
      (cons (Scar the-global-environment)
	 (Scdr the-global-environment)))
   (Sset-car! the-global-environment (cons var val)))

;*---------------------------------------------------------------------*/
;*      update! ...                                                    */
;*---------------------------------------------------------------------*/
(define (update! variable val dynamic-env) (set-bbv-version-limit! #f) 
   (Sset-cdr! variable (val dynamic-env))
   (Scar variable))

;*---------------------------------------------------------------------*/
;*    extend-env! ...                                                  */
;*---------------------------------------------------------------------*/
(define (extend-env! extend old-env) (set-bbv-version-limit! #f) 
   (let _loop_ ((extend extend))
      (cond
	 ((null? extend)
	  old-env)
	 ((not (pair? extend))
	  (cons extend old-env))
	 (else
	  (cons (Scar extend) (_loop_ (Scdr extend)))))))

;*---------------------------------------------------------------------*/
;*    pair ...                                                         */
;*---------------------------------------------------------------------*/
(define (pair n l) (set-bbv-version-limit! #f) 
   (if (SFX< n 0)
       (let loop ((n n)
		  (l l))
	  (cond
	     ((SFX= -1 n)
	      #t)
	     ((not (pair? l))
	      #f)
	     (else
	      (loop (SFX+ 1 n) (Scdr l)))))
       (let loop ((n n)
		  (l l))
	  (cond
	     ((SFX= 0 n)
	      (null? l))
	     ((not (pair? l))
	      #f)
	     (else
	      (loop (SFX- n 1) (Scdr l)))))))
	      
;*---------------------------------------------------------------------*/
;*    comp-lambda ...                                                  */
;*---------------------------------------------------------------------*/
(define (comp-lambda formals body tail?) (set-bbv-version-limit! #f) 
   (cond
      ((null? formals)
       (lambda (dynamic-env) (set-bbv-version-limit! #f) 
	  (lambda ()
	     (body dynamic-env))))
      ((pair 1 formals)
       (lambda (dynamic-env) (set-bbv-version-limit! #f) 
	  (lambda (x) (set-bbv-version-limit! #f) 
	     (body (cons x dynamic-env)))))
      ((pair 2 formals)
       (lambda (dynamic-env) (set-bbv-version-limit! #f) 
	  (lambda (x y) (set-bbv-version-limit! #f) 
	     (body (cons x (cons y dynamic-env))))))
      ((pair 3 formals)
       (lambda (dynamic-env) (set-bbv-version-limit! #f) 
	  (lambda (x y z) (set-bbv-version-limit! #f) 
	     (body (cons x (cons y (cons z dynamic-env)))))))
      ((pair 4 formals)
       (lambda (dynamic-env) (set-bbv-version-limit! #f) 
	  (lambda (x y z t) (set-bbv-version-limit! #f) 
	     (body (cons x (cons y (cons z (cons z dynamic-env))))))))
      ((symbol? formals)
       (lambda (dynamic-env) (set-bbv-version-limit! #f) 
	  (lambda x
	     (body (cons x dynamic-env)))))
      ((pair -1 formals)
       (lambda (dynamic-env) (set-bbv-version-limit! #f) 
	  (lambda (x . y) (set-bbv-version-limit! #f) 
	     (body (cons x (cons y dynamic-env))))))
      ((pair -2 formals)
       (lambda (dynamic-env) (set-bbv-version-limit! #f) 
	  (lambda (x y . z) (set-bbv-version-limit! #f) 
	     (body (cons x (cons y (cons z dynamic-env)))))))
      ((pair -3 formals)
       (lambda (dynamic-env) (set-bbv-version-limit! #f) 
	  (lambda (x y z . t) (set-bbv-version-limit! #f) 
	     (body (cons x (cons y (cons z (cons z dynamic-env))))))))
      (else
       (lambda (dynamic-env) (set-bbv-version-limit! #f) 
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
				   (cons actuals dynamic-env))
				  (else
				   (cons (Scar actuals)
				      (_loop_ (Scdr actuals)
					 (Scdr formals))))))))
		(body new-env)))))))

;*---------------------------------------------------------------------*/
;*    comp-global-application ...                                      */
;*---------------------------------------------------------------------*/
(define (comp-global-application proc actuals tail?) (set-bbv-version-limit! #f) 
   (case (Slength actuals)
      ((0)
       (lambda (dynamic-env) (set-bbv-version-limit! #f)  ((Scdr proc))))
      ((1)
       (lambda (dynamic-env) (set-bbv-version-limit! #f)  ((Scdr proc) ((Scar actuals) dynamic-env))))
      ((2)
       (lambda (dynamic-env) (set-bbv-version-limit! #f)  ((Scdr proc) ((Scar actuals) dynamic-env)
					  ((Scadr actuals) dynamic-env))))
      ((3)
       (lambda (dynamic-env) (set-bbv-version-limit! #f) 
	  ((Scdr proc) ((Scar actuals) dynamic-env)
					  ((Scadr actuals) dynamic-env)
					  ((Scaddr actuals) dynamic-env))))
      ((4)
       (lambda (dynamic-env) (set-bbv-version-limit! #f)  ((Scdr proc) ((Scar actuals) dynamic-env)
					  ((Scadr actuals) dynamic-env)
					  ((Scaddr actuals) dynamic-env)
					  ((Scadddr actuals) dynamic-env))))
      (else
       (lambda (dynamic-env) (set-bbv-version-limit! #f) 
	  (apply (Scdr proc) (Smap2 (lambda (v) (set-bbv-version-limit! #f)  (v dynamic-env)) actuals))))))

;*---------------------------------------------------------------------*/
;*    comp-compd-application ...                                       */
;*---------------------------------------------------------------------*/
(define (comp-compd-application proc actuals tail?) (set-bbv-version-limit! #f) 
   (case (Slength actuals)
      ((0)
       (lambda (dynamic-env) (set-bbv-version-limit! #f)  (proc)))
      ((1)
       (lambda (dynamic-env) (set-bbv-version-limit! #f)  (proc ((Scar actuals) dynamic-env))))
      ((2)
       (lambda (dynamic-env) (set-bbv-version-limit! #f)  (proc ((Scar actuals) dynamic-env)
				((Scadr actuals) dynamic-env))))
      ((3)
       (lambda (dynamic-env) (set-bbv-version-limit! #f) 
	  (proc ((Scar actuals) dynamic-env)
				((Scadr actuals) dynamic-env)
				((Scaddr actuals) dynamic-env))))
      ((4)
       (lambda (dynamic-env) (set-bbv-version-limit! #f)  (proc ((Scar actuals) dynamic-env)
				((Scadr actuals) dynamic-env)
				((Scaddr actuals) dynamic-env)
				((Scadddr actuals) dynamic-env))))
      (else
       (lambda (dynamic-env) (set-bbv-version-limit! #f) 
	  (apply proc (Smap2 (lambda (v) (set-bbv-version-limit! #f)  (v dynamic-env)) actuals))))))
   
;*---------------------------------------------------------------------*/
;*    comp-application ...                                             */
;*---------------------------------------------------------------------*/
(define (comp-application proc actuals tail?) (set-bbv-version-limit! #f) 
   (case (Slength actuals)
      ((0)
       (lambda (dynamic-env) (set-bbv-version-limit! #f)  ((proc dynamic-env))))
      ((1)
       (lambda (dynamic-env) (set-bbv-version-limit! #f)  ((proc dynamic-env)
			      ((Scar actuals) dynamic-env))))
      ((2)
       (lambda (dynamic-env) (set-bbv-version-limit! #f)  ((proc dynamic-env)
			      ((Scar actuals) dynamic-env)
			      ((Scadr actuals) dynamic-env))))
      ((3)
       (lambda (dynamic-env) (set-bbv-version-limit! #f) 
	  ((proc dynamic-env)
			      ((Scar actuals) dynamic-env)
			      ((Scadr actuals) dynamic-env)
			      ((Scaddr actuals) dynamic-env))))
      ((4)
       (lambda (dynamic-env) (set-bbv-version-limit! #f)  ((proc dynamic-env)
			      ((Scar actuals) dynamic-env)
			      ((Scadr actuals) dynamic-env)
			      ((Scaddr actuals) dynamic-env)
			      ((Scadddr actuals) dynamic-env))))
      (else
       (lambda (dynamic-env) (set-bbv-version-limit! #f) 
	  (apply (proc dynamic-env) (Smap2 (lambda (v) (set-bbv-version-limit! #f)  (v dynamic-env))
				       actuals))))))

;*---------------------------------------------------------------------*/
;*    Les inits                                                        */
;*---------------------------------------------------------------------*/
(linit-the-global-environment!)

(ldefine-primitive! '+ (lambda (x y) (set-bbv-version-limit! #f)  (SFX+ x y)))
(ldefine-primitive! '- (lambda (x y) (set-bbv-version-limit! #f)  (SFX- x y)))
(ldefine-primitive! '< (lambda (x y) (set-bbv-version-limit! #f)  (SFX< x y)))
(ldefine-primitive! '>= (lambda (x y) (set-bbv-version-limit! #f)  (SFX>= x y)))

;*---------------------------------------------------------------------*/
;*    cnt                                                              */
;*---------------------------------------------------------------------*/
(define cnt '(define cnt 0))
(define cntref 'cnt)

;*---------------------------------------------------------------------*/
;*    tak ...                                                          */
;*---------------------------------------------------------------------*/
(define tak
   '(define tak (lambda (x y z)
		   (begin
		      (set! cnt (+ cnt 1))
		      (if (>= y x)
			  z
			  (tak (tak (- x 1) y z)
			     (tak (- y 1) z x)
			     (tak (- z 1) x y)))))))
(define takcall
   (unknown '(tak 20 10 3) '(tak 10 5 3)))

;*---------------------------------------------------------------------*/
;*    fib ...                                                          */
;*---------------------------------------------------------------------*/
(define fib
   '(define fib (lambda (x)
		   (begin
		      (set! cnt (+ cnt 1))
		      (if (< x 2)
			  1
			  (+ (fib (- x 1)) (fib (- x 2))))))))
(define fibcall
	(unknown '(fib 20) '(fib 10)))

;*---------------------------------------------------------------------*/
;*    run                                                              */
;*---------------------------------------------------------------------*/
(define-keys (run !key (n (unknown 60 1)))
   
   (define (test) (set-bbv-version-limit! #f) 
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
(define (check result) (set-bbv-version-limit! #f) 
   ;(print "result=" result)
   (equal? result (unknown '#(4 10946 1452256) '#(5 89 366))))
