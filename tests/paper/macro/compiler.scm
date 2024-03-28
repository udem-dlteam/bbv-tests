;(define integer->char ascii->char)
;(define char->integer char->ascii)

(define open-input-file* open-input-file)
(define (pp-expression expr port) (set-bbv-version-limit! #f)  (write expr port) (newline port))
(define (write-returning-len obj port) (set-bbv-version-limit! #f)  (write obj port) 1)
(define (display-returning-len obj port) (set-bbv-version-limit! #f)  (display obj port) 1)
(define (write-word w port) (set-bbv-version-limit! #f) 
  (write-char (Sinteger->char (SFXquotient w 256)) port)
  (write-char (Sinteger->char (SFXmodulo w 256)) port))
(define char-nul (Sinteger->char 0))
(define char-tab (Sinteger->char 9))
(define char-newline (Sinteger->char 10))
(define character-encoding (lambda (c) (set-bbv-version-limit! #f)  (Schar->integer c)))
(define max-character-encoding 255)
(define (fatal-err msg arg) (set-bbv-version-limit! #f)  (cond-expand (bigloo (error 'compiler msg arg)) (else (fatal-error msg arg))))
(define (scheme-global-var name) (set-bbv-version-limit! #f)  name)
(define (scheme-global-var-ref var) (set-bbv-version-limit! #f)  (scheme-global-eval var fatal-err))
(define (scheme-global-var-set! var val) (set-bbv-version-limit! #f) 
  (scheme-global-eval (list 'set! var (list 'quote val)) fatal-err))
(define (scheme-global-eval expr err) (set-bbv-version-limit! #f)  `(eval ,expr)) ;; eval not needed for test
(define (pinpoint-error filename line char) (set-bbv-version-limit! #f)  #t)
(define file-path-sep #\:)
(define file-ext-sep #\.)
(define (path-absolute? x) (set-bbv-version-limit! #f) 
  (and (SFX> (Sstring-length x) 0)
       (let ((c (Sstring-ref x 0))) (or (Schar=? c #\/) (Schar=? c #\~)))))
(define (file-path x) (set-bbv-version-limit! #f) 
  (let loop1 ((i (Sstring-length x)))
    (if (and (SFX> i 0) (not (Schar=? (Sstring-ref x (SFX- i 1)) file-path-sep)))
        (loop1 (SFX- i 1))
        (let ((result (Smake-string1 i)))
          (let loop2 ((j (SFX- i 1)))
            (if (SFX< j 0)
                result
                (begin
                  (Sstring-set! result j (Sstring-ref x j))
                  (loop2 (SFX- j 1)))))))))
(define (file-name x) (set-bbv-version-limit! #f) 
  (let loop1 ((i (Sstring-length x)))
    (if (and (SFX> i 0) (not (Schar=? (Sstring-ref x (SFX- i 1)) file-path-sep)))
        (loop1 (SFX- i 1))
        (let ((result (Smake-string1 (SFX- (Sstring-length x) i))))
          (let loop2 ((j (SFX- (Sstring-length x) 1)))
            (if (SFX< j i)
                result
                (begin
                  (Sstring-set! result (SFX- j i) (Sstring-ref x j))
                  (loop2 (SFX- j 1)))))))))
(define (file-ext x) (set-bbv-version-limit! #f) 
  (let loop1 ((i (Sstring-length x)))
    (if (or (SFX= i 0) (Schar=? (Sstring-ref x (SFX- i 1)) file-path-sep))
        #f
        (if (not (Schar=? (Sstring-ref x (SFX- i 1)) file-ext-sep))
            (loop1 (SFX- i 1))
            (let ((result (Smake-string1 (SFX- (Sstring-length x) i))))
              (let loop2 ((j (SFX- (Sstring-length x) 1)))
                (if (SFX< j i)
                    result
                    (begin
                      (Sstring-set! result (SFX- j i) (Sstring-ref x j))
                      (loop2 (SFX- j 1))))))))))
(define (file-root x) (set-bbv-version-limit! #f) 
  (let loop1 ((i (Sstring-length x)))
    (if (or (SFX= i 0) (Schar=? (Sstring-ref x (SFX- i 1)) file-path-sep))
        x
        (if (not (Schar=? (Sstring-ref x (SFX- i 1)) file-ext-sep))
            (loop1 (SFX- i 1))
            (let ((result (Smake-string1 (SFX- i 1))))
              (let loop2 ((j (SFX- i 2)))
                (if (SFX< j 0)
                    result
                    (begin
                      (Sstring-set! result j (Sstring-ref x j))
                      (loop2 (SFX- j 1))))))))))
(define (make-counter next limit limit-error) (set-bbv-version-limit! #f) 
  (lambda ()
    (if (SFX< next limit)
        (let ((result next)) (set! next (SFX+ next 1)) result)
        (limit-error))))
(define (pos-in-list x l) (set-bbv-version-limit! #f) 
  (let loop ((l l) (i 0))
    (cond ((not (pair? l)) #f)
          ((eq? (Scar l) x) i)
          (else (loop (Scdr l) (SFX+ i 1))))))
(define (string-pos-in-list x l) (set-bbv-version-limit! #f) 
  (let loop ((l l) (i 0))
    (cond ((not (pair? l)) #f)
          ((Sstring=? (Scar l) x) i)
          (else (loop (Scdr l) (SFX+ i 1))))))
(define (nth-after l n) (set-bbv-version-limit! #f) 
  (let loop ((l l) (n n)) (if (SFX> n 0) (loop (Scdr l) (SFX- n 1)) l)))
(define (pair-up l1 l2) (set-bbv-version-limit! #f) 
  (define (pair l1 l2) (set-bbv-version-limit! #f) 
    (if (pair? l1)
        (cons (cons (Scar l1) (Scar l2)) (pair (Scdr l1) (Scdr l2)))
        '()))
  (pair l1 l2))
(define (my-last-pair l) (set-bbv-version-limit! #f) 
  (let loop ((l l)) (if (pair? (Scdr l)) (loop (Scdr l)) l)))
(define (sort-list l <?) (set-bbv-version-limit! #f) 
  (define (mergesort l) (set-bbv-version-limit! #f) 
    (define (merge l1 l2) (set-bbv-version-limit! #f) 
      (cond ((null? l1) l2)
            ((null? l2) l1)
            (else
             (let ((e1 (Scar l1)) (e2 (Scar l2)))
               (if (<? e1 e2)
                   (cons e1 (merge (Scdr l1) l2))
                   (cons e2 (merge l1 (Scdr l2))))))))
    (define (split l) (set-bbv-version-limit! #f) 
      (if (or (null? l) (null? (Scdr l))) l (cons (Scar l) (split (Scddr l)))))
    (if (or (null? l) (null? (Scdr l)))
        l
        (let* ((l1 (mergesort (split l))) (l2 (mergesort (split (Scdr l)))))
          (merge l1 l2))))
  (mergesort l))
(define (lst->vector l) (set-bbv-version-limit! #f) 
  (let* ((n (Slength l)) (v (Smake-vector1 n)))
    (let loop ((l l) (i 0))
      (if (pair? l)
          (begin (Svector-set! v i (Scar l)) (loop (Scdr l) (SFX+ i 1)))
          v))))
(define (vector->lst v) (set-bbv-version-limit! #f) 
  (let loop ((l '()) (i (SFX- (Svector-length v) 1)))
    (if (SFX< i 0) l (loop (cons (Svector-ref v i) l) (SFX- i 1)))))
(define (lst->string l) (set-bbv-version-limit! #f) 
  (let* ((n (Slength l)) (s (Smake-string1 n)))
    (let loop ((l l) (i 0))
      (if (pair? l)
          (begin (Sstring-set! s i (Scar l)) (loop (Scdr l) (SFX+ i 1)))
          s))))
(define (string->lst s) (set-bbv-version-limit! #f) 
  (let loop ((l '()) (i (SFX- (Sstring-length s) 1)))
    (if (SFX< i 0) l (loop (cons (Sstring-ref s i) l) (SFX- i 1)))))
(define (with-exception-handling proc) (set-bbv-version-limit! #f) 
  (proc)
;;  (let ((old-exception-handler throw-to-exception-handler))
;;    (let ((val (call-with-current-continuation
;;                (lambda (cont) (set-bbv-version-limit! #f) 
;;                  (set! throw-to-exception-handler cont)
;;                  (proc)))))
;;      (set! throw-to-exception-handler old-exception-handler)
;;      val))
)
(define (throw-to-exception-handler val) (set-bbv-version-limit! #f) 
  (fatal-err "Internal error, no exception handler at this point" val))
(define (compiler-error msg . args) (set-bbv-version-limit! #f) 
  (newline)
  (display "*** ERROR -- ")
  (display msg)
  (for-each (lambda (x) (set-bbv-version-limit! #f)  (display " ") (write x)) args)
  (newline)
  (compiler-abort))
(define (compiler-user-error loc msg . args) (set-bbv-version-limit! #f) 
  (newline)
  (display "*** ERROR -- In ")
  (locat-show loc)
  (newline)
  (display "*** ")
  (display msg)
  (for-each (lambda (x) (set-bbv-version-limit! #f)  (display " ") (write x)) args)
  (newline)
  (compiler-abort))
(define (compiler-internal-error msg . args) (set-bbv-version-limit! #f) 
  (newline)
  (display "*** ERROR -- Compiler internal error detected")
  (newline)
  (display "*** in procedure ")
  (display msg)
  (for-each (lambda (x) (set-bbv-version-limit! #f)  (display " ") (write x)) args)
  (newline)
  (compiler-abort))
(define (compiler-limitation-error msg . args) (set-bbv-version-limit! #f) 
  (newline)
  (display "*** ERROR -- Compiler limit reached")
  (newline)
  (display "*** ")
  (display msg)
  (for-each (lambda (x) (set-bbv-version-limit! #f)  (display " ") (write x)) args)
  (newline)
  (compiler-abort))
(define (compiler-abort) (set-bbv-version-limit! #f)  (throw-to-exception-handler #f))
(define (make-gnode label edges) (set-bbv-version-limit! #f)  (vector label edges))
(define (gnode-label x) (set-bbv-version-limit! #f)  (Svector-ref x 0))
(define (gnode-edges x) (set-bbv-version-limit! #f)  (Svector-ref x 1))
(define (transitive-closure graph) (set-bbv-version-limit! #f) 
  (define changed? #f)
  (define (closure edges) (set-bbv-version-limit! #f) 
    (list->set
     (set-union
      edges
      (apply set-union
             (Smap2 (lambda (label) (set-bbv-version-limit! #f)  (gnode-edges (gnode-find label graph)))
                  (set->list edges))))))
  (let ((new-graph
         (set-map (lambda (x) (set-bbv-version-limit! #f) 
                    (let ((new-edges (closure (gnode-edges x))))
                      (if (not (set-equal? new-edges (gnode-edges x)))
                          (set! changed? #t))
                      (make-gnode (gnode-label x) new-edges)))
                  graph)))
    (if changed? (transitive-closure new-graph) new-graph)))
(define (gnode-find label graph) (set-bbv-version-limit! #f) 
  (define (find label l) (set-bbv-version-limit! #f) 
    (cond ((null? l) #f)
          ((eq? (gnode-label (Scar l)) label) (Scar l))
          (else (find label (Scdr l)))))
  (find label (set->list graph)))
(define (topological-sort graph) (set-bbv-version-limit! #f) 
  (if (set-empty? graph)
      '()
      (let ((to-remove (or (remove-no-edges graph) (remove-cycle graph))))
        (let ((labels (set-map gnode-label to-remove)))
          (cons labels
                (topological-sort
                 (set-map (lambda (x) (set-bbv-version-limit! #f) 
                            (make-gnode
                             (gnode-label x)
                             (set-difference (gnode-edges x) labels)))
                          (set-difference graph to-remove))))))))
(define (remove-no-edges graph) (set-bbv-version-limit! #f) 
  (let ((nodes-with-no-edges
         (set-keep (lambda (x) (set-bbv-version-limit! #f)  (set-empty? (gnode-edges x))) graph)))
    (if (set-empty? nodes-with-no-edges) #f nodes-with-no-edges)))
(define (remove-cycle graph) (set-bbv-version-limit! #f) 
  (define (remove l) (set-bbv-version-limit! #f) 
    (let ((edges (gnode-edges (Scar l))))
      (define (equal-edges? x) (set-bbv-version-limit! #f)  (set-equal? (gnode-edges x) edges))
      (define (member-edges? x) (set-bbv-version-limit! #f)  (set-member? (gnode-label x) edges))
      (if (set-member? (gnode-label (Scar l)) edges)
          (let ((edge-graph (set-keep member-edges? graph)))
            (if (set-every? equal-edges? edge-graph)
                edge-graph
                (remove (Scdr l))))
          (remove (Scdr l)))))
  (remove (set->list graph)))
(define (list->set list) (set-bbv-version-limit! #f)  list)
(define (set->list set) (set-bbv-version-limit! #f)  set)
(define (set-empty) (set-bbv-version-limit! #f)  '())
(define (set-empty? set) (set-bbv-version-limit! #f)  (null? set))
(define (set-member? x set) (set-bbv-version-limit! #f)  (Smemq x set))
(define (set-singleton x) (set-bbv-version-limit! #f)  (list x))
(define (set-adjoin set x) (set-bbv-version-limit! #f)  (if (Smemq x set) set (cons x set)))
(define (set-remove set x) (set-bbv-version-limit! #f) 
  (cond ((null? set) '())
        ((eq? (Scar set) x) (Scdr set))
        (else (cons (Scar set) (set-remove (Scdr set) x)))))
(define (set-equal? s1 s2) (set-bbv-version-limit! #f) 
  (cond ((null? s1) (null? s2))
        ((Smemq (Scar s1) s2) (set-equal? (Scdr s1) (set-remove s2 (Scar s1))))
        (else #f)))
(define (set-difference set . other-sets) (set-bbv-version-limit! #f) 
  (define (difference s1 s2) (set-bbv-version-limit! #f) 
    (cond ((null? s1) '())
          ((Smemq (Scar s1) s2) (difference (Scdr s1) s2))
          (else (cons (Scar s1) (difference (Scdr s1) s2)))))
  (n-ary difference set other-sets))
(define (set-union . sets) (set-bbv-version-limit! #f) 
  (define (union s1 s2) (set-bbv-version-limit! #f) 
    (cond ((null? s1) s2)
          ((Smemq (Scar s1) s2) (union (Scdr s1) s2))
          (else (cons (Scar s1) (union (Scdr s1) s2)))))
  (n-ary union '() sets))
(define (set-intersection set . other-sets) (set-bbv-version-limit! #f) 
  (define (intersection s1 s2) (set-bbv-version-limit! #f) 
    (cond ((null? s1) '())
          ((Smemq (Scar s1) s2) (cons (Scar s1) (intersection (Scdr s1) s2)))
          (else (intersection (Scdr s1) s2))))
  (n-ary intersection set other-sets))
(define (n-ary function first rest) (set-bbv-version-limit! #f) 
  (if (null? rest)
      first
      (n-ary function (function first (Scar rest)) (Scdr rest))))
(define (set-keep keep? set) (set-bbv-version-limit! #f) 
  (cond ((null? set) '())
        ((keep? (Scar set)) (cons (Scar set) (set-keep keep? (Scdr set))))
        (else (set-keep keep? (Scdr set)))))
(define (set-every? pred? set) (set-bbv-version-limit! #f) 
  (or (null? set) (and (pred? (Scar set)) (set-every? pred? (Scdr set)))))
(define (set-map proc set) (set-bbv-version-limit! #f) 
  (if (null? set) '() (cons (proc (Scar set)) (set-map proc (Scdr set)))))
(define (list->queue list) (set-bbv-version-limit! #f) 
  (cons list (if (pair? list) (my-last-pair list) '())))
(define (queue->list queue) (set-bbv-version-limit! #f)  (Scar queue))
(define (queue-empty) (set-bbv-version-limit! #f)  (cons '() '()))
(define (queue-empty? queue) (set-bbv-version-limit! #f)  (null? (Scar queue)))
(define (queue-get! queue) (set-bbv-version-limit! #f) 
  (if (null? (Scar queue))
      (compiler-internal-error "queue-get!, queue is empty")
      (let ((x (Scaar queue)))
        (Sset-car! queue (Scdar queue))
        (if (null? (Scar queue)) (Sset-cdr! queue '()))
        x)))
(define (queue-put! queue x) (set-bbv-version-limit! #f) 
  (let ((entry (cons x '())))
    (if (null? (Scar queue))
        (Sset-car! queue entry)
        (Sset-cdr! (Scdr queue) entry))
    (Sset-cdr! queue entry)
    x))
(define (string->canonical-symbol str) (set-bbv-version-limit! #f) 
  (let ((len (Sstring-length str)))
    (let loop ((str str) (s (Smake-string1 len)) (i (SFX- len 1)))
      (if (SFX>= i 0)
          (begin
            (Sstring-set! s i (Schar-downcase (Sstring-ref str i)))
            (loop str s (SFX- i 1)))
          (Sstring->symbol s)))))
(define quote-sym (string->canonical-symbol "QUOTE"))
(define quasiquote-sym (string->canonical-symbol "QUASIQUOTE"))
(define unquote-sym (string->canonical-symbol "UNQUOTE"))
(define unquote-splicing-sym (string->canonical-symbol "UNQUOTE-SPLICING"))
(define lambda-sym (string->canonical-symbol "LAMBDA"))
(define if-sym (string->canonical-symbol "IF"))
(define set!-sym (string->canonical-symbol "SET!"))
(define cond-sym (string->canonical-symbol "COND"))
(define =>-sym (string->canonical-symbol "=>"))
(define else-sym (string->canonical-symbol "ELSE"))
(define and-sym (string->canonical-symbol "AND"))
(define or-sym (string->canonical-symbol "OR"))
(define case-sym (string->canonical-symbol "CASE"))
(define let-sym (string->canonical-symbol "LET"))
(define let*-sym (string->canonical-symbol "LET*"))
(define letrec-sym (string->canonical-symbol "LETREC"))
(define begin-sym (string->canonical-symbol "BEGIN"))
(define do-sym (string->canonical-symbol "DO"))
(define define-sym (string->canonical-symbol "DEFINE"))
(define delay-sym (string->canonical-symbol "DELAY"))
(define future-sym (string->canonical-symbol "FUTURE"))
(define **define-macro-sym (string->canonical-symbol "DEFINE-MACRO"))
(define **declare-sym (string->canonical-symbol "DECLARE"))
(define **include-sym (string->canonical-symbol "INCLUDE"))
(define not-sym (string->canonical-symbol "NOT"))
(define **c-declaration-sym (string->canonical-symbol "C-DECLARATION"))
(define **c-init-sym (string->canonical-symbol "C-INIT"))
(define **c-procedure-sym (string->canonical-symbol "C-PROCEDURE"))
(define void-sym (string->canonical-symbol "VOID"))
(define char-sym (string->canonical-symbol "CHAR"))
(define signed-char-sym (string->canonical-symbol "SIGNED-CHAR"))
(define unsigned-char-sym (string->canonical-symbol "UNSIGNED-CHAR"))
(define short-sym (string->canonical-symbol "SHORT"))
(define unsigned-short-sym (string->canonical-symbol "UNSIGNED-SHORT"))
(define int-sym (string->canonical-symbol "INT"))
(define unsigned-int-sym (string->canonical-symbol "UNSIGNED-INT"))
(define long-sym (string->canonical-symbol "LONG"))
(define unsigned-long-sym (string->canonical-symbol "UNSIGNED-LONG"))
(define float-sym (string->canonical-symbol "FLOAT"))
(define double-sym (string->canonical-symbol "DOUBLE"))
(define pointer-sym (string->canonical-symbol "POINTER"))
(define boolean-sym (string->canonical-symbol "BOOLEAN"))
(define string-sym (string->canonical-symbol "STRING"))
(define scheme-object-sym (string->canonical-symbol "SCHEME-OBJECT"))
(define c-id-prefix "___")
(define false-object (if (eq? '() #f) (Sstring->symbol "#f") #f))
(define (false-object? obj) (set-bbv-version-limit! #f)  (eq? obj false-object))
(define undef-object (Sstring->symbol "#[undefined]"))
(define (undef-object? obj) (set-bbv-version-limit! #f)  (eq? obj undef-object))
(define (symbol-object? obj) (set-bbv-version-limit! #f) 
  (and (not (false-object? obj)) (not (undef-object? obj)) (symbol? obj)))
(define scm-file-exts '("scm" #f))
(define compiler-version "2.2.2")
(define (open-sf filename) (set-bbv-version-limit! #f) 
  (define (open-err) (set-bbv-version-limit! #f)  (compiler-error "Can't find file" filename))
  (if (not (file-ext filename))
      (let loop ((exts scm-file-exts))
        (if (pair? exts)
            (let* ((ext (Scar exts))
                   (full-name
                    (if ext (Sstring-append filename "." ext) filename))
                   (port (open-input-file* full-name)))
              (if port (vector port full-name 0 1 0) (loop (Scdr exts))))
            (open-err)))
      (let ((port (open-input-file* filename)))
        (if port (vector port filename 0 1 0) (open-err)))))
(define (close-sf sf) (set-bbv-version-limit! #f)  (close-input-port (Svector-ref sf 0)))
(define (sf-read-char sf) (set-bbv-version-limit! #f) 
  (let ((c (read-char (Svector-ref sf 0))))
    (cond ((eof-object? c))
          ((Schar=? c char-newline)
           (Svector-set! sf 3 (SFX+ (Svector-ref sf 3) 1))
           (Svector-set! sf 4 0))
          (else (Svector-set! sf 4 (SFX+ (Svector-ref sf 4) 1))))
    c))
(define (sf-peek-char sf) (set-bbv-version-limit! #f)  (peek-char (Svector-ref sf 0)))
(define (sf-read-error sf msg . args) (set-bbv-version-limit! #f) 
  (apply compiler-user-error
         (cons (sf->locat sf)
               (cons (Sstring-append "Read error -- " msg) args))))
(define (sf->locat sf) (set-bbv-version-limit! #f) 
  (vector 'file
          (Svector-ref sf 1)
          (Svector-ref sf 2)
          (Svector-ref sf 3)
          (Svector-ref sf 4)))
(define (expr->locat expr source) (set-bbv-version-limit! #f)  (vector 'expr expr source))
(define (locat-show loc) (set-bbv-version-limit! #f) 
  (if loc
      (case (Svector-ref loc 0)
        ((file)
         (if (pinpoint-error
              (Svector-ref loc 1)
              (Svector-ref loc 3)
              (Svector-ref loc 4))
             (begin
               (display "file \"")
               (display (Svector-ref loc 1))
               (display "\", line ")
               (display (Svector-ref loc 3))
               (display ", character ")
               (display (Svector-ref loc 4)))))
        ((expr)
         (display "expression ")
         (write (Svector-ref loc 1))
         (if (Svector-ref loc 2)
             (begin
               (display " ")
               (locat-show (source-locat (Svector-ref loc 2))))))
        (else (compiler-internal-error "locat-show, unknown location tag")))
      (display "unknown location")))
(define (locat-filename loc) (set-bbv-version-limit! #f) 
  (if loc
      (case (Svector-ref loc 0)
        ((file) (Svector-ref loc 1))
        ((expr)
         (let ((source (Svector-ref loc 2)))
           (if source (locat-filename (source-locat source)) "")))
        (else
         (compiler-internal-error "locat-filename, unknown location tag")))
      ""))
(define (make-source code locat) (set-bbv-version-limit! #f)  (vector code locat))
(define (source-code x) (set-bbv-version-limit! #f)  (Svector-ref x 0))
(define (source-code-set! x y) (set-bbv-version-limit! #f)  (Svector-set! x 0 y) x)
(define (source-locat x) (set-bbv-version-limit! #f)  (Svector-ref x 1))
(define (expression->source expr source) (set-bbv-version-limit! #f) 
  (define (expr->source x) (set-bbv-version-limit! #f) 
    (make-source
     (cond ((pair? x) (list->source x))
           ((vector? x) (vector->source x))
           ((symbol-object? x) (string->canonical-symbol (Ssymbol->string x)))
           (else x))
     (expr->locat x source)))
  (define (list->source l) (set-bbv-version-limit! #f) 
    (cond ((pair? l) (cons (expr->source (Scar l)) (list->source (Scdr l))))
          ((null? l) '())
          (else (expr->source l))))
  (define (vector->source v) (set-bbv-version-limit! #f) 
    (let* ((len (Svector-length v)) (x (Smake-vector1 len)))
      (let loop ((i (SFX- len 1)))
        (if (SFX>= i 0)
            (begin
              (Svector-set! x i (expr->source (Svector-ref v i)))
              (loop (SFX- i 1)))))
      x))
  (expr->source expr))
(define (source->expression source) (set-bbv-version-limit! #f) 
  (define (list->expression l) (set-bbv-version-limit! #f) 
    (cond ((pair? l)
           (cons (source->expression (Scar l)) (list->expression (Scdr l))))
          ((null? l) '())
          (else (source->expression l))))
  (define (vector->expression v) (set-bbv-version-limit! #f) 
    (let* ((len (Svector-length v)) (x (Smake-vector1 len)))
      (let loop ((i (SFX- len 1)))
        (if (SFX>= i 0)
            (begin
              (Svector-set! x i (source->expression (Svector-ref v i)))
              (loop (SFX- i 1)))))
      x))
  (let ((code (source-code source)))
    (cond ((pair? code) (list->expression code))
          ((vector? code) (vector->expression code))
          (else code))))
(define (file->sources filename info-port) (set-bbv-version-limit! #f) 
  (if info-port
      (begin
        (display "(reading \"" info-port)
        (display filename info-port)
        (display "\"" info-port)))
  (let ((sf (open-sf filename)))
    (define (read-sources) (set-bbv-version-limit! #f) 
      (let ((source (read-source sf)))
        (if (not (eof-object? source))
            (begin
              (if info-port (display "." info-port))
              (cons source (read-sources)))
            '())))
    (let ((sources (read-sources)))
      (if info-port (display ")" info-port))
      (close-sf sf)
      sources)))
(define (file->sources* filename info-port loc) (set-bbv-version-limit! #f) 
  (file->sources
   (if (path-absolute? filename)
       filename
       (Sstring-append (file-path (locat-filename loc)) filename))
   info-port))
(define (read-source sf) (set-bbv-version-limit! #f) 
  (define (read-char*) (set-bbv-version-limit! #f) 
    (let ((c (sf-read-char sf)))
      (if (eof-object? c)
          (sf-read-error sf "Premature end of file encountered")
          c)))
  (define (read-non-whitespace-char) (set-bbv-version-limit! #f) 
    (let ((c (read-char*)))
      (cond ((SFX< 0 (Svector-ref read-table (Schar->integer c)))
             (read-non-whitespace-char))
            ((Schar=? c #\;)
             (let loop ()
               (if (not (Schar=? (read-char*) char-newline))
                   (loop)
                   (read-non-whitespace-char))))
            (else c))))
  (define (delimiter? c) (set-bbv-version-limit! #f) 
    (or (eof-object? c) (not (SFX= (Svector-ref read-table (Schar->integer c)) 0))))
  (define (read-list first) (set-bbv-version-limit! #f) 
    (let ((result (cons first '())))
      (let loop ((end result))
        (let ((c (read-non-whitespace-char)))
          (cond ((Schar=? c #\)))
                ((and (Schar=? c #\.) (delimiter? (sf-peek-char sf)))
                 (let ((x (read-source sf)))
                   (if (Schar=? (read-non-whitespace-char) #\))
                       (Sset-cdr! end x)
                       (sf-read-error sf "')' expected"))))
                (else
                 (let ((tail (cons (rd* c) '())))
                   (Sset-cdr! end tail)
                   (loop tail))))))
      result))
  (define (read-vector) (set-bbv-version-limit! #f) 
    (define (loop i) (set-bbv-version-limit! #f) 
      (let ((c (read-non-whitespace-char)))
        (if (Schar=? c #\))
            (Smake-vector2 i '())
            (let* ((x (rd* c)) (v (loop (SFX+ i 1)))) (Svector-set! v i x) v))))
    (loop 0))
  (define (read-string) (set-bbv-version-limit! #f) 
    (define (loop i) (set-bbv-version-limit! #f) 
      (let ((c (read-char*)))
        (cond ((Schar=? c #\") (Smake-string2 i #\space))
              ((Schar=? c #\\)
               (let* ((c (read-char*)) (s (loop (SFX+ i 1))))
                 (Sstring-set! s i c)
                 s))
              (else (let ((s (loop (SFX+ i 1)))) (Sstring-set! s i c) s)))))
    (loop 0))
  (define (read-symbol/number-string i) (set-bbv-version-limit! #f) 
    (if (delimiter? (sf-peek-char sf))
        (Smake-string2 i #\space)
        (let* ((c (sf-read-char sf)) (s (read-symbol/number-string (SFX+ i 1))))
          (Sstring-set! s i (Schar-downcase c))
          s)))
  (define (read-symbol/number c) (set-bbv-version-limit! #f) 
    (let ((s (read-symbol/number-string 1)))
      (Sstring-set! s 0 (Schar-downcase c))
      (or (Sstring->number2 s 10) (string->canonical-symbol s))))
  (define (read-prefixed-number c) (set-bbv-version-limit! #f) 
    (let ((s (read-symbol/number-string 2)))
      (Sstring-set! s 0 #\#)
      (Sstring-set! s 1 c)
      (Sstring->number2 s 10)))
  (define (read-special-symbol) (set-bbv-version-limit! #f) 
    (let ((s (read-symbol/number-string 2)))
      (Sstring-set! s 0 #\#)
      (Sstring-set! s 1 #\#)
      (string->canonical-symbol s)))
  (define (rd c) (set-bbv-version-limit! #f) 
    (cond ((eof-object? c) c)
          ((SFX< 0 (Svector-ref read-table (Schar->integer c)))
           (rd (sf-read-char sf)))
          ((Schar=? c #\;)
           (let loop ()
             (let ((c (sf-read-char sf)))
               (cond ((eof-object? c) c)
                     ((Schar=? c char-newline) (rd (sf-read-char sf)))
                     (else (loop))))))
          (else (rd* c))))
  (define (rd* c) (set-bbv-version-limit! #f) 
    (let ((source (make-source #f (sf->locat sf))))
      (source-code-set!
       source
       (cond ((Schar=? c #\()
              (let ((x (read-non-whitespace-char)))
                (if (Schar=? x #\)) '() (read-list (rd* x)))))
             ((Schar=? c #\#)
              (let ((c (Schar-downcase (sf-read-char sf))))
                (cond ((Schar=? c #\() (read-vector))
                      ((Schar=? c #\f) false-object)
                      ((Schar=? c #\t) #t)
                      ((Schar=? c #\\)
                       (let ((c (read-char*)))
                         (if (or (not (Schar-alphabetic? c))
                                 (delimiter? (sf-peek-char sf)))
                             c
                             (let ((name (read-symbol/number c)))
                               (let ((x (Sassq name named-char-table)))
                                 (if x
                                     (Scdr x)
                                     (sf-read-error
                                      sf
                                      "Unknown character name"
                                      name)))))))
                      ((Schar=? c #\#) (read-special-symbol))
                      (else
                       (let ((num (read-prefixed-number c)))
                         (or num
                             (sf-read-error
                              sf
                              "Unknown '#' read macro"
                              c)))))))
             ((Schar=? c #\") (read-string))
             ((Schar=? c #\')
              (list (make-source quote-sym (sf->locat sf)) (read-source sf)))
             ((Schar=? c #\`)
              (list (make-source quasiquote-sym (sf->locat sf))
                    (read-source sf)))
             ((Schar=? c #\,)
              (if (Schar=? (sf-peek-char sf) #\@)
                  (let ((x (make-source unquote-splicing-sym (sf->locat sf))))
                    (sf-read-char sf)
                    (list x (read-source sf)))
                  (list (make-source unquote-sym (sf->locat sf))
                        (read-source sf))))
             ((Schar=? c #\)) (sf-read-error sf "Misplaced ')'"))
             ((or (Schar=? c #\[) (Schar=? c #\]) (Schar=? c #\{) (Schar=? c #\}))
              (sf-read-error sf "Illegal character" c))
             (else
              (if (Schar=? c #\.)
                  (if (delimiter? (sf-peek-char sf))
                      (sf-read-error sf "Misplaced '.'")))
              (read-symbol/number c))))))
  (rd (sf-read-char sf)))
(define named-char-table
  (list (cons (string->canonical-symbol "NUL") char-nul)
        (cons (string->canonical-symbol "TAB") char-tab)
        (cons (string->canonical-symbol "NEWLINE") char-newline)
        (cons (string->canonical-symbol "SPACE") #\space)))
(define read-table
  (let ((rt (Smake-vector2 (SFX+ max-character-encoding 1) 0)))
    (Svector-set! rt (Schar->integer char-tab) 1)
    (Svector-set! rt (Schar->integer char-newline) 1)
    (Svector-set! rt (Schar->integer #\space) 1)
    (Svector-set! rt (Schar->integer #\;) -1)
    (Svector-set! rt (Schar->integer #\() -1)
    (Svector-set! rt (Schar->integer #\)) -1)
    (Svector-set! rt (Schar->integer #\") -1)
    (Svector-set! rt (Schar->integer #\') -1)
    (Svector-set! rt (Schar->integer #\`) -1)
    rt))
(define (make-var name bound refs sets source) (set-bbv-version-limit! #f) 
  (vector var-tag name bound refs sets source #f))
(define (var? x) (set-bbv-version-limit! #f) 
  (and (vector? x) (SFX> (Svector-length x) 0) (eq? (Svector-ref x 0) var-tag)))
(define (var-name x) (set-bbv-version-limit! #f)  (Svector-ref x 1))
(define (var-bound x) (set-bbv-version-limit! #f)  (Svector-ref x 2))
(define (var-refs x) (set-bbv-version-limit! #f)  (Svector-ref x 3))
(define (var-sets x) (set-bbv-version-limit! #f)  (Svector-ref x 4))
(define (var-source x) (set-bbv-version-limit! #f)  (Svector-ref x 5))
(define (var-info x) (set-bbv-version-limit! #f)  (Svector-ref x 6))
(define (var-name-set! x y) (set-bbv-version-limit! #f)  (Svector-set! x 1 y))
(define (var-bound-set! x y) (set-bbv-version-limit! #f)  (Svector-set! x 2 y))
(define (var-refs-set! x y) (set-bbv-version-limit! #f)  (Svector-set! x 3 y))
(define (var-sets-set! x y) (set-bbv-version-limit! #f)  (Svector-set! x 4 y))
(define (var-source-set! x y) (set-bbv-version-limit! #f)  (Svector-set! x 5 y))
(define (var-info-set! x y) (set-bbv-version-limit! #f)  (Svector-set! x 6 y))
(define var-tag (list 'var-tag))
(define (var-copy var) (set-bbv-version-limit! #f) 
  (make-var (var-name var) #t (set-empty) (set-empty) (var-source var)))
(define (make-temp-var name) (set-bbv-version-limit! #f)  (make-var name #t (set-empty) (set-empty) #f))
(define (temp-var? var) (set-bbv-version-limit! #f)  (eq? (var-bound var) #t))
(define ret-var (make-temp-var 'ret))
(define ret-var-set (set-singleton ret-var))
(define closure-env-var (make-temp-var 'closure-env))
(define empty-var (make-temp-var #f))
(define make-global-environment #f)
(set! make-global-environment (lambda () (env-frame #f '())))
(define (env-frame env vars) (set-bbv-version-limit! #f)  (vector (cons vars #f) '() '() env))
(define (env-new-var! env name source) (set-bbv-version-limit! #f) 
  (let* ((glob (not (env-parent-ref env)))
         (var (make-var name (not glob) (set-empty) (set-empty) source)))
    (env-vars-set! env (cons var (env-vars-ref env)))
    var))
(define (env-macro env name def) (set-bbv-version-limit! #f) 
  (let ((name* (if (full-name? name)
                   name
                   (let ((prefix (env-namespace-prefix env name)))
                     (if prefix (make-full-name prefix name) name)))))
    (vector (Svector-ref env 0)
            (cons (cons name* def) (env-macros-ref env))
            (env-decls-ref env)
            (env-parent-ref env))))
(define (env-declare env decl) (set-bbv-version-limit! #f) 
  (vector (Svector-ref env 0)
          (env-macros-ref env)
          (cons decl (env-decls-ref env))
          (env-parent-ref env)))
(define (env-vars-ref env) (set-bbv-version-limit! #f)  (Scar (Svector-ref env 0)))
(define (env-vars-set! env vars) (set-bbv-version-limit! #f)  (Sset-car! (Svector-ref env 0) vars))
(define (env-macros-ref env) (set-bbv-version-limit! #f)  (Svector-ref env 1))
(define (env-decls-ref env) (set-bbv-version-limit! #f)  (Svector-ref env 2))
(define (env-parent-ref env) (set-bbv-version-limit! #f)  (Svector-ref env 3))
(define (env-namespace-prefix env name) (set-bbv-version-limit! #f) 
  (let loop ((decls (env-decls-ref env)))
    (if (pair? decls)
        (let ((decl (Scar decls)))
          (if (eq? (Scar decl) namespace-sym)
              (let ((syms (Scddr decl)))
                (if (or (null? syms) (Smemq name syms))
                    (Scadr decl)
                    (loop (Scdr decls))))
              (loop (Scdr decls))))
        #f)))
(define (env-lookup env name stop-at-first-frame? proc) (set-bbv-version-limit! #f) 
  (define (search env name full?) (set-bbv-version-limit! #f) 
    (if full?
        (search* env name full?)
        (let ((prefix (env-namespace-prefix env name)))
          (if prefix
              (search* env (make-full-name prefix name) #t)
              (search* env name full?)))))
  (define (search* env name full?) (set-bbv-version-limit! #f) 
    (define (search-macros macros) (set-bbv-version-limit! #f) 
      (if (pair? macros)
          (let ((m (Scar macros)))
            (if (eq? (Scar m) name)
                (proc env name (Scdr m))
                (search-macros (Scdr macros))))
          (search-vars (env-vars-ref env))))
    (define (search-vars vars) (set-bbv-version-limit! #f) 
      (if (pair? vars)
          (let ((v (Scar vars)))
            (if (eq? (var-name v) name)
                (proc env name v)
                (search-vars (Scdr vars))))
          (let ((env* (env-parent-ref env)))
            (if (or stop-at-first-frame? (not env*))
                (proc env name #f)
                (search env* name full?)))))
    (search-macros (env-macros-ref env)))
  (search env name (full-name? name)))
(define (valid-prefix? str) (set-bbv-version-limit! #f) 
  (let ((l (Sstring-length str)))
    (or (SFX= l 0) (and (SFX>= l 2) (Schar=? (Sstring-ref str (SFX- l 1)) #\#)))))
(define (full-name? sym) (set-bbv-version-limit! #f) 
  (let ((str (Ssymbol->string sym)))
    (let loop ((i (SFX- (Sstring-length str) 1)))
      (if (SFX< i 0) #f (if (Schar=? (Sstring-ref str i) #\#) #t (loop (SFX- i 1)))))))
(define (make-full-name prefix sym) (set-bbv-version-limit! #f) 
  (if (SFX= (Sstring-length prefix) 0)
      sym
      (string->canonical-symbol (Sstring-append prefix (Ssymbol->string sym)))))
(define (env-lookup-var env name source) (set-bbv-version-limit! #f) 
  (env-lookup
   env
   name
   #f
   (lambda (env name x) (set-bbv-version-limit! #f) 
     (if x
         (if (var? x)
             x
             (compiler-internal-error
              "env-lookup-var, name is that of a macro"
              name))
         (env-new-var! env name source)))))
(define (env-define-var env name source) (set-bbv-version-limit! #f) 
  (env-lookup
   env
   name
   #t
   (lambda (env name x) (set-bbv-version-limit! #f) 
     (if x
         (if (var? x)
             (pt-syntax-error source "Duplicate definition of a variable")
             (compiler-internal-error
              "env-define-var, name is that of a macro"
              name))
         (env-new-var! env name source)))))
(define (env-lookup-global-var env name) (set-bbv-version-limit! #f) 
  (let ((env* (env-global-env env)))
    (define (search-vars vars) (set-bbv-version-limit! #f) 
      (if (pair? vars)
          (let ((v (Scar vars)))
            (if (eq? (var-name v) name) v (search-vars (Scdr vars))))
          (env-new-var! env* name #f)))
    (search-vars (env-vars-ref env*))))
(define (env-global-variables env) (set-bbv-version-limit! #f)  (env-vars-ref (env-global-env env)))
(define (env-global-env env) (set-bbv-version-limit! #f) 
  (let loop ((env env))
    (let ((env* (env-parent-ref env))) (if env* (loop env*) env))))
(define (env-lookup-macro env name) (set-bbv-version-limit! #f) 
  (env-lookup
   env
   name
   #f
   (lambda (env name x) (set-bbv-version-limit! #f)  (if (or (not x) (var? x)) #f x))))
(define (env-declarations env) (set-bbv-version-limit! #f)  env)
(define flag-declarations '())
(define parameterized-declarations '())
(define boolean-declarations '())
(define namable-declarations '())
(define namable-boolean-declarations '())
(define namable-string-declarations '())
(define (define-flag-decl name type) (set-bbv-version-limit! #f) 
  (set! flag-declarations (cons (cons name type) flag-declarations))
  '())
(define (define-parameterized-decl name) (set-bbv-version-limit! #f) 
  (set! parameterized-declarations (cons name parameterized-declarations))
  '())
(define (define-boolean-decl name) (set-bbv-version-limit! #f) 
  (set! boolean-declarations (cons name boolean-declarations))
  '())
(define (define-namable-decl name type) (set-bbv-version-limit! #f) 
  (set! namable-declarations (cons (cons name type) namable-declarations))
  '())
(define (define-namable-boolean-decl name) (set-bbv-version-limit! #f) 
  (set! namable-boolean-declarations (cons name namable-boolean-declarations))
  '())
(define (define-namable-string-decl name) (set-bbv-version-limit! #f) 
  (set! namable-string-declarations (cons name namable-string-declarations))
  '())
(define (flag-decl source type val) (set-bbv-version-limit! #f)  (list type val))
(define (parameterized-decl source id parm) (set-bbv-version-limit! #f)  (list id parm))
(define (boolean-decl source id pos) (set-bbv-version-limit! #f)  (list id pos))
(define (namable-decl source type val names) (set-bbv-version-limit! #f)  (cons type (cons val names)))
(define (namable-boolean-decl source id pos names) (set-bbv-version-limit! #f)  (cons id (cons pos names)))
(define (namable-string-decl source id str names) (set-bbv-version-limit! #f) 
  (if (and (eq? id namespace-sym) (not (valid-prefix? str)))
      (pt-syntax-error source "Illegal namespace"))
  (cons id (cons str names)))
(define (declaration-value name element default decls) (set-bbv-version-limit! #f) 
  (if (not decls)
      default
      (let loop ((l (env-decls-ref decls)))
        (if (pair? l)
            (let ((d (Scar l)))
              (if (and (eq? (Scar d) name)
                       (or (null? (Scddr d)) (Smemq element (Scddr d))))
                  (Scadr d)
                  (loop (Scdr l))))
            (declaration-value name element default (env-parent-ref decls))))))
(define namespace-sym (string->canonical-symbol "NAMESPACE"))
(define-namable-string-decl namespace-sym)
(define (node-parent x) (set-bbv-version-limit! #f)  (Svector-ref x 1))
(define (node-children x) (set-bbv-version-limit! #f)  (Svector-ref x 2))
(define (node-fv x) (set-bbv-version-limit! #f)  (Svector-ref x 3))
(define (node-decl x) (set-bbv-version-limit! #f)  (Svector-ref x 4))
(define (node-source x) (set-bbv-version-limit! #f)  (Svector-ref x 5))
(define (node-parent-set! x y) (set-bbv-version-limit! #f)  (Svector-set! x 1 y))
(define (node-fv-set! x y) (set-bbv-version-limit! #f)  (Svector-set! x 3 y))
(define (node-decl-set! x y) (set-bbv-version-limit! #f)  (Svector-set! x 4 y))
(define (node-source-set! x y) (set-bbv-version-limit! #f)  (Svector-set! x 5 y))
(define (node-children-set! x y) (set-bbv-version-limit! #f) 
  (Svector-set! x 2 y)
  (for-each (lambda (child) (set-bbv-version-limit! #f)  (node-parent-set! child x)) y)
  (node-fv-invalidate! x))
(define (node-fv-invalidate! x) (set-bbv-version-limit! #f) 
  (let loop ((node x))
    (if node (begin (node-fv-set! node #t) (loop (node-parent node))))))
(define (make-cst parent children fv decl source val) (set-bbv-version-limit! #f) 
  (vector cst-tag parent children fv decl source val))
(define (cst? x) (set-bbv-version-limit! #f) 
  (and (vector? x) (SFX> (Svector-length x) 0) (eq? (Svector-ref x 0) cst-tag)))
(define (cst-val x) (set-bbv-version-limit! #f)  (Svector-ref x 6))
(define (cst-val-set! x y) (set-bbv-version-limit! #f)  (Svector-set! x 6 y))
(define cst-tag (list 'cst-tag))
(define (make-ref parent children fv decl source var) (set-bbv-version-limit! #f) 
  (vector ref-tag parent children fv decl source var))
(define (ref? x) (set-bbv-version-limit! #f) 
  (and (vector? x) (SFX> (Svector-length x) 0) (eq? (Svector-ref x 0) ref-tag)))
(define (ref-var x) (set-bbv-version-limit! #f)  (Svector-ref x 6))
(define (ref-var-set! x y) (set-bbv-version-limit! #f)  (Svector-set! x 6 y))
(define ref-tag (list 'ref-tag))
(define (make-set parent children fv decl source var) (set-bbv-version-limit! #f) 
  (vector set-tag parent children fv decl source var))
(define (set? x) (set-bbv-version-limit! #f) 
  (and (vector? x) (SFX> (Svector-length x) 0) (eq? (Svector-ref x 0) set-tag)))
(define (set-var x) (set-bbv-version-limit! #f)  (Svector-ref x 6))
(define (set-var-set! x y) (set-bbv-version-limit! #f)  (Svector-set! x 6 y))
(define set-tag (list 'set-tag))
(define (make-def parent children fv decl source var) (set-bbv-version-limit! #f) 
  (vector def-tag parent children fv decl source var))
(define (def? x) (set-bbv-version-limit! #f) 
  (and (vector? x) (SFX> (Svector-length x) 0) (eq? (Svector-ref x 0) def-tag)))
(define (def-var x) (set-bbv-version-limit! #f)  (Svector-ref x 6))
(define (def-var-set! x y) (set-bbv-version-limit! #f)  (Svector-set! x 6 y))
(define def-tag (list 'def-tag))
(define (make-tst parent children fv decl source) (set-bbv-version-limit! #f) 
  (vector tst-tag parent children fv decl source))
(define (tst? x) (set-bbv-version-limit! #f) 
  (and (vector? x) (SFX> (Svector-length x) 0) (eq? (Svector-ref x 0) tst-tag)))
(define tst-tag (list 'tst-tag))
(define (make-conj parent children fv decl source) (set-bbv-version-limit! #f) 
  (vector conj-tag parent children fv decl source))
(define (conj? x) (set-bbv-version-limit! #f) 
  (and (vector? x) (SFX> (Svector-length x) 0) (eq? (Svector-ref x 0) conj-tag)))
(define conj-tag (list 'conj-tag))
(define (make-disj parent children fv decl source) (set-bbv-version-limit! #f) 
  (vector disj-tag parent children fv decl source))
(define (disj? x) (set-bbv-version-limit! #f) 
  (and (vector? x) (SFX> (Svector-length x) 0) (eq? (Svector-ref x 0) disj-tag)))
(define disj-tag (list 'disj-tag))
(define (make-prc parent children fv decl source name min rest parms) (set-bbv-version-limit! #f) 
  (vector prc-tag parent children fv decl source name min rest parms))
(define (prc? x) (set-bbv-version-limit! #f) 
  (and (vector? x) (SFX> (Svector-length x) 0) (eq? (Svector-ref x 0) prc-tag)))
(define (prc-name x) (set-bbv-version-limit! #f)  (Svector-ref x 6))
(define (prc-min x) (set-bbv-version-limit! #f)  (Svector-ref x 7))
(define (prc-rest x) (set-bbv-version-limit! #f)  (Svector-ref x 8))
(define (prc-parms x) (set-bbv-version-limit! #f)  (Svector-ref x 9))
(define (prc-name-set! x y) (set-bbv-version-limit! #f)  (Svector-set! x 6 y))
(define (prc-min-set! x y) (set-bbv-version-limit! #f)  (Svector-set! x 7 y))
(define (prc-rest-set! x y) (set-bbv-version-limit! #f)  (Svector-set! x 8 y))
(define (prc-parms-set! x y) (set-bbv-version-limit! #f)  (Svector-set! x 9 y))
(define prc-tag (list 'prc-tag))
(define (make-app parent children fv decl source) (set-bbv-version-limit! #f) 
  (vector app-tag parent children fv decl source))
(define (app? x) (set-bbv-version-limit! #f) 
  (and (vector? x) (SFX> (Svector-length x) 0) (eq? (Svector-ref x 0) app-tag)))
(define app-tag (list 'app-tag))
(define (make-fut parent children fv decl source) (set-bbv-version-limit! #f) 
  (vector fut-tag parent children fv decl source))
(define (fut? x) (set-bbv-version-limit! #f) 
  (and (vector? x) (SFX> (Svector-length x) 0) (eq? (Svector-ref x 0) fut-tag)))
(define fut-tag (list 'fut-tag))
(define (new-cst source decl val) (set-bbv-version-limit! #f)  (make-cst #f '() #t decl source val))
(define (new-ref source decl var) (set-bbv-version-limit! #f) 
  (let ((node (make-ref #f '() #t decl source var)))
    (var-refs-set! var (set-adjoin (var-refs var) node))
    node))
(define (new-ref-extended-bindings source name env) (set-bbv-version-limit! #f) 
  (new-ref source
           (add-extended-bindings (env-declarations env))
           (env-lookup-global-var env name)))
(define (new-set source decl var val) (set-bbv-version-limit! #f) 
  (let ((node (make-set #f (list val) #t decl source var)))
    (var-sets-set! var (set-adjoin (var-sets var) node))
    (node-parent-set! val node)
    node))
(define (set-val x) (set-bbv-version-limit! #f) 
  (if (set? x)
      (Scar (node-children x))
      (compiler-internal-error "set-val, 'set' node expected" x)))
(define (new-def source decl var val) (set-bbv-version-limit! #f) 
  (let ((node (make-def #f (list val) #t decl source var)))
    (var-sets-set! var (set-adjoin (var-sets var) node))
    (node-parent-set! val node)
    node))
(define (def-val x) (set-bbv-version-limit! #f) 
  (if (def? x)
      (Scar (node-children x))
      (compiler-internal-error "def-val, 'def' node expected" x)))
(define (new-tst source decl pre con alt) (set-bbv-version-limit! #f) 
  (let ((node (make-tst #f (list pre con alt) #t decl source)))
    (node-parent-set! pre node)
    (node-parent-set! con node)
    (node-parent-set! alt node)
    node))
(define (tst-pre x) (set-bbv-version-limit! #f) 
  (if (tst? x)
      (Scar (node-children x))
      (compiler-internal-error "tst-pre, 'tst' node expected" x)))
(define (tst-con x) (set-bbv-version-limit! #f) 
  (if (tst? x)
      (Scadr (node-children x))
      (compiler-internal-error "tst-con, 'tst' node expected" x)))
(define (tst-alt x) (set-bbv-version-limit! #f) 
  (if (tst? x)
      (Scaddr (node-children x))
      (compiler-internal-error "tst-alt, 'tst' node expected" x)))
(define (new-conj source decl pre alt) (set-bbv-version-limit! #f) 
  (let ((node (make-conj #f (list pre alt) #t decl source)))
    (node-parent-set! pre node)
    (node-parent-set! alt node)
    node))
(define (conj-pre x) (set-bbv-version-limit! #f) 
  (if (conj? x)
      (Scar (node-children x))
      (compiler-internal-error "conj-pre, 'conj' node expected" x)))
(define (conj-alt x) (set-bbv-version-limit! #f) 
  (if (conj? x)
      (Scadr (node-children x))
      (compiler-internal-error "conj-alt, 'conj' node expected" x)))
(define (new-disj source decl pre alt) (set-bbv-version-limit! #f) 
  (let ((node (make-disj #f (list pre alt) #t decl source)))
    (node-parent-set! pre node)
    (node-parent-set! alt node)
    node))
(define (disj-pre x) (set-bbv-version-limit! #f) 
  (if (disj? x)
      (Scar (node-children x))
      (compiler-internal-error "disj-pre, 'disj' node expected" x)))
(define (disj-alt x) (set-bbv-version-limit! #f) 
  (if (disj? x)
      (Scadr (node-children x))
      (compiler-internal-error "disj-alt, 'disj' node expected" x)))
(define (new-prc source decl name min rest parms body) (set-bbv-version-limit! #f) 
  (let ((node (make-prc #f (list body) #t decl source name min rest parms)))
    (for-each (lambda (x) (set-bbv-version-limit! #f)  (var-bound-set! x node)) parms)
    (node-parent-set! body node)
    node))
(define (prc-body x) (set-bbv-version-limit! #f) 
  (if (prc? x)
      (Scar (node-children x))
      (compiler-internal-error "prc-body, 'proc' node expected" x)))
(define (new-call source decl oper args) (set-bbv-version-limit! #f) 
  (let ((node (make-app #f (cons oper args) #t decl source)))
    (node-parent-set! oper node)
    (for-each (lambda (x) (set-bbv-version-limit! #f)  (node-parent-set! x node)) args)
    node))
(define (new-call* source decl oper args) (set-bbv-version-limit! #f) 
  (if *ptree-port*
      (if (ref? oper)
          (let ((var (ref-var oper)))
            (if (global? var)
                (let ((proc (standard-procedure
                             (var-name var)
                             (node-decl oper))))
                  (if (and proc
                           (not (nb-args-conforms?
                                 (Slength args)
                                 (standard-procedure-call-pattern proc))))
                      (begin
                        (display "*** WARNING -- \"" *ptree-port*)
                        (display (var-name var) *ptree-port*)
                        (display "\" is called with " *ptree-port*)
                        (display (Slength args) *ptree-port*)
                        (display " argument(s)." *ptree-port*)
                        (newline *ptree-port*))))))))
  (new-call source decl oper args))
(define (app-oper x) (set-bbv-version-limit! #f) 
  (if (app? x)
      (Scar (node-children x))
      (compiler-internal-error "app-oper, 'call' node expected" x)))
(define (app-args x) (set-bbv-version-limit! #f) 
  (if (app? x)
      (Scdr (node-children x))
      (compiler-internal-error "app-args, 'call' node expected" x)))
(define (oper-pos? node) (set-bbv-version-limit! #f) 
  (let ((parent (node-parent node)))
    (if parent (and (app? parent) (eq? (app-oper parent) node)) #f)))
(define (new-fut source decl val) (set-bbv-version-limit! #f) 
  (let ((node (make-fut #f (list val) #t decl source)))
    (node-parent-set! val node)
    node))
(define (fut-val x) (set-bbv-version-limit! #f) 
  (if (fut? x)
      (Scar (node-children x))
      (compiler-internal-error "fut-val, 'fut' node expected" x)))
(define (new-disj-call source decl pre oper alt) (set-bbv-version-limit! #f) 
  (new-call*
   source
   decl
   (let* ((parms (new-temps source '(temp))) (temp (Scar parms)))
     (new-prc source
              decl
              #f
              1
              #f
              parms
              (new-tst source
                       decl
                       (new-ref source decl temp)
                       (new-call*
                        source
                        decl
                        oper
                        (list (new-ref source decl temp)))
                       alt)))
   (list pre)))
(define (new-seq source decl before after) (set-bbv-version-limit! #f) 
  (new-call*
   source
   decl
   (new-prc source decl #f 1 #f (new-temps source '(temp)) after)
   (list before)))
(define (new-let ptree proc vars vals body) (set-bbv-version-limit! #f) 
  (if (pair? vars)
      (new-call
       (node-source ptree)
       (node-decl ptree)
       (new-prc (node-source proc)
                (node-decl proc)
                (prc-name proc)
                (Slength vars)
                #f
                (Sreverse vars)
                body)
       (Sreverse vals))
      body))
(define (new-temps source names) (set-bbv-version-limit! #f) 
  (if (null? names)
      '()
      (cons (make-var (Scar names) #t (set-empty) (set-empty) source)
            (new-temps source (Scdr names)))))
(define (new-variables vars) (set-bbv-version-limit! #f) 
  (if (null? vars)
      '()
      (cons (make-var
             (source-code (Scar vars))
             #t
             (set-empty)
             (set-empty)
             (Scar vars))
            (new-variables (Scdr vars)))))
(define (set-prc-names! vars vals) (set-bbv-version-limit! #f) 
  (let loop ((vars vars) (vals vals))
    (if (not (null? vars))
        (let ((var (Scar vars)) (val (Scar vals)))
          (if (prc? val) (prc-name-set! val (Ssymbol->string (var-name var))))
          (loop (Scdr vars) (Scdr vals))))))
(define (free-variables node) (set-bbv-version-limit! #f) 
  (if (eq? (node-fv node) #t)
      (let ((x (apply set-union (Smap2 free-variables (node-children node)))))
        (node-fv-set!
         node
         (cond ((ref? node)
                (if (global? (ref-var node)) x (set-adjoin x (ref-var node))))
               ((set? node)
                (if (global? (set-var node)) x (set-adjoin x (set-var node))))
               ((prc? node) (set-difference x (list->set (prc-parms node))))
               ((and (app? node) (prc? (app-oper node)))
                (set-difference x (list->set (prc-parms (app-oper node)))))
               (else x)))))
  (node-fv node))
(define (bound-variables node) (set-bbv-version-limit! #f)  (list->set (prc-parms node)))
(define (not-mutable? var) (set-bbv-version-limit! #f)  (set-empty? (var-sets var)))
(define (mutable? var) (set-bbv-version-limit! #f)  (not (not-mutable? var)))
(define (bound? var) (set-bbv-version-limit! #f)  (var-bound var))
(define (global? var) (set-bbv-version-limit! #f)  (not (bound? var)))
(define (global-val var) (set-bbv-version-limit! #f) 
  (and (global? var)
       (let ((sets (set->list (var-sets var))))
         (and (pair? sets)
              (null? (Scdr sets))
              (def? (Scar sets))
              (eq? (compilation-strategy (node-decl (Scar sets))) block-sym)
              (def-val (Scar sets))))))
(define **not-sym (string->canonical-symbol "##NOT"))
(define **quasi-append-sym (string->canonical-symbol "##QUASI-APPEND"))
(define **quasi-list-sym (string->canonical-symbol "##QUASI-LIST"))
(define **quasi-cons-sym (string->canonical-symbol "##QUASI-CONS"))
(define **quasi-list->vector-sym
  (string->canonical-symbol "##QUASI-LIST->VECTOR"))
(define **case-memv-sym (string->canonical-symbol "##CASE-MEMV"))
(define **unassigned?-sym (string->canonical-symbol "##UNASSIGNED?"))
(define **make-cell-sym (string->canonical-symbol "##MAKE-CELL"))
(define **cell-ref-sym (string->canonical-symbol "##CELL-REF"))
(define **cell-set!-sym (string->canonical-symbol "##CELL-SET!"))
(define **make-placeholder-sym (string->canonical-symbol "##MAKE-PLACEHOLDER"))
(define ieee-scheme-sym (string->canonical-symbol "IEEE-SCHEME"))
(define r4rs-scheme-sym (string->canonical-symbol "R4RS-SCHEME"))
(define multilisp-sym (string->canonical-symbol "MULTILISP"))
(define lambda-lift-sym (string->canonical-symbol "LAMBDA-LIFT"))
(define block-sym (string->canonical-symbol "BLOCK"))
(define separate-sym (string->canonical-symbol "SEPARATE"))
(define standard-bindings-sym (string->canonical-symbol "STANDARD-BINDINGS"))
(define extended-bindings-sym (string->canonical-symbol "EXTENDED-BINDINGS"))
(define safe-sym (string->canonical-symbol "SAFE"))
(define interrupts-enabled-sym (string->canonical-symbol "INTERRUPTS-ENABLED"))
(define-flag-decl ieee-scheme-sym 'dialect)
(define-flag-decl r4rs-scheme-sym 'dialect)
(define-flag-decl multilisp-sym 'dialect)
(define-boolean-decl lambda-lift-sym)
(define-flag-decl block-sym 'compilation-strategy)
(define-flag-decl separate-sym 'compilation-strategy)
(define-namable-boolean-decl standard-bindings-sym)
(define-namable-boolean-decl extended-bindings-sym)
(define-boolean-decl safe-sym)
(define-boolean-decl interrupts-enabled-sym)
(define (scheme-dialect decl) (set-bbv-version-limit! #f) 
  (declaration-value 'dialect #f ieee-scheme-sym decl))
(define (lambda-lift? decl) (set-bbv-version-limit! #f)  (declaration-value lambda-lift-sym #f #t decl))
(define (compilation-strategy decl) (set-bbv-version-limit! #f) 
  (declaration-value 'compilation-strategy #f separate-sym decl))
(define (standard-binding? name decl) (set-bbv-version-limit! #f) 
  (declaration-value standard-bindings-sym name #f decl))
(define (extended-binding? name decl) (set-bbv-version-limit! #f) 
  (declaration-value extended-bindings-sym name #f decl))
(define (add-extended-bindings decl) (set-bbv-version-limit! #f) 
  (add-decl (list extended-bindings-sym #t) decl))
(define (intrs-enabled? decl) (set-bbv-version-limit! #f) 
  (declaration-value interrupts-enabled-sym #f #t decl))
(define (add-not-interrupts-enabled decl) (set-bbv-version-limit! #f) 
  (add-decl (list interrupts-enabled-sym #f) decl))
(define (safe? decl) (set-bbv-version-limit! #f)  (declaration-value safe-sym #f #f decl))
(define (add-not-safe decl) (set-bbv-version-limit! #f)  (add-decl (list safe-sym #f) decl))
(define (dialect-specific-keywords dialect) (set-bbv-version-limit! #f) 
  (cond ((eq? dialect ieee-scheme-sym) ieee-scheme-specific-keywords)
        ((eq? dialect r4rs-scheme-sym) r4rs-scheme-specific-keywords)
        ((eq? dialect multilisp-sym) multilisp-specific-keywords)
        (else
         (compiler-internal-error
          "dialect-specific-keywords, unknown dialect"
          dialect))))
(define (dialect-specific-procedures dialect) (set-bbv-version-limit! #f) 
  (cond ((eq? dialect ieee-scheme-sym) ieee-scheme-specific-procedures)
        ((eq? dialect r4rs-scheme-sym) r4rs-scheme-specific-procedures)
        ((eq? dialect multilisp-sym) multilisp-specific-procedures)
        (else
         (compiler-internal-error
          "dialect-specific-procedures, unknown dialect"
          dialect))))
(define (make-standard-procedure x) (set-bbv-version-limit! #f) 
  (cons (string->canonical-symbol (Scar x)) (Scdr x)))
(define (standard-procedure name decl) (set-bbv-version-limit! #f) 
  (or (Sassq name (dialect-specific-procedures (scheme-dialect decl)))
      (Sassq name common-procedures)))
(define (standard-procedure-call-pattern proc) (set-bbv-version-limit! #f)  (Scdr proc))
(define ieee-scheme-specific-keywords '())
(define ieee-scheme-specific-procedures (Smap2 make-standard-procedure '()))
(define r4rs-scheme-specific-keywords (list delay-sym))
(define r4rs-scheme-specific-procedures
  (Smap2 make-standard-procedure
       '(("LIST-TAIL" 2)
         ("-" . 1)
         ("/" . 1)
         ("STRING->LIST" 1)
         ("LIST->STRING" 1)
         ("STRING-COPY" 1)
         ("STRING-FILL!" 2)
         ("VECTOR->LIST" 1)
         ("LIST->VECTOR" 1)
         ("VECTOR-FILL!" 2)
         ("FORCE" 1)
         ("WITH-INPUT-FROM-FILE" 2)
         ("WITH-OUTPUT-TO-FILE" 2)
         ("CHAR-READY?" 0 1)
         ("LOAD" 1)
         ("TRANSCRIPT-ON" 1)
         ("TRANSCRIPT-OFF" 0))))
(define multilisp-specific-keywords (list delay-sym future-sym))
(define multilisp-specific-procedures
  (Smap2 make-standard-procedure '(("FORCE" 1) ("TOUCH" 1))))
(define common-keywords
  (list quote-sym
        quasiquote-sym
        unquote-sym
        unquote-splicing-sym
        lambda-sym
        if-sym
        set!-sym
        cond-sym
        =>-sym
        else-sym
        and-sym
        or-sym
        case-sym
        let-sym
        let*-sym
        letrec-sym
        begin-sym
        do-sym
        define-sym
        **define-macro-sym
        **declare-sym
        **include-sym))
(define common-procedures
  (Smap2 make-standard-procedure
       '(("NOT" 1)
         ("BOOLEAN?" 1)
         ("EQV?" 2)
         ("EQ?" 2)
         ("EQUAL?" 2)
         ("PAIR?" 1)
         ("CONS" 2)
         ("CAR" 1)
         ("CDR" 1)
         ("SET-CAR!" 2)
         ("SET-CDR!" 2)
         ("CAAR" 1)
         ("CADR" 1)
         ("CDAR" 1)
         ("CDDR" 1)
         ("CAAAR" 1)
         ("CAADR" 1)
         ("CADAR" 1)
         ("CADDR" 1)
         ("CDAAR" 1)
         ("CDADR" 1)
         ("CDDAR" 1)
         ("CDDDR" 1)
         ("CAAAAR" 1)
         ("CAAADR" 1)
         ("CAADAR" 1)
         ("CAADDR" 1)
         ("CADAAR" 1)
         ("CADADR" 1)
         ("CADDAR" 1)
         ("CADDDR" 1)
         ("CDAAAR" 1)
         ("CDAADR" 1)
         ("CDADAR" 1)
         ("CDADDR" 1)
         ("CDDAAR" 1)
         ("CDDADR" 1)
         ("CDDDAR" 1)
         ("CDDDDR" 1)
         ("NULL?" 1)
         ("LIST?" 1)
         ("LIST" . 0)
         ("LENGTH" 1)
         ("APPEND" . 0)
         ("REVERSE" 1)
         ("LIST-REF" 2)
         ("MEMQ" 2)
         ("MEMV" 2)
         ("MEMBER" 2)
         ("ASSQ" 2)
         ("ASSV" 2)
         ("ASSOC" 2)
         ("SYMBOL?" 1)
         ("SYMBOL->STRING" 1)
         ("STRING->SYMBOL" 1)
         ("NUMBER?" 1)
         ("COMPLEX?" 1)
         ("REAL?" 1)
         ("RATIONAL?" 1)
         ("INTEGER?" 1)
         ("EXACT?" 1)
         ("INEXACT?" 1)
         ("=" . 2)
         ("<" . 2)
         (">" . 2)
         ("<=" . 2)
         (">=" . 2)
         ("ZERO?" 1)
         ("POSITIVE?" 1)
         ("NEGATIVE?" 1)
         ("ODD?" 1)
         ("EVEN?" 1)
         ("MAX" . 1)
         ("MIN" . 1)
         ("+" . 0)
         ("*" . 0)
         ("-" 1 2)
         ("/" 1 2)
         ("ABS" 1)
         ("QUOTIENT" 2)
         ("REMAINDER" 2)
         ("MODULO" 2)
         ("GCD" . 0)
         ("LCM" . 0)
         ("NUMERATOR" 1)
         ("DENOMINATOR" 1)
         ("FLOOR" 1)
         ("CEILING" 1)
         ("TRUNCATE" 1)
         ("ROUND" 1)
         ("RATIONALIZE" 2)
         ("EXP" 1)
         ("LOG" 1)
         ("SIN" 1)
         ("COS" 1)
         ("TAN" 1)
         ("ASIN" 1)
         ("ACOS" 1)
         ("ATAN" 1 2)
         ("SQRT" 1)
         ("EXPT" 2)
         ("MAKE-RECTANGULAR" 2)
         ("MAKE-POLAR" 2)
         ("REAL-PART" 1)
         ("IMAG-PART" 1)
         ("MAGNITUDE" 1)
         ("ANGLE" 1)
         ("EXACT->INEXACT" 1)
         ("INEXACT->EXACT" 1)
         ("NUMBER->STRING" 1 2)
         ("STRING->NUMBER" 1 2)
         ("CHAR?" 1)
         ("CHAR=?" 2)
         ("CHAR<?" 2)
         ("CHAR>?" 2)
         ("CHAR<=?" 2)
         ("CHAR>=?" 2)
         ("CHAR-CI=?" 2)
         ("CHAR-CI<?" 2)
         ("CHAR-CI>?" 2)
         ("CHAR-CI<=?" 2)
         ("CHAR-CI>=?" 2)
         ("CHAR-ALPHABETIC?" 1)
         ("CHAR-NUMERIC?" 1)
         ("CHAR-WHITESPACE?" 1)
         ("CHAR-UPPER-CASE?" 1)
         ("CHAR-LOWER-CASE?" 1)
         ("CHAR->INTEGER" 1)
         ("INTEGER->CHAR" 1)
         ("CHAR-UPCASE" 1)
         ("CHAR-DOWNCASE" 1)
         ("STRING?" 1)
         ("MAKE-STRING" 1 2)
         ("STRING" . 0)
         ("STRING-LENGTH" 1)
         ("STRING-REF" 2)
         ("STRING-SET!" 3)
         ("STRING=?" 2)
         ("STRING<?" 2)
         ("STRING>?" 2)
         ("STRING<=?" 2)
         ("STRING>=?" 2)
         ("STRING-CI=?" 2)
         ("STRING-CI<?" 2)
         ("STRING-CI>?" 2)
         ("STRING-CI<=?" 2)
         ("STRING-CI>=?" 2)
         ("SUBSTRING" 3)
         ("STRING-APPEND" . 0)
         ("VECTOR?" 1)
         ("MAKE-VECTOR" 1 2)
         ("VECTOR" . 0)
         ("VECTOR-LENGTH" 1)
         ("VECTOR-REF" 2)
         ("VECTOR-SET!" 3)
         ("PROCEDURE?" 1)
         ("APPLY" . 2)
         ("MAP" . 2)
         ("FOR-EACH" . 2)
         ("CALL-WITH-CURRENT-CONTINUATION" 1)
         ("CALL-WITH-INPUT-FILE" 2)
         ("CALL-WITH-OUTPUT-FILE" 2)
         ("INPUT-PORT?" 1)
         ("OUTPUT-PORT?" 1)
         ("CURRENT-INPUT-PORT" 0)
         ("CURRENT-OUTPUT-PORT" 0)
         ("OPEN-INPUT-FILE" 1)
         ("OPEN-OUTPUT-FILE" 1)
         ("CLOSE-INPUT-PORT" 1)
         ("CLOSE-OUTPUT-PORT" 1)
         ("EOF-OBJECT?" 1)
         ("READ" 0 1)
         ("READ-CHAR" 0 1)
         ("PEEK-CHAR" 0 1)
         ("WRITE" 1 2)
         ("DISPLAY" 1 2)
         ("NEWLINE" 0 1)
         ("WRITE-CHAR" 1 2))))
(define (parse-program program env module-name proc) (set-bbv-version-limit! #f) 
  (define (parse-prog program env lst proc) (set-bbv-version-limit! #f) 
    (if (null? program)
        (proc (Sreverse lst) env)
        (let ((source (Scar program)))
          (cond ((macro-expr? source env)
                 (parse-prog
                  (cons (macro-expand source env) (Scdr program))
                  env
                  lst
                  proc))
                ((begin-defs-expr? source)
                 (parse-prog
                  (Sappend (begin-defs-body source) (Scdr program))
                  env
                  lst
                  proc))
                ((include-expr? source)
                 (if *ptree-port* (display "  " *ptree-port*))
                 (let ((x (file->sources*
                           (include-filename source)
                           *ptree-port*
                           (source-locat source))))
                   (if *ptree-port* (newline *ptree-port*))
                   (parse-prog (Sappend x (Scdr program)) env lst proc)))
                ((define-macro-expr? source env)
                 (if *ptree-port*
                     (begin
                       (display "  \"macro\"" *ptree-port*)
                       (newline *ptree-port*)))
                 (parse-prog (Scdr program) (add-macro source env) lst proc))
                ((declare-expr? source)
                 (if *ptree-port*
                     (begin
                       (display "  \"decl\"" *ptree-port*)
                       (newline *ptree-port*)))
                 (parse-prog
                  (Scdr program)
                  (add-declarations source env)
                  lst
                  proc))
                ((define-expr? source env)
                 (let* ((var** (definition-variable source))
                        (var* (source-code var**))
                        (var (env-lookup-var env var* var**)))
                   (if *ptree-port*
                       (begin
                         (display "  " *ptree-port*)
                         (display (var-name var) *ptree-port*)
                         (newline *ptree-port*)))
                   (let ((node (pt (definition-value source) env 'true)))
                     (set-prc-names! (list var) (list node))
                     (parse-prog
                      (Scdr program)
                      env
                      (cons (cons (new-def source
                                           (env-declarations env)
                                           var
                                           node)
                                  env)
                            lst)
                      proc))))
                ((c-declaration-expr? source)
                 (if *ptree-port*
                     (begin
                       (display "  \"c-decl\"" *ptree-port*)
                       (newline *ptree-port*)))
                 (add-c-declaration (source-code (Scadr (source-code source))))
                 (parse-prog (Scdr program) env lst proc))
                ((c-init-expr? source)
                 (if *ptree-port*
                     (begin
                       (display "  \"c-init\"" *ptree-port*)
                       (newline *ptree-port*)))
                 (add-c-init (source-code (Scadr (source-code source))))
                 (parse-prog (Scdr program) env lst proc))
                (else
                 (if *ptree-port*
                     (begin
                       (display "  \"expr\"" *ptree-port*)
                       (newline *ptree-port*)))
                 (parse-prog
                  (Scdr program)
                  env
                  (cons (cons (pt source env 'true) env) lst)
                  proc))))))
  (if *ptree-port*
      (begin (display "Parsing:" *ptree-port*) (newline *ptree-port*)))
  (c-interface-begin module-name)
  (parse-prog
   program
   env
   '()
   (lambda (lst env) (set-bbv-version-limit! #f) 
     (if *ptree-port* (newline *ptree-port*))
     (proc lst env (c-interface-end)))))
(define (c-interface-begin module-name) (set-bbv-version-limit! #f) 
  (set! c-interface-module-name module-name)
  (set! c-interface-proc-count 0)
  (set! c-interface-decls '())
  (set! c-interface-procs '())
  (set! c-interface-inits '())
  #f)
(define (c-interface-end) (set-bbv-version-limit! #f) 
  (let ((i (make-c-intf
            (Sreverse c-interface-decls)
            (Sreverse c-interface-procs)
            (Sreverse c-interface-inits))))
    (set! c-interface-module-name #f)
    (set! c-interface-proc-count #f)
    (set! c-interface-decls #f)
    (set! c-interface-procs #f)
    (set! c-interface-inits #f)
    i))
(define c-interface-module-name #f)
(define c-interface-proc-count #f)
(define c-interface-decls #f)
(define c-interface-procs #f)
(define c-interface-inits #f)
(define (make-c-intf decls procs inits) (set-bbv-version-limit! #f)  (vector decls procs inits))
(define (c-intf-decls c-intf) (set-bbv-version-limit! #f)  (Svector-ref c-intf 0))
(define (c-intf-decls-set! c-intf x) (set-bbv-version-limit! #f)  (Svector-set! c-intf 0 x))
(define (c-intf-procs c-intf) (set-bbv-version-limit! #f)  (Svector-ref c-intf 1))
(define (c-intf-procs-set! c-intf x) (set-bbv-version-limit! #f)  (Svector-set! c-intf 1 x))
(define (c-intf-inits c-intf) (set-bbv-version-limit! #f)  (Svector-ref c-intf 2))
(define (c-intf-inits-set! c-intf x) (set-bbv-version-limit! #f)  (Svector-set! c-intf 2 x))
(define (c-declaration-expr? source) (set-bbv-version-limit! #f) 
  (and (mymatch **c-declaration-sym 1 source)
       (let ((code (source-code source)))
         (or (string? (source-code (Scadr code)))
             (pt-syntax-error
              source
              "Argument to '##c-declaration' must be a string")))))
(define (c-init-expr? source) (set-bbv-version-limit! #f) 
  (and (mymatch **c-init-sym 1 source)
       (let ((code (source-code source)))
         (or (string? (source-code (Scadr code)))
             (pt-syntax-error
              source
              "Argument to '##c-init' must be a string")))))
(define (c-procedure-expr? source) (set-bbv-version-limit! #f) 
  (and (mymatch **c-procedure-sym 3 source)
       (let ((code (source-code source)))
         (if (not (string? (source-code (Scadddr code))))
             (pt-syntax-error
              source
              "Last argument to '##c-procedure' must be a string")
             (check-arg-and-result-types source (Scadr code) (Scaddr code))))))
(define scheme-to-c-notation
  (list (list void-sym "VOID" "void")
        (list char-sym "CHAR" "char")
        (list signed-char-sym "SCHAR" "signed char")
        (list unsigned-char-sym "UCHAR" "unsigned char")
        (list short-sym "SHORT" "short")
        (list unsigned-short-sym "USHORT" "unsigned short")
        (list int-sym "INT" "int")
        (list unsigned-int-sym "UINT" "unsigned int")
        (list long-sym "LONG" "long")
        (list unsigned-long-sym "ULONG" "unsigned long")
        (list float-sym "FLOAT" "float")
        (list double-sym "DOUBLE" "double")
        (list pointer-sym "POINTER" "void*")
        (list boolean-sym "BOOLEAN" "int")
        (list string-sym "STRING" "char*")
        (list scheme-object-sym "SCMOBJ" "long")))
(define (convert-type typ) (set-bbv-version-limit! #f)  (if (Sassq typ scheme-to-c-notation) typ #f))
(define (check-arg-and-result-types source arg-typs-source res-typ-source) (set-bbv-version-limit! #f) 
  (let ((arg-typs (source-code arg-typs-source))
        (res-typ (source-code res-typ-source)))
    (let ((res-type (convert-type res-typ)))
      (if (not res-type)
          (pt-syntax-error res-typ-source "Invalid result type")
          (if (not (proper-length arg-typs))
              (pt-syntax-error
               arg-typs-source
               "Ill-terminated argument type list")
              (let loop ((lst arg-typs))
                (if (pair? lst)
                    (let* ((arg-typ (source-code (Scar lst)))
                           (arg-type (convert-type arg-typ)))
                      (if (or (not arg-type) (eq? arg-type void-sym))
                          (pt-syntax-error (Scar lst) "Invalid argument type")
                          (loop (Scdr lst))))
                    #t)))))))
(define (add-c-declaration declaration-string) (set-bbv-version-limit! #f) 
  (set! c-interface-decls (cons declaration-string c-interface-decls))
  #f)
(define (add-c-init initialization-code-string) (set-bbv-version-limit! #f) 
  (set! c-interface-inits (cons initialization-code-string c-interface-inits))
  #f)
(define (add-c-proc scheme-name c-name arity def) (set-bbv-version-limit! #f) 
  (set! c-interface-procs
        (cons (vector scheme-name c-name arity def) c-interface-procs))
  #f)
(define (pt-c-procedure source env use) (set-bbv-version-limit! #f) 
  (let* ((code (source-code source))
         (name (build-c-procedure
                (Smap2 source-code (source-code (Scadr code)))
                (source-code (Scaddr code))
                (source-code (Scadddr code))))
         (decl (env-declarations env)))
    (new-ref source decl (env-lookup-global-var env (Sstring->symbol name)))))
(define (build-c-procedure argument-types result-type proc-name-or-code) (set-bbv-version-limit! #f) 
  (define proc-name?
    (let loop ((i (SFX- (Sstring-length proc-name-or-code) 1)))
      (if (SFX>= i 0)
          (let ((c (Sstring-ref proc-name-or-code i)))
            (if (or (Schar-alphabetic? c) (Schar=? c #\_)) (loop (SFX- i 1)) #f))
          #t)))
  (define nl (string #\newline))
  (define undefined-value "UND")
  (define scheme-arg-prefix "ARG")
  (define scheme-result-name "RESULT")
  (define c-arg-prefix "arg")
  (define c-result-name "result")
  (define scheme-to-c-prefix "SCMOBJ_TO_")
  (define c-to-scheme-suffix "_TO_SCMOBJ")
  (define (c-type-name typ) (set-bbv-version-limit! #f)  (Scadr (Sassq typ scheme-to-c-notation)))
  (define (c-type-decl typ) (set-bbv-version-limit! #f)  (Scaddr (Sassq typ scheme-to-c-notation)))
  (define (listify strings) (set-bbv-version-limit! #f) 
    (if (null? strings)
        ""
        (Sstring-append
         (Scar strings)
         (LIBstring-concatenate
                (Smap2 (lambda (s) (set-bbv-version-limit! #f)  (Sstring-append "," s)) (Scdr strings))))))
  (define (scheme-arg-var t) (set-bbv-version-limit! #f) 
    (Sstring-append c-id-prefix scheme-arg-prefix (SFXnumber->string (Scdr t))))
  (define (c-arg-var t) (set-bbv-version-limit! #f) 
    (Sstring-append c-id-prefix c-arg-prefix (SFXnumber->string (Scdr t))))
  (define (make-c-procedure arg-types res-type) (set-bbv-version-limit! #f) 
    (define (make-arg-decl) (set-bbv-version-limit! #f) 
      (LIBstring-concatenate
             (Smap2 (lambda (t) (set-bbv-version-limit! #f) 
                    (Sstring-append
                     (c-type-decl (Scar t))
                     " "
                     (c-arg-var t)
                     ";"
                     nl))
                  arg-types)))
    (define (make-conversions) (set-bbv-version-limit! #f) 
      (if (not (null? arg-types))
          (let loop ((lst arg-types) (str (Sstring-append "if (" nl)))
            (if (null? lst)
                (Sstring-append str "   )" nl)
                (let ((t (Scar lst)) (rest (Scdr lst)))
                  (loop rest
                        (Sstring-append
                         str
                         "    "
                         c-id-prefix
                         scheme-to-c-prefix
                         (c-type-name (Scar t))
                         "("
                         (scheme-arg-var t)
                         ","
                         (c-arg-var t)
                         ")"
                         (if (null? rest) "" " &&")
                         nl)))))
          ""))
    (define (make-body) (set-bbv-version-limit! #f) 
      (if proc-name?
          (let* ((param-list (listify (Smap2 c-arg-var arg-types)))
                 (call (Sstring-append proc-name-or-code "(" param-list ")")))
            (if (eq? res-type void-sym)
                (Sstring-append
                 "{"
                 nl
                 call
                 ";"
                 nl
                 c-id-prefix
                 scheme-result-name
                 " = "
                 c-id-prefix
                 undefined-value
                 ";"
                 nl
                 "}"
                 nl)
                (Sstring-append
                 c-id-prefix
                 (c-type-name res-type)
                 c-to-scheme-suffix
                 "("
                 call
                 ","
                 c-id-prefix
                 scheme-result-name
                 ");"
                 nl)))
          (if (eq? res-type void-sym)
              (Sstring-append
               "{"
               nl
               proc-name-or-code
               nl
               c-id-prefix
               scheme-result-name
               " = "
               c-id-prefix
               undefined-value
               ";"
               nl
               "}"
               nl)
              (Sstring-append
               "{"
               nl
               proc-name-or-code
               nl
               c-id-prefix
               (c-type-name res-type)
               c-to-scheme-suffix
               "("
               c-id-prefix
               c-result-name
               ","
               c-id-prefix
               scheme-result-name
               ");"
               nl
               "}"
               nl))))
    (let* ((index (SFXnumber->string c-interface-proc-count))
           (scheme-name (Sstring-append "#!" c-interface-module-name "#" index))
           (c-name (Sstring-append c-id-prefix (scheme-id->c-id scheme-name)))
           (arity (Slength argument-types))
           (def (Sstring-append
                 (if (or proc-name? (eq? res-type void-sym))
                     ""
                     (Sstring-append
                      (c-type-decl res-type)
                      " "
                      c-id-prefix
                      c-result-name
                      ";"
                      nl))
                 (make-arg-decl)
                 (make-conversions)
                 (make-body))))
      (set! c-interface-proc-count (SFX+ c-interface-proc-count 1))
      (add-c-proc scheme-name c-name arity def)
      scheme-name))
  (let loop ((i 1) (lst1 argument-types) (lst2 '()))
    (if (pair? lst1)
        (loop (SFX+ i 1) (Scdr lst1) (cons (cons (Scar lst1) i) lst2))
        (make-c-procedure (Sreverse lst2) result-type))))
(define (scheme-id->c-id s) (set-bbv-version-limit! #f) 
  (define (hex->char i) (set-bbv-version-limit! #f)  (Sstring-ref "0123456789abcdef" i))
  (let loop ((i (SFX- (Sstring-length s) 1)) (l '()))
    (if (SFX>= i 0)
        (let ((c (Sstring-ref s i)))
          (cond ((or (Schar-alphabetic? c) (Schar-numeric? c))
                 (loop (SFX- i 1) (cons c l)))
                ((Schar=? c #\_) (loop (SFX- i 1) (cons c (cons c l))))
                (else
                 (let ((n (character-encoding c)))
                   (loop (SFX- i 1)
                         (cons #\_
                               (cons (hex->char (SFXquotient n 16))
                                     (cons (hex->char (SFXmodulo n 16)) l))))))))
        (lst->string l))))
(define (pt-syntax-error source msg . args) (set-bbv-version-limit! #f) 
  (apply compiler-user-error
         (cons (source-locat source)
               (cons (Sstring-append "Syntax error -- " msg) args))))
(define (pt source env use) (set-bbv-version-limit! #f) 
  (cond ((macro-expr? source env) (pt (macro-expand source env) env use))
        ((self-eval-expr? source) (pt-self-eval source env use))
        ((quote-expr? source) (pt-quote source env use))
        ((quasiquote-expr? source) (pt-quasiquote source env use))
        ((unquote-expr? source)
         (pt-syntax-error source "Ill-placed 'unquote'"))
        ((unquote-splicing-expr? source)
         (pt-syntax-error source "Ill-placed 'unquote-splicing'"))
        ((var-expr? source env) (pt-var source env use))
        ((set!-expr? source env) (pt-set! source env use))
        ((lambda-expr? source env) (pt-lambda source env use))
        ((if-expr? source) (pt-if source env use))
        ((cond-expr? source) (pt-cond source env use))
        ((and-expr? source) (pt-and source env use))
        ((or-expr? source) (pt-or source env use))
        ((case-expr? source) (pt-case source env use))
        ((let-expr? source env) (pt-let source env use))
        ((let*-expr? source env) (pt-let* source env use))
        ((letrec-expr? source env) (pt-letrec source env use))
        ((begin-expr? source) (pt-begin source env use))
        ((do-expr? source env) (pt-do source env use))
        ((define-expr? source env)
         (pt-syntax-error source "Ill-placed 'define'"))
        ((delay-expr? source env) (pt-delay source env use))
        ((future-expr? source env) (pt-future source env use))
        ((define-macro-expr? source env)
         (pt-syntax-error source "Ill-placed '##define-macro'"))
        ((begin-defs-expr? source)
         (pt-syntax-error source "Ill-placed 'begin' style definitions"))
        ((declare-expr? source)
         (pt-syntax-error source "Ill-placed '##declare'"))
        ((c-declaration-expr? source)
         (pt-syntax-error source "Ill-placed '##c-declaration'"))
        ((c-init-expr? source)
         (pt-syntax-error source "Ill-placed '##c-init'"))
        ((c-procedure-expr? source) (pt-c-procedure source env use))
        ((combination-expr? source) (pt-combination source env use))
        (else (compiler-internal-error "pt, unknown expression type" source))))
(define (macro-expand source env) (set-bbv-version-limit! #f) 
  (let ((code (source-code source)))
    (expression->source
     (apply (Scdr (env-lookup-macro env (source-code (Scar code))))
            (Scdr (source->expression source)))
     source)))
(define (pt-self-eval source env use) (set-bbv-version-limit! #f) 
  (let ((val (source->expression source)))
    (if (eq? use 'none)
        (new-cst source (env-declarations env) undef-object)
        (new-cst source (env-declarations env) val))))
(define (pt-quote source env use) (set-bbv-version-limit! #f) 
  (let ((code (source-code source)))
    (if (eq? use 'none)
        (new-cst source (env-declarations env) undef-object)
        (new-cst source
                 (env-declarations env)
                 (source->expression (Scadr code))))))
(define (pt-quasiquote source env use) (set-bbv-version-limit! #f) 
  (let ((code (source-code source))) (pt-quasiquotation (Scadr code) 1 env)))
(define (pt-quasiquotation form level env) (set-bbv-version-limit! #f) 
  (cond ((SFX= level 0) (pt form env 'true))
        ((quasiquote-expr? form)
         (pt-quasiquotation-list form (source-code form) (SFX+ level 1) env))
        ((unquote-expr? form)
         (if (SFX= level 1)
             (pt (Scadr (source-code form)) env 'true)
             (pt-quasiquotation-list form (source-code form) (SFX- level 1) env)))
        ((unquote-splicing-expr? form)
         (if (SFX= level 1)
             (pt-syntax-error form "Ill-placed 'unquote-splicing'")
             (pt-quasiquotation-list form (source-code form) (SFX- level 1) env)))
        ((pair? (source-code form))
         (pt-quasiquotation-list form (source-code form) level env))
        ((vector? (source-code form))
         (vector-form
          form
          (pt-quasiquotation-list
           form
           (vector->lst (source-code form))
           level
           env)
          env))
        (else
         (new-cst form (env-declarations env) (source->expression form)))))
(define (pt-quasiquotation-list form l level env) (set-bbv-version-limit! #f) 
  (cond ((pair? l)
         (if (and (unquote-splicing-expr? (Scar l)) (SFX= level 1))
             (let ((x (pt (Scadr (source-code (Scar l))) env 'true)))
               (if (null? (Scdr l))
                   x
                   (append-form
                    (Scar l)
                    x
                    (pt-quasiquotation-list form (Scdr l) 1 env)
                    env)))
             (cons-form
              form
              (pt-quasiquotation (Scar l) level env)
              (pt-quasiquotation-list form (Scdr l) level env)
              env)))
        ((null? l) (new-cst form (env-declarations env) '()))
        (else (pt-quasiquotation l level env))))
(define (append-form source ptree1 ptree2 env) (set-bbv-version-limit! #f) 
  (cond ((and (cst? ptree1) (cst? ptree2))
         (new-cst source
                  (env-declarations env)
                  (Sappend (cst-val ptree1) (cst-val ptree2))))
        ((and (cst? ptree2) (null? (cst-val ptree2))) ptree1)
        (else
         (new-call*
          source
          (add-not-safe (env-declarations env))
          (new-ref-extended-bindings source **quasi-append-sym env)
          (list ptree1 ptree2)))))
(define (cons-form source ptree1 ptree2 env) (set-bbv-version-limit! #f) 
  (cond ((and (cst? ptree1) (cst? ptree2))
         (new-cst source
                  (env-declarations env)
                  (cons (cst-val ptree1) (cst-val ptree2))))
        ((and (cst? ptree2) (null? (cst-val ptree2)))
         (new-call*
          source
          (add-not-safe (env-declarations env))
          (new-ref-extended-bindings source **quasi-list-sym env)
          (list ptree1)))
        (else
         (new-call*
          source
          (add-not-safe (env-declarations env))
          (new-ref-extended-bindings source **quasi-cons-sym env)
          (list ptree1 ptree2)))))
(define (vector-form source ptree env) (set-bbv-version-limit! #f) 
  (if (cst? ptree)
      (new-cst source (env-declarations env) (lst->vector (cst-val ptree)))
      (new-call*
       source
       (add-not-safe (env-declarations env))
       (new-ref-extended-bindings source **quasi-list->vector-sym env)
       (list ptree))))
(define (pt-var source env use) (set-bbv-version-limit! #f) 
  (if (eq? use 'none)
      (new-cst source (env-declarations env) undef-object)
      (new-ref source
               (env-declarations env)
               (env-lookup-var env (source-code source) source))))
(define (pt-set! source env use) (set-bbv-version-limit! #f) 
  (let ((code (source-code source)))
    (new-set source
             (env-declarations env)
             (env-lookup-var env (source-code (Scadr code)) (Scadr code))
             (pt (Scaddr code) env 'true))))
(define (pt-lambda source env use) (set-bbv-version-limit! #f) 
  (let ((code (source-code source)))
    (define (new-params parms) (set-bbv-version-limit! #f) 
      (cond ((pair? parms)
             (let* ((parm* (Scar parms))
                    (parm (source-code parm*))
                    (p* (if (pair? parm) (Scar parm) parm*)))
               (cons (make-var (source-code p*) #t (set-empty) (set-empty) p*)
                     (new-params (Scdr parms)))))
            ((null? parms) '())
            (else
             (list (make-var
                    (source-code parms)
                    #t
                    (set-empty)
                    (set-empty)
                    parms)))))
    (define (min-params parms) (set-bbv-version-limit! #f) 
      (let loop ((l parms) (n 0))
        (if (pair? l)
            (if (pair? (source-code (Scar l))) n (loop (Scdr l) (SFX+ n 1)))
            n)))
    (define (rest-param? parms) (set-bbv-version-limit! #f) 
      (if (pair? parms) (rest-param? (Scdr parms)) (not (null? parms))))
    (define (optionals parms source body env) (set-bbv-version-limit! #f) 
      (if (pair? parms)
          (let* ((parm* (Scar parms)) (parm (source-code parm*)))
            (if (and (pair? parm) (length? parm 2))
                (let* ((var (Scar parm))
                       (vars (new-variables (list var)))
                       (decl (env-declarations env)))
                  (new-call*
                   parm*
                   decl
                   (new-prc parm*
                            decl
                            #f
                            1
                            #f
                            vars
                            (optionals
                             (Scdr parms)
                             source
                             body
                             (env-frame env vars)))
                   (list (new-tst parm*
                                  decl
                                  (new-call*
                                   parm*
                                   decl
                                   (new-ref-extended-bindings
                                    parm*
                                    **unassigned?-sym
                                    env)
                                   (list (new-ref parm*
                                                  decl
                                                  (env-lookup-var
                                                   env
                                                   (source-code var)
                                                   var))))
                                  (pt (Scadr parm) env 'true)
                                  (new-ref parm*
                                           decl
                                           (env-lookup-var
                                            env
                                            (source-code var)
                                            var))))))
                (optionals (Scdr parms) source body env)))
          (pt-body source body env 'true)))
    (if (eq? use 'none)
        (new-cst source (env-declarations env) undef-object)
        (let* ((parms (source->parms (Scadr code))) (frame (new-params parms)))
          (new-prc source
                   (env-declarations env)
                   #f
                   (min-params parms)
                   (rest-param? parms)
                   frame
                   (optionals
                    parms
                    source
                    (Scddr code)
                    (env-frame env frame)))))))
(define (source->parms source) (set-bbv-version-limit! #f) 
  (let ((x (source-code source))) (if (or (pair? x) (null? x)) x source)))
(define (pt-body source body env use) (set-bbv-version-limit! #f) 
  (define (letrec-defines vars vals envs body env) (set-bbv-version-limit! #f) 
    (cond ((null? body)
           (pt-syntax-error
            source
            "Body must contain at least one evaluable expression"))
          ((macro-expr? (Scar body) env)
           (letrec-defines
            vars
            vals
            envs
            (cons (macro-expand (Scar body) env) (Scdr body))
            env))
          ((begin-defs-expr? (Scar body))
           (letrec-defines
            vars
            vals
            envs
            (Sappend (begin-defs-body (Scar body)) (Scdr body))
            env))
          ((include-expr? (Scar body))
           (if *ptree-port* (display "  " *ptree-port*))
           (let ((x (file->sources*
                     (include-filename (Scar body))
                     *ptree-port*
                     (source-locat (Scar body)))))
             (if *ptree-port* (newline *ptree-port*))
             (letrec-defines vars vals envs (Sappend x (Scdr body)) env)))
          ((define-expr? (Scar body) env)
           (let* ((var** (definition-variable (Scar body)))
                  (var* (source-code var**))
                  (var (env-define-var env var* var**)))
             (letrec-defines
              (cons var vars)
              (cons (definition-value (Scar body)) vals)
              (cons env envs)
              (Scdr body)
              env)))
          ((declare-expr? (Scar body))
           (letrec-defines
            vars
            vals
            envs
            (Scdr body)
            (add-declarations (Scar body) env)))
          ((define-macro-expr? (Scar body) env)
           (letrec-defines
            vars
            vals
            envs
            (Scdr body)
            (add-macro (Scar body) env)))
          ((c-declaration-expr? (Scar body))
           (add-c-declaration (source-code (Scadr (source-code (Scar body)))))
           (letrec-defines vars vals envs (Scdr body) env))
          ((c-init-expr? (Scar body))
           (add-c-init (source-code (Scadr (source-code (Scar body)))))
           (letrec-defines vars vals envs (Scdr body) env))
          ((null? vars) (pt-sequence source body env use))
          (else
           (let ((vars* (Sreverse vars)))
             (let loop ((vals* '()) (l1 vals) (l2 envs))
               (if (not (null? l1))
                   (loop (cons (pt (Scar l1) (Scar l2) 'true) vals*)
                         (Scdr l1)
                         (Scdr l2))
                   (pt-recursive-let source vars* vals* body env use)))))))
  (letrec-defines '() '() '() body (env-frame env '())))
(define (pt-sequence source seq env use) (set-bbv-version-limit! #f) 
  (if (length? seq 1)
      (pt (Scar seq) env use)
      (new-seq source
               (env-declarations env)
               (pt (Scar seq) env 'none)
               (pt-sequence source (Scdr seq) env use))))
(define (pt-if source env use) (set-bbv-version-limit! #f) 
  (let ((code (source-code source)))
    (new-tst source
             (env-declarations env)
             (pt (Scadr code) env 'pred)
             (pt (Scaddr code) env use)
             (if (length? code 3)
                 (new-cst source (env-declarations env) undef-object)
                 (pt (Scadddr code) env use)))))
(define (pt-cond source env use) (set-bbv-version-limit! #f) 
  (define (pt-clauses clauses) (set-bbv-version-limit! #f) 
    (if (length? clauses 0)
        (new-cst source (env-declarations env) undef-object)
        (let* ((clause* (Scar clauses)) (clause (source-code clause*)))
          (cond ((eq? (source-code (Scar clause)) else-sym)
                 (pt-sequence clause* (Scdr clause) env use))
                ((length? clause 1)
                 (new-disj
                  clause*
                  (env-declarations env)
                  (pt (Scar clause) env (if (eq? use 'true) 'true 'pred))
                  (pt-clauses (Scdr clauses))))
                ((eq? (source-code (Scadr clause)) =>-sym)
                 (new-disj-call
                  clause*
                  (env-declarations env)
                  (pt (Scar clause) env 'true)
                  (pt (Scaddr clause) env 'true)
                  (pt-clauses (Scdr clauses))))
                (else
                 (new-tst clause*
                          (env-declarations env)
                          (pt (Scar clause) env 'pred)
                          (pt-sequence clause* (Scdr clause) env use)
                          (pt-clauses (Scdr clauses))))))))
  (pt-clauses (Scdr (source-code source))))
(define (pt-and source env use) (set-bbv-version-limit! #f) 
  (define (pt-exprs exprs) (set-bbv-version-limit! #f) 
    (cond ((length? exprs 0) (new-cst source (env-declarations env) #t))
          ((length? exprs 1) (pt (Scar exprs) env use))
          (else
           (new-conj
            (Scar exprs)
            (env-declarations env)
            (pt (Scar exprs) env (if (eq? use 'true) 'true 'pred))
            (pt-exprs (Scdr exprs))))))
  (pt-exprs (Scdr (source-code source))))
(define (pt-or source env use) (set-bbv-version-limit! #f) 
  (define (pt-exprs exprs) (set-bbv-version-limit! #f) 
    (cond ((length? exprs 0)
           (new-cst source (env-declarations env) false-object))
          ((length? exprs 1) (pt (Scar exprs) env use))
          (else
           (new-disj
            (Scar exprs)
            (env-declarations env)
            (pt (Scar exprs) env (if (eq? use 'true) 'true 'pred))
            (pt-exprs (Scdr exprs))))))
  (pt-exprs (Scdr (source-code source))))
(define (pt-case source env use) (set-bbv-version-limit! #f) 
  (let ((code (source-code source)) (temp (new-temps source '(temp))))
    (define (pt-clauses clauses) (set-bbv-version-limit! #f) 
      (if (length? clauses 0)
          (new-cst source (env-declarations env) undef-object)
          (let* ((clause* (Scar clauses)) (clause (source-code clause*)))
            (if (eq? (source-code (Scar clause)) else-sym)
                (pt-sequence clause* (Scdr clause) env use)
                (new-tst clause*
                         (env-declarations env)
                         (new-call*
                          clause*
                          (add-not-safe (env-declarations env))
                          (new-ref-extended-bindings
                           clause*
                           **case-memv-sym
                           env)
                          (list (new-ref clause*
                                         (env-declarations env)
                                         (Scar temp))
                                (new-cst (Scar clause)
                                         (env-declarations env)
                                         (source->expression (Scar clause)))))
                         (pt-sequence clause* (Scdr clause) env use)
                         (pt-clauses (Scdr clauses)))))))
    (new-call*
     source
     (env-declarations env)
     (new-prc source
              (env-declarations env)
              #f
              1
              #f
              temp
              (pt-clauses (Scddr code)))
     (list (pt (Scadr code) env 'true)))))
(define (pt-let source env use) (set-bbv-version-limit! #f) 
  (let ((code (source-code source)))
    (if (bindable-var? (Scadr code) env)
        (let* ((self (new-variables (list (Scadr code))))
               (bindings (Smap2 source-code (source-code (Scaddr code))))
               (vars (new-variables (Smap2 (lambda (x) (set-bbv-version-limit! #f)  (Scar x)) bindings)))
               (vals (Smap2 (lambda (x) (set-bbv-version-limit! #f)  (pt (Scadr x) env 'true)) bindings))
               (env (env-frame (env-frame env vars) self))
               (self-proc
                (list (new-prc source
                               (env-declarations env)
                               #f
                               (Slength vars)
                               #f
                               vars
                               (pt-body source (Scdddr code) env use)))))
          (set-prc-names! self self-proc)
          (set-prc-names! vars vals)
          (new-call*
           source
           (env-declarations env)
           (new-prc source
                    (env-declarations env)
                    #f
                    1
                    #f
                    self
                    (new-call*
                     source
                     (env-declarations env)
                     (new-ref source (env-declarations env) (Scar self))
                     vals))
           self-proc))
        (if (null? (source-code (Scadr code)))
            (pt-body source (Scddr code) env use)
            (let* ((bindings (Smap2 source-code (source-code (Scadr code))))
                   (vars (new-variables (Smap2 (lambda (x) (set-bbv-version-limit! #f)  (Scar x)) bindings)))
                   (vals (Smap2 (lambda (x) (set-bbv-version-limit! #f)  (pt (Scadr x) env 'true)) bindings))
                   (env (env-frame env vars)))
              (set-prc-names! vars vals)
              (new-call*
               source
               (env-declarations env)
               (new-prc source
                        (env-declarations env)
                        #f
                        (Slength vars)
                        #f
                        vars
                        (pt-body source (Scddr code) env use))
               vals))))))
(define (pt-let* source env use) (set-bbv-version-limit! #f) 
  (let ((code (source-code source)))
    (define (pt-bindings bindings env use) (set-bbv-version-limit! #f) 
      (if (null? bindings)
          (pt-body source (Scddr code) env use)
          (let* ((binding* (Scar bindings))
                 (binding (source-code binding*))
                 (vars (new-variables (list (Scar binding))))
                 (vals (list (pt (Scadr binding) env 'true)))
                 (env (env-frame env vars)))
            (set-prc-names! vars vals)
            (new-call*
             binding*
             (env-declarations env)
             (new-prc binding*
                      (env-declarations env)
                      #f
                      1
                      #f
                      vars
                      (pt-bindings (Scdr bindings) env use))
             vals))))
    (pt-bindings (source-code (Scadr code)) env use)))
(define (pt-letrec source env use) (set-bbv-version-limit! #f) 
  (let* ((code (source-code source))
         (bindings (Smap2 source-code (source-code (Scadr code))))
         (vars* (new-variables (Smap2 (lambda (x) (set-bbv-version-limit! #f)  (Scar x)) bindings)))
         (env* (env-frame env vars*)))
    (pt-recursive-let
     source
     vars*
     (Smap2 (lambda (x) (set-bbv-version-limit! #f)  (pt (Scadr x) env* 'true)) bindings)
     (Scddr code)
     env*
     use)))
(define (pt-recursive-let source vars vals body env use) (set-bbv-version-limit! #f) 
  (define (dependency-graph vars vals) (set-bbv-version-limit! #f) 
    (define (dgraph vars* vals*) (set-bbv-version-limit! #f) 
      (if (null? vars*)
          (set-empty)
          (let ((var (Scar vars*)) (val (Scar vals*)))
            (set-adjoin
             (dgraph (Scdr vars*) (Scdr vals*))
             (make-gnode
              var
              (set-intersection (list->set vars) (free-variables val)))))))
    (dgraph vars vals))
  (define (val-of var) (set-bbv-version-limit! #f) 
    (Slist-ref vals (SFX- (Slength vars) (Slength (Smemq var vars)))))
  (define (bind-in-order order) (set-bbv-version-limit! #f) 
    (if (null? order)
        (pt-body source body env use)
        (let* ((vars-set (Scar order)) (vars (set->list vars-set)))
          (let loop1 ((l (Sreverse vars))
                      (vars-b '())
                      (vals-b '())
                      (vars-a '()))
            (if (not (null? l))
                (let* ((var (Scar l)) (val (val-of var)))
                  (if (or (prc? val)
                          (set-empty?
                           (set-intersection (free-variables val) vars-set)))
                      (loop1 (Scdr l)
                             (cons var vars-b)
                             (cons val vals-b)
                             vars-a)
                      (loop1 (Scdr l) vars-b vals-b (cons var vars-a))))
                (let* ((result1 (let loop2 ((l vars-a))
                                  (if (not (null? l))
                                      (let* ((var (Scar l)) (val (val-of var)))
                                        (new-seq source
                                                 (env-declarations env)
                                                 (new-set source
                                                          (env-declarations
                                                           env)
                                                          var
                                                          val)
                                                 (loop2 (Scdr l))))
                                      (bind-in-order (Scdr order)))))
                       (result2 (if (null? vars-b)
                                    result1
                                    (new-call*
                                     source
                                     (env-declarations env)
                                     (new-prc source
                                              (env-declarations env)
                                              #f
                                              (Slength vars-b)
                                              #f
                                              vars-b
                                              result1)
                                     vals-b)))
                       (result3 (if (null? vars-a)
                                    result2
                                    (new-call*
                                     source
                                     (env-declarations env)
                                     (new-prc source
                                              (env-declarations env)
                                              #f
                                              (Slength vars-a)
                                              #f
                                              vars-a
                                              result2)
                                     (Smap2 (lambda (var) (set-bbv-version-limit! #f) 
                                            (new-cst source
                                                     (env-declarations env)
                                                     undef-object))
                                          vars-a)))))
                  result3))))))
  (set-prc-names! vars vals)
  (bind-in-order
   (topological-sort (transitive-closure (dependency-graph vars vals)))))
(define (pt-begin source env use) (set-bbv-version-limit! #f) 
  (pt-sequence source (Scdr (source-code source)) env use))
(define (pt-do source env use) (set-bbv-version-limit! #f) 
  (let* ((code (source-code source))
         (loop (new-temps source '(loop)))
         (bindings (Smap2 source-code (source-code (Scadr code))))
         (vars (new-variables (Smap2 (lambda (x) (set-bbv-version-limit! #f)  (Scar x)) bindings)))
         (init (Smap2 (lambda (x) (set-bbv-version-limit! #f)  (pt (Scadr x) env 'true)) bindings))
         (env (env-frame env vars))
         (step (Smap2 (lambda (x) (set-bbv-version-limit! #f) 
                      (pt (if (length? x 2) (Scar x) (Scaddr x)) env 'true))
                    bindings))
         (exit (source-code (Scaddr code))))
    (set-prc-names! vars init)
    (new-call*
     source
     (env-declarations env)
     (new-prc source
              (env-declarations env)
              #f
              1
              #f
              loop
              (new-call*
               source
               (env-declarations env)
               (new-ref source (env-declarations env) (Scar loop))
               init))
     (list (new-prc source
                    (env-declarations env)
                    #f
                    (Slength vars)
                    #f
                    vars
                    (new-tst source
                             (env-declarations env)
                             (pt (Scar exit) env 'pred)
                             (if (length? exit 1)
                                 (new-cst (Scaddr code)
                                          (env-declarations env)
                                          undef-object)
                                 (pt-sequence (Scaddr code) (Scdr exit) env use))
                             (if (length? code 3)
                                 (new-call*
                                  source
                                  (env-declarations env)
                                  (new-ref source
                                           (env-declarations env)
                                           (Scar loop))
                                  step)
                                 (new-seq source
                                          (env-declarations env)
                                          (pt-sequence
                                           source
                                           (Scdddr code)
                                           env
                                           'none)
                                          (new-call*
                                           source
                                           (env-declarations env)
                                           (new-ref source
                                                    (env-declarations env)
                                                    (Scar loop))
                                           step)))))))))
(define (pt-combination source env use) (set-bbv-version-limit! #f) 
  (let* ((code (source-code source))
         (oper (pt (Scar code) env 'true))
         (decl (node-decl oper)))
    (new-call*
     source
     (env-declarations env)
     oper
     (Smap2 (lambda (x) (set-bbv-version-limit! #f)  (pt x env 'true)) (Scdr code)))))
(define (pt-delay source env use) (set-bbv-version-limit! #f) 
  (let ((code (source-code source)))
    (new-call*
     source
     (add-not-safe (env-declarations env))
     (new-ref-extended-bindings source **make-placeholder-sym env)
     (list (new-prc source
                    (env-declarations env)
                    #f
                    0
                    #f
                    '()
                    (pt (Scadr code) env 'true))))))
(define (pt-future source env use) (set-bbv-version-limit! #f) 
  (let ((decl (env-declarations env)) (code (source-code source)))
    (new-fut source decl (pt (Scadr code) env 'true))))
(define (self-eval-expr? source) (set-bbv-version-limit! #f) 
  (let ((code (source-code source)))
    (and (not (pair? code)) (not (symbol-object? code)))))
(define (quote-expr? source) (set-bbv-version-limit! #f)  (mymatch quote-sym 1 source))
(define (quasiquote-expr? source) (set-bbv-version-limit! #f)  (mymatch quasiquote-sym 1 source))
(define (unquote-expr? source) (set-bbv-version-limit! #f)  (mymatch unquote-sym 1 source))
(define (unquote-splicing-expr? source) (set-bbv-version-limit! #f) 
  (mymatch unquote-splicing-sym 1 source))
(define (var-expr? source env) (set-bbv-version-limit! #f) 
  (let ((code (source-code source)))
    (and (symbol-object? code)
         (not-keyword source env code)
         (not-macro source env code))))
(define (not-macro source env name) (set-bbv-version-limit! #f) 
  (if (env-lookup-macro env name)
      (pt-syntax-error source "Macro name can't be used as a variable:" name)
      #t))
(define (bindable-var? source env) (set-bbv-version-limit! #f) 
  (let ((code (source-code source)))
    (and (symbol-object? code) (not-keyword source env code))))
(define (not-keyword source env name) (set-bbv-version-limit! #f) 
  (if (or (Smemq name common-keywords)
          (Smemq name
                (dialect-specific-keywords
                 (scheme-dialect (env-declarations env)))))
      (pt-syntax-error
       source
       "Predefined keyword can't be used as a variable:"
       name)
      #t))
(define (set!-expr? source env) (set-bbv-version-limit! #f) 
  (and (mymatch set!-sym 2 source)
       (var-expr? (Scadr (source-code source)) env)))
(define (lambda-expr? source env) (set-bbv-version-limit! #f) 
  (and (mymatch lambda-sym -2 source)
       (proper-parms? (source->parms (Scadr (source-code source))) env)))
(define (if-expr? source) (set-bbv-version-limit! #f) 
  (and (mymatch if-sym -2 source)
       (or (SFX<= (Slength (source-code source)) 4)
           (pt-syntax-error source "Ill-formed special form" if-sym))))
(define (cond-expr? source) (set-bbv-version-limit! #f) 
  (and (mymatch cond-sym -1 source) (proper-clauses? source)))
(define (and-expr? source) (set-bbv-version-limit! #f)  (mymatch and-sym 0 source))
(define (or-expr? source) (set-bbv-version-limit! #f)  (mymatch or-sym 0 source))
(define (case-expr? source) (set-bbv-version-limit! #f) 
  (and (mymatch case-sym -2 source) (proper-case-clauses? source)))
(define (let-expr? source env) (set-bbv-version-limit! #f) 
  (and (mymatch let-sym -2 source)
       (let ((code (source-code source)))
         (if (bindable-var? (Scadr code) env)
             (and (proper-bindings? (Scaddr code) #t env)
                  (or (SFX> (Slength code) 3)
                      (pt-syntax-error source "Ill-formed named 'let'")))
             (proper-bindings? (Scadr code) #t env)))))
(define (let*-expr? source env) (set-bbv-version-limit! #f) 
  (and (mymatch let*-sym -2 source)
       (proper-bindings? (Scadr (source-code source)) #f env)))
(define (letrec-expr? source env) (set-bbv-version-limit! #f) 
  (and (mymatch letrec-sym -2 source)
       (proper-bindings? (Scadr (source-code source)) #t env)))
(define (begin-expr? source) (set-bbv-version-limit! #f)  (mymatch begin-sym -1 source))
(define (do-expr? source env) (set-bbv-version-limit! #f) 
  (and (mymatch do-sym -2 source)
       (proper-do-bindings? source env)
       (proper-do-exit? source)))
(define (define-expr? source env) (set-bbv-version-limit! #f) 
  (and (mymatch define-sym -1 source)
       (proper-definition? source env)
       (let ((v (definition-variable source)))
         (not-macro v env (source-code v)))))
(define (combination-expr? source) (set-bbv-version-limit! #f) 
  (let ((length (proper-length (source-code source))))
    (if length
        (or (SFX> length 0) (pt-syntax-error source "Ill-formed procedure call"))
        (pt-syntax-error source "Ill-terminated procedure call"))))
(define (delay-expr? source env) (set-bbv-version-limit! #f) 
  (and (not (eq? (scheme-dialect (env-declarations env)) ieee-scheme-sym))
       (mymatch delay-sym 1 source)))
(define (future-expr? source env) (set-bbv-version-limit! #f) 
  (and (eq? (scheme-dialect (env-declarations env)) multilisp-sym)
       (mymatch future-sym 1 source)))
(define (macro-expr? source env) (set-bbv-version-limit! #f) 
  (let ((code (source-code source)))
    (and (pair? code)
         (symbol-object? (source-code (Scar code)))
         (let ((macr (env-lookup-macro env (source-code (Scar code)))))
           (and macr
                (let ((len (proper-length (Scdr code))))
                  (if len
                      (let ((len* (SFX+ len 1)) (size (Scar macr)))
                        (or (if (SFX> size 0) (SFX= len* size) (SFX>= len* (SFX- size)))
                            (pt-syntax-error source "Ill-formed macro form")))
                      (pt-syntax-error
                       source
                       "Ill-terminated macro form"))))))))
(define (define-macro-expr? source env) (set-bbv-version-limit! #f) 
  (and (mymatch **define-macro-sym -1 source) (proper-definition? source env)))
(define (declare-expr? source) (set-bbv-version-limit! #f)  (mymatch **declare-sym -1 source))
(define (include-expr? source) (set-bbv-version-limit! #f)  (mymatch **include-sym 1 source))
(define (begin-defs-expr? source) (set-bbv-version-limit! #f)  (mymatch begin-sym 0 source))
(define (mymatch keyword size source) (set-bbv-version-limit! #f) 
  (let ((code (source-code source)))
    (and (pair? code)
         (eq? (source-code (Scar code)) keyword)
         (let ((length (proper-length (Scdr code))))
           (if length
               (or (if (SFX> size 0) (SFX= length size) (SFX>= length (SFX- size)))
                   (pt-syntax-error source "Ill-formed special form" keyword))
               (pt-syntax-error
                source
                "Ill-terminated special form"
                keyword))))))
(define (proper-length l) (set-bbv-version-limit! #f) 
  (define (length l n) (set-bbv-version-limit! #f) 
    (cond ((pair? l) (length (Scdr l) (SFX+ n 1))) ((null? l) n) (else #f)))
  (length l 0))
(define (proper-definition? source env) (set-bbv-version-limit! #f) 
  (let* ((code (source-code source))
         (pattern* (Scadr code))
         (pattern (source-code pattern*))
         (body (Scddr code)))
    (cond ((bindable-var? pattern* env)
           (cond ((length? body 0) #t)
                 ((length? body 1) #t)
                 (else (pt-syntax-error source "Ill-formed definition body"))))
          ((pair? pattern)
           (if (length? body 0)
               (pt-syntax-error
                source
                "Body of a definition must have at least one expression"))
           (if (bindable-var? (Scar pattern) env)
               (proper-parms? (Scdr pattern) env)
               (pt-syntax-error
                (Scar pattern)
                "Procedure name must be an identifier")))
          (else (pt-syntax-error pattern* "Ill-formed definition pattern")))))
(define (definition-variable def) (set-bbv-version-limit! #f) 
  (let* ((code (source-code def)) (pattern (Scadr code)))
    (if (pair? (source-code pattern)) (Scar (source-code pattern)) pattern)))
(define (definition-value def) (set-bbv-version-limit! #f) 
  (let ((code (source-code def)) (loc (source-locat def)))
    (cond ((pair? (source-code (Scadr code)))
           (make-source
            (cons (make-source lambda-sym loc)
                  (cons (parms->source (Scdr (source-code (Scadr code))) loc)
                        (Scddr code)))
            loc))
          ((null? (Scddr code))
           (make-source
            (list (make-source quote-sym loc) (make-source undef-object loc))
            loc))
          (else (Scaddr code)))))
(define (parms->source parms loc) (set-bbv-version-limit! #f) 
  (if (or (pair? parms) (null? parms)) (make-source parms loc) parms))
(define (proper-parms? parms env) (set-bbv-version-limit! #f) 
  (define (proper-parms parms seen optional-seen) (set-bbv-version-limit! #f) 
    (cond ((pair? parms)
           (let* ((parm* (Scar parms)) (parm (source-code parm*)))
             (cond ((pair? parm)
                    (if (eq? (scheme-dialect (env-declarations env))
                             multilisp-sym)
                        (let ((Slength (proper-length parm)))
                          (if (or (eqv? length 1) (eqv? length 2))
                              (let ((var (Scar parm)))
                                (if (bindable-var? var env)
                                    (if (Smemq (source-code var) seen)
                                        (pt-syntax-error
                                         var
                                         "Duplicate parameter in parameter list")
                                        (proper-parms
                                         (Scdr parms)
                                         (cons (source-code var) seen)
                                         #t))
                                    (pt-syntax-error
                                     var
                                     "Parameter must be an identifier")))
                              (pt-syntax-error
                               parm*
                               "Ill-formed optional parameter")))
                        (pt-syntax-error
                         parm*
                         "optional parameters illegal in this dialect")))
                   (optional-seen
                    (pt-syntax-error parm* "Optional parameter expected"))
                   ((bindable-var? parm* env)
                    (if (Smemq parm seen)
                        (pt-syntax-error
                         parm*
                         "Duplicate parameter in parameter list"))
                    (proper-parms (Scdr parms) (cons parm seen) #f))
                   (else
                    (pt-syntax-error
                     parm*
                     "Parameter must be an identifier")))))
          ((null? parms) #t)
          ((bindable-var? parms env)
           (if (Smemq (source-code parms) seen)
               (pt-syntax-error parms "Duplicate parameter in parameter list")
               #t))
          (else
           (pt-syntax-error parms "Rest parameter must be an identifier"))))
  (proper-parms parms '() #f))
(define (proper-clauses? source) (set-bbv-version-limit! #f) 
  (define (proper-clauses clauses) (set-bbv-version-limit! #f) 
    (or (null? clauses)
        (let* ((clause* (Scar clauses))
               (clause (source-code clause*))
               (length (proper-length clause)))
          (if length
              (if (SFX>= length 1)
                  (if (eq? (source-code (Scar clause)) else-sym)
                      (cond ((SFX= length 1)
                             (pt-syntax-error
                              clause*
                              "Else clause must have a body"))
                            ((not (null? (Scdr clauses)))
                             (pt-syntax-error
                              clause*
                              "Else clause must be the last clause"))
                            (else (proper-clauses (Scdr clauses))))
                      (if (and (SFX>= length 2)
                               (eq? (source-code (Scadr clause)) =>-sym)
                               (not (SFX= length 3)))
                          (pt-syntax-error
                           (Scadr clause)
                           "'=>' must be followed by a single expression")
                          (proper-clauses (Scdr clauses))))
                  (pt-syntax-error clause* "Ill-formed 'cond' clause"))
              (pt-syntax-error clause* "Ill-terminated 'cond' clause")))))
  (proper-clauses (Scdr (source-code source))))
(define (proper-case-clauses? source) (set-bbv-version-limit! #f) 
  (define (proper-case-clauses clauses) (set-bbv-version-limit! #f) 
    (or (null? clauses)
        (let* ((clause* (Scar clauses))
               (clause (source-code clause*))
               (length (proper-length clause)))
          (if length
              (if (SFX>= length 2)
                  (if (eq? (source-code (Scar clause)) else-sym)
                      (if (not (null? (Scdr clauses)))
                          (pt-syntax-error
                           clause*
                           "Else clause must be the last clause")
                          (proper-case-clauses (Scdr clauses)))
                      (begin
                        (proper-selector-list? (Scar clause))
                        (proper-case-clauses (Scdr clauses))))
                  (pt-syntax-error
                   clause*
                   "A 'case' clause must have a selector list and a body"))
              (pt-syntax-error clause* "Ill-terminated 'case' clause")))))
  (proper-case-clauses (Scddr (source-code source))))
(define (proper-selector-list? source) (set-bbv-version-limit! #f) 
  (let* ((code (source-code source)) (length (proper-length code)))
    (if length
        (or (SFX>= length 1)
            (pt-syntax-error
             source
             "Selector list must contain at least one element"))
        (pt-syntax-error source "Ill-terminated selector list"))))
(define (proper-bindings? bindings check-dupl? env) (set-bbv-version-limit! #f) 
  (define (proper-bindings l seen) (set-bbv-version-limit! #f) 
    (cond ((pair? l)
           (let* ((binding* (Scar l)) (binding (source-code binding*)))
             (if (eqv? (proper-length binding) 2)
                 (let ((var (Scar binding)))
                   (if (bindable-var? var env)
                       (if (and check-dupl? (Smemq (source-code var) seen))
                           (pt-syntax-error
                            var
                            "Duplicate variable in bindings")
                           (proper-bindings
                            (Scdr l)
                            (cons (source-code var) seen)))
                       (pt-syntax-error
                        var
                        "Binding variable must be an identifier")))
                 (pt-syntax-error binding* "Ill-formed binding"))))
          ((null? l) #t)
          (else (pt-syntax-error bindings "Ill-terminated binding list"))))
  (proper-bindings (source-code bindings) '()))
(define (proper-do-bindings? source env) (set-bbv-version-limit! #f) 
  (let ((bindings (Scadr (source-code source))))
    (define (proper-bindings l seen) (set-bbv-version-limit! #f) 
      (cond ((pair? l)
             (let* ((binding* (Scar l))
                    (binding (source-code binding*))
                    (length (proper-length binding)))
               (if (or (eqv? length 2) (eqv? length 3))
                   (let ((var (Scar binding)))
                     (if (bindable-var? var env)
                         (if (Smemq (source-code var) seen)
                             (pt-syntax-error
                              var
                              "Duplicate variable in bindings")
                             (proper-bindings
                              (Scdr l)
                              (cons (source-code var) seen)))
                         (pt-syntax-error
                          var
                          "Binding variable must be an identifier")))
                   (pt-syntax-error binding* "Ill-formed binding"))))
            ((null? l) #t)
            (else (pt-syntax-error bindings "Ill-terminated binding list"))))
    (proper-bindings (source-code bindings) '())))
(define (proper-do-exit? source) (set-bbv-version-limit! #f) 
  (let* ((code (source-code (Scaddr (source-code source))))
         (length (proper-length code)))
    (if length
        (or (SFX> length 0) (pt-syntax-error source "Ill-formed exit clause"))
        (pt-syntax-error source "Ill-terminated exit clause"))))
(define (include-filename source) (set-bbv-version-limit! #f)  (source-code (Scadr (source-code source))))
(define (begin-defs-body source) (set-bbv-version-limit! #f)  (Scdr (source-code source)))
(define (length? l n) (set-bbv-version-limit! #f) 
  (cond ((null? l) (SFX= n 0)) ((SFX> n 0) (length? (Scdr l) (SFX- n 1))) (else #f)))
(define (transform-declaration source) (set-bbv-version-limit! #f) 
  (let ((code (source-code source)))
    (if (not (pair? code))
        (pt-syntax-error source "Ill-formed declaration")
        (let* ((pos (not (eq? (source-code (Scar code)) not-sym)))
               (x (if pos code (Scdr code))))
          (if (not (pair? x))
              (pt-syntax-error source "Ill-formed declaration")
              (let* ((id* (Scar x)) (id (source-code id*)))
                (cond ((not (symbol-object? id))
                       (pt-syntax-error
                        id*
                        "Declaration name must be an identifier"))
                      ((Sassq id flag-declarations)
                       (cond ((not pos)
                              (pt-syntax-error
                               id*
                               "Declaration can't be negated"))
                             ((null? (Scdr x))
                              (flag-decl
                               source
                               (Scdr (Sassq id flag-declarations))
                               id))
                             (else
                              (pt-syntax-error
                               source
                               "Ill-formed declaration"))))
                      ((Smemq id parameterized-declarations)
                       (cond ((not pos)
                              (pt-syntax-error
                               id*
                               "Declaration can't be negated"))
                             ((eqv? (proper-length x) 2)
                              (parameterized-decl
                               source
                               id
                               (source->expression (Scadr x))))
                             (else
                              (pt-syntax-error
                               source
                               "Ill-formed declaration"))))
                      ((Smemq id boolean-declarations)
                       (if (null? (Scdr x))
                           (boolean-decl source id pos)
                           (pt-syntax-error source "Ill-formed declaration")))
                      ((Sassq id namable-declarations)
                       (cond ((not pos)
                              (pt-syntax-error
                               id*
                               "Declaration can't be negated"))
                             (else
                              (namable-decl
                               source
                               (Scdr (Sassq id namable-declarations))
                               id
                               (Smap2 source->expression (Scdr x))))))
                      ((Smemq id namable-boolean-declarations)
                       (namable-boolean-decl
                        source
                        id
                        pos
                        (Smap2 source->expression (Scdr x))))
                      ((Smemq id namable-string-declarations)
                       (if (not (pair? (Scdr x)))
                           (pt-syntax-error source "Ill-formed declaration")
                           (let* ((str* (Scadr x)) (str (source-code str*)))
                             (cond ((not pos)
                                    (pt-syntax-error
                                     id*
                                     "Declaration can't be negated"))
                                   ((not (string? str))
                                    (pt-syntax-error str* "String expected"))
                                   (else
                                    (namable-string-decl
                                     source
                                     id
                                     str
                                     (Smap2 source->expression (Scddr x))))))))
                      (else (pt-syntax-error id* "Unknown declaration")))))))))
(define (add-declarations source env) (set-bbv-version-limit! #f) 
  (let loop ((l (Scdr (source-code source))) (env env))
    (if (pair? l)
        (loop (Scdr l) (env-declare env (transform-declaration (Scar l))))
        env)))
(define (add-decl d decl) (set-bbv-version-limit! #f)  (env-declare decl d))
(define (add-macro source env) (set-bbv-version-limit! #f) 
  (define (form-size parms) (set-bbv-version-limit! #f) 
    (let loop ((l parms) (n 1))
      (if (pair? l) (loop (Scdr l) (SFX+ n 1)) (if (null? l) n (SFX- n)))))
  (define (error-proc . msgs) (set-bbv-version-limit! #f) 
    (apply compiler-user-error
           (cons (source-locat source) (cons "(in macro body)" msgs))))
  (let ((var (definition-variable source)) (proc (definition-value source)))
    (if (lambda-expr? proc env)
        (env-macro
         env
         (source-code var)
         (cons (form-size (source->parms (Scadr (source-code proc))))
               (scheme-global-eval (source->expression proc) error-proc)))
        (pt-syntax-error source "Macro value must be a lambda expression"))))
(define (ptree.begin! info-port) (set-bbv-version-limit! #f)  (set! *ptree-port* info-port) '())
(define (ptree.end!) (set-bbv-version-limit! #f)  '())
(define *ptree-port* '())
(define (normalize-parse-tree ptree env) (set-bbv-version-limit! #f) 
  (define (normalize ptree) (set-bbv-version-limit! #f) 
    (let ((tree (assignment-convert (partial-evaluate ptree) env)))
      (lambda-lift! tree)
      tree))
  (if (def? ptree)
      (begin
        (node-children-set! ptree (list (normalize (def-val ptree))))
        ptree)
      (normalize ptree)))
(define (partial-evaluate ptree) (set-bbv-version-limit! #f)  (pe ptree '()))
(define (pe ptree consts) (set-bbv-version-limit! #f) 
  (cond ((cst? ptree)
         (new-cst (node-source ptree) (node-decl ptree) (cst-val ptree)))
        ((ref? ptree)
         (let ((var (ref-var ptree)))
           (var-refs-set! var (set-remove (var-refs var) ptree))
           (let ((x (Sassq var consts)))
             (if x
                 (new-cst (node-source ptree) (node-decl ptree) (Scdr x))
                 (let ((y (global-val var)))
                   (if (and y (cst? y))
                       (new-cst (node-source ptree)
                                (node-decl ptree)
                                (cst-val y))
                       (new-ref (node-source ptree)
                                (node-decl ptree)
                                var)))))))
        ((set? ptree)
         (let ((var (set-var ptree)) (val (pe (set-val ptree) consts)))
           (var-sets-set! var (set-remove (var-sets var) ptree))
           (new-set (node-source ptree) (node-decl ptree) var val)))
        ((tst? ptree)
         (let ((pre (pe (tst-pre ptree) consts)))
           (if (cst? pre)
               (let ((val (cst-val pre)))
                 (if (false-object? val)
                     (pe (tst-alt ptree) consts)
                     (pe (tst-con ptree) consts)))
               (new-tst (node-source ptree)
                        (node-decl ptree)
                        pre
                        (pe (tst-con ptree) consts)
                        (pe (tst-alt ptree) consts)))))
        ((conj? ptree)
         (let ((pre (pe (conj-pre ptree) consts)))
           (if (cst? pre)
               (let ((val (cst-val pre)))
                 (if (false-object? val) pre (pe (conj-alt ptree) consts)))
               (new-conj
                (node-source ptree)
                (node-decl ptree)
                pre
                (pe (conj-alt ptree) consts)))))
        ((disj? ptree)
         (let ((pre (pe (disj-pre ptree) consts)))
           (if (cst? pre)
               (let ((val (cst-val pre)))
                 (if (false-object? val) (pe (disj-alt ptree) consts) pre))
               (new-disj
                (node-source ptree)
                (node-decl ptree)
                pre
                (pe (disj-alt ptree) consts)))))
        ((prc? ptree)
         (new-prc (node-source ptree)
                  (node-decl ptree)
                  (prc-name ptree)
                  (prc-min ptree)
                  (prc-rest ptree)
                  (prc-parms ptree)
                  (pe (prc-body ptree) consts)))
        ((app? ptree)
         (let ((oper (app-oper ptree)) (args (app-args ptree)))
           (if (and (prc? oper)
                    (not (prc-rest oper))
                    (SFX= (Slength (prc-parms oper)) (Slength args)))
               (pe-let ptree consts)
               (new-call
                (node-source ptree)
                (node-decl ptree)
                (pe oper consts)
                (Smap2 (lambda (x) (set-bbv-version-limit! #f)  (pe x consts)) args)))))
        ((fut? ptree)
         (new-fut (node-source ptree)
                  (node-decl ptree)
                  (pe (fut-val ptree) consts)))
        (else (compiler-internal-error "pe, unknown parse tree node type"))))
(define (pe-let ptree consts) (set-bbv-version-limit! #f) 
  (let* ((proc (app-oper ptree))
         (vals (app-args ptree))
         (vars (prc-parms proc))
         (non-mut-vars (set-keep not-mutable? (list->set vars))))
    (for-each
     (lambda (var) (set-bbv-version-limit! #f) 
       (var-refs-set! var (set-empty))
       (var-sets-set! var (set-empty)))
     vars)
    (let loop ((l vars)
               (v vals)
               (new-vars '())
               (new-vals '())
               (new-consts consts))
      (if (null? l)
          (if (null? new-vars)
              (pe (prc-body proc) new-consts)
              (new-call
               (node-source ptree)
               (node-decl ptree)
               (new-prc (node-source proc)
                        (node-decl proc)
                        #f
                        (Slength new-vars)
                        #f
                        (Sreverse new-vars)
                        (pe (prc-body proc) new-consts))
               (Sreverse new-vals)))
          (let ((var (Scar l)) (val (pe (Scar v) consts)))
            (if (and (set-member? var non-mut-vars) (cst? val))
                (loop (Scdr l)
                      (Scdr v)
                      new-vars
                      new-vals
                      (cons (cons var (cst-val val)) new-consts))
                (loop (Scdr l)
                      (Scdr v)
                      (cons var new-vars)
                      (cons val new-vals)
                      new-consts)))))))
(define (assignment-convert ptree env) (set-bbv-version-limit! #f) 
  (ac ptree (env-declare env (list safe-sym #f)) '()))
(define (ac ptree env mut) (set-bbv-version-limit! #f) 
  (cond ((cst? ptree) ptree)
        ((ref? ptree)
         (let ((var (ref-var ptree)))
           (if (global? var)
               ptree
               (let ((x (Sassq var mut)))
                 (if x
                     (let ((source (node-source ptree)))
                       (var-refs-set! var (set-remove (var-refs var) ptree))
                       (new-call
                        source
                        (node-decl ptree)
                        (new-ref-extended-bindings source **cell-ref-sym env)
                        (list (new-ref source (node-decl ptree) (Scdr x)))))
                     ptree)))))
        ((set? ptree)
         (let ((var (set-var ptree))
               (source (node-source ptree))
               (val (ac (set-val ptree) env mut)))
           (var-sets-set! var (set-remove (var-sets var) ptree))
           (if (global? var)
               (new-set source (node-decl ptree) var val)
               (new-call
                source
                (node-decl ptree)
                (new-ref-extended-bindings source **cell-set!-sym env)
                (list (new-ref source (node-decl ptree) (Scdr (Sassq var mut)))
                      val)))))
        ((tst? ptree)
         (new-tst (node-source ptree)
                  (node-decl ptree)
                  (ac (tst-pre ptree) env mut)
                  (ac (tst-con ptree) env mut)
                  (ac (tst-alt ptree) env mut)))
        ((conj? ptree)
         (new-conj
          (node-source ptree)
          (node-decl ptree)
          (ac (conj-pre ptree) env mut)
          (ac (conj-alt ptree) env mut)))
        ((disj? ptree)
         (new-disj
          (node-source ptree)
          (node-decl ptree)
          (ac (disj-pre ptree) env mut)
          (ac (disj-alt ptree) env mut)))
        ((prc? ptree) (ac-proc ptree env mut))
        ((app? ptree)
         (let ((oper (app-oper ptree)) (args (app-args ptree)))
           (if (and (prc? oper)
                    (not (prc-rest oper))
                    (SFX= (Slength (prc-parms oper)) (Slength args)))
               (ac-let ptree env mut)
               (new-call
                (node-source ptree)
                (node-decl ptree)
                (ac oper env mut)
                (Smap2 (lambda (x) (set-bbv-version-limit! #f)  (ac x env mut)) args)))))
        ((fut? ptree)
         (new-fut (node-source ptree)
                  (node-decl ptree)
                  (ac (fut-val ptree) env mut)))
        (else (compiler-internal-error "ac, unknown parse tree node type"))))
(define (ac-proc ptree env mut) (set-bbv-version-limit! #f) 
  (let* ((mut-parms (ac-mutables (prc-parms ptree)))
         (mut-parms-copies (Smap2 var-copy mut-parms))
         (mut (Sappend (pair-up mut-parms mut-parms-copies) mut))
         (new-body (ac (prc-body ptree) env mut)))
    (new-prc (node-source ptree)
             (node-decl ptree)
             (prc-name ptree)
             (prc-min ptree)
             (prc-rest ptree)
             (prc-parms ptree)
             (if (null? mut-parms)
                 new-body
                 (new-call
                  (node-source ptree)
                  (node-decl ptree)
                  (new-prc (node-source ptree)
                           (node-decl ptree)
                           #f
                           (Slength mut-parms-copies)
                           #f
                           mut-parms-copies
                           new-body)
                  (Smap2 (lambda (var) (set-bbv-version-limit! #f) 
                         (new-call
                          (var-source var)
                          (node-decl ptree)
                          (new-ref-extended-bindings
                           (var-source var)
                           **make-cell-sym
                           env)
                          (list (new-ref (var-source var)
                                         (node-decl ptree)
                                         var))))
                       mut-parms))))))
(define (ac-let ptree env mut) (set-bbv-version-limit! #f) 
  (let* ((proc (app-oper ptree))
         (vals (app-args ptree))
         (vars (prc-parms proc))
         (vals-fv (apply set-union (Smap2 free-variables vals)))
         (mut-parms (ac-mutables vars))
         (mut-parms-copies (Smap2 var-copy mut-parms))
         (mut (Sappend (pair-up mut-parms mut-parms-copies) mut)))
    (let loop ((l vars)
               (v vals)
               (new-vars '())
               (new-vals '())
               (new-body (ac (prc-body proc) env mut)))
      (if (null? l)
          (new-let ptree proc new-vars new-vals new-body)
          (let ((var (Scar l)) (val (Scar v)))
            (if (Smemq var mut-parms)
                (let ((src (node-source val))
                      (decl (node-decl val))
                      (var* (Scdr (Sassq var mut))))
                  (if (set-member? var vals-fv)
                      (loop (Scdr l)
                            (Scdr v)
                            (cons var* new-vars)
                            (cons (new-call
                                   src
                                   decl
                                   (new-ref-extended-bindings
                                    src
                                    **make-cell-sym
                                    env)
                                   (list (new-cst src decl undef-object)))
                                  new-vals)
                            (new-seq src
                                     decl
                                     (new-call
                                      src
                                      decl
                                      (new-ref-extended-bindings
                                       src
                                       **cell-set!-sym
                                       env)
                                      (list (new-ref src decl var*)
                                            (ac val env mut)))
                                     new-body))
                      (loop (Scdr l)
                            (Scdr v)
                            (cons var* new-vars)
                            (cons (new-call
                                   src
                                   decl
                                   (new-ref-extended-bindings
                                    src
                                    **make-cell-sym
                                    env)
                                   (list (ac val env mut)))
                                  new-vals)
                            new-body)))
                (loop (Scdr l)
                      (Scdr v)
                      (cons var new-vars)
                      (cons (ac val env mut) new-vals)
                      new-body)))))))
(define (ac-mutables l) (set-bbv-version-limit! #f) 
  (if (pair? l)
      (let ((var (Scar l)) (rest (ac-mutables (Scdr l))))
        (if (mutable? var) (cons var rest) rest))
      '()))
(define (lambda-lift! ptree) (set-bbv-version-limit! #f)  (ll! ptree (set-empty) '()))
(define (ll! ptree cst-procs env) (set-bbv-version-limit! #f) 
  (define (new-env env vars) (set-bbv-version-limit! #f) 
    (define (loop i l) (set-bbv-version-limit! #f) 
      (if (pair? l)
          (let ((var (Scar l)))
            (cons (cons var (cons (Slength (set->list (var-refs var))) i))
                  (loop (SFX+ i 1) (Scdr l))))
          env))
    (loop (Slength env) vars))
  (cond ((or (cst? ptree)
             (ref? ptree)
             (set? ptree)
             (tst? ptree)
             (conj? ptree)
             (disj? ptree)
             (fut? ptree))
         (for-each
          (lambda (child) (set-bbv-version-limit! #f)  (ll! child cst-procs env))
          (node-children ptree)))
        ((prc? ptree)
         (ll! (prc-body ptree) cst-procs (new-env env (prc-parms ptree))))
        ((app? ptree)
         (let ((oper (app-oper ptree)) (args (app-args ptree)))
           (if (and (prc? oper)
                    (not (prc-rest oper))
                    (SFX= (Slength (prc-parms oper)) (Slength args)))
               (ll!-let ptree cst-procs (new-env env (prc-parms oper)))
               (for-each
                (lambda (child) (set-bbv-version-limit! #f)  (ll! child cst-procs env))
                (node-children ptree)))))
        (else (compiler-internal-error "ll!, unknown parse tree node type"))))
(define (ll!-let ptree cst-procs env) (set-bbv-version-limit! #f) 
  (let* ((proc (app-oper ptree))
         (vals (app-args ptree))
         (vars (prc-parms proc))
         (var-val-map (pair-up vars vals)))
    (define (var->val var) (set-bbv-version-limit! #f)  (Scdr (Sassq var var-val-map)))
    (define (liftable-proc-vars vars) (set-bbv-version-limit! #f) 
      (let loop ((cst-proc-vars
                  (set-keep
                   (lambda (var) (set-bbv-version-limit! #f) 
                     (let ((val (var->val var)))
                       (and (prc? val)
                            (lambda-lift? (node-decl val))
                            (set-every? oper-pos? (var-refs var)))))
                   (list->set vars))))
        (let* ((non-cst-proc-vars
                (set-keep
                 (lambda (var) (set-bbv-version-limit! #f) 
                   (let ((val (var->val var)))
                     (and (prc? val) (not (set-member? var cst-proc-vars)))))
                 (list->set vars)))
               (cst-proc-vars*
                (set-keep
                 (lambda (var) (set-bbv-version-limit! #f) 
                   (let ((val (var->val var)))
                     (set-empty?
                      (set-intersection
                       (free-variables val)
                       non-cst-proc-vars))))
                 cst-proc-vars)))
          (if (set-equal? cst-proc-vars cst-proc-vars*)
              cst-proc-vars
              (loop cst-proc-vars*)))))
    (define (transitively-closed-free-variables vars) (set-bbv-version-limit! #f) 
      (let ((tcfv-map
             (Smap2 (lambda (var) (set-bbv-version-limit! #f)  (cons var (free-variables (var->val var))))
                  vars)))
        (let loop ((changed? #f))
          (for-each
           (lambda (var-tcfv) (set-bbv-version-limit! #f) 
             (let loop2 ((l (set->list (Scdr var-tcfv))) (fv (Scdr var-tcfv)))
               (if (null? l)
                   (if (not (set-equal? fv (Scdr var-tcfv)))
                       (begin (Sset-cdr! var-tcfv fv) (set! changed? #t)))
                   (let ((x (Sassq (Scar l) tcfv-map)))
                     (loop2 (Scdr l) (if x (set-union fv (Scdr x)) fv))))))
           tcfv-map)
          (if changed? (loop #f) tcfv-map))))
    (let* ((tcfv-map
            (transitively-closed-free-variables (liftable-proc-vars vars)))
           (cst-proc-vars-list (Smap2 (lambda (x) (set-bbv-version-limit! #f)  (Scar x)) tcfv-map))
           (cst-procs* (set-union (list->set cst-proc-vars-list) cst-procs)))
      (define (var->tcfv var) (set-bbv-version-limit! #f)  (Scdr (Sassq var tcfv-map)))
      (define (order-vars vars) (set-bbv-version-limit! #f) 
        (Smap2 (lambda (x) (set-bbv-version-limit! #f)  (Scar x))
             (sort-list
              (Smap2 (lambda (var) (set-bbv-version-limit! #f)  (Sassq var env)) vars)
              (lambda (x y) (set-bbv-version-limit! #f) 
                (if (SFX= (Scadr x) (Scadr y))
                    (SFX< (Scddr x) (Scddr y))
                    (SFX< (Scadr x) (Scadr y)))))))
      (define (lifted-vars var) (set-bbv-version-limit! #f) 
        (order-vars (set->list (set-difference (var->tcfv var) cst-procs*))))
      (define (lift-app! var) (set-bbv-version-limit! #f) 
        (let* ((val (var->val var)) (vars (lifted-vars var)))
          (define (new-ref* var) (set-bbv-version-limit! #f) 
            (new-ref (var-source var) (node-decl val) var))
          (if (not (null? vars))
              (for-each
               (lambda (oper) (set-bbv-version-limit! #f) 
                 (let ((node (node-parent oper)))
                   (node-children-set!
                    node
                    (cons (app-oper node)
                          (Sappend (Smap2 new-ref* vars) (app-args node))))))
               (set->list (var-refs var))))))
      (define (lift-prc! var) (set-bbv-version-limit! #f) 
        (let* ((val (var->val var)) (vars (lifted-vars var)))
          (if (not (null? vars))
              (let ((var-copies (Smap2 var-copy vars)))
                (prc-parms-set! val (Sappend var-copies (prc-parms val)))
                (for-each (lambda (x) (set-bbv-version-limit! #f)  (var-bound-set! x val)) var-copies)
                (node-fv-invalidate! val)
                (prc-min-set! val (SFX+ (prc-min val) (Slength vars)))
                (ll-rename! val (pair-up vars var-copies))))))
      (for-each lift-app! cst-proc-vars-list)
      (for-each lift-prc! cst-proc-vars-list)
      (for-each (lambda (node) (set-bbv-version-limit! #f)  (ll! node cst-procs* env)) vals)
      (ll! (prc-body proc) cst-procs* env))))
(define (ll-rename! ptree var-map) (set-bbv-version-limit! #f) 
  (cond ((ref? ptree)
         (let* ((var (ref-var ptree)) (x (Sassq var var-map)))
           (if x
               (begin
                 (var-refs-set! var (set-remove (var-refs var) ptree))
                 (var-refs-set! (Scdr x) (set-adjoin (var-refs (Scdr x)) ptree))
                 (ref-var-set! ptree (Scdr x))))))
        ((set? ptree)
         (let* ((var (set-var ptree)) (x (Sassq var var-map)))
           (if x
               (begin
                 (var-sets-set! var (set-remove (var-sets var) ptree))
                 (var-sets-set! (Scdr x) (set-adjoin (var-sets (Scdr x)) ptree))
                 (set-var-set! ptree (Scdr x)))))))
  (node-fv-set! ptree #t)
  (for-each (lambda (child) (set-bbv-version-limit! #f)  (ll-rename! child var-map)) (node-children ptree)))
(define (parse-tree->expression ptree) (set-bbv-version-limit! #f)  (se ptree '() (list 0)))
(define (se ptree env num) (set-bbv-version-limit! #f) 
  (cond ((cst? ptree) (list quote-sym (cst-val ptree)))
        ((ref? ptree)
         (let ((x (Sassq (ref-var ptree) env)))
           (if x (Scdr x) (var-name (ref-var ptree)))))
        ((set? ptree)
         (list set!-sym
               (let ((x (Sassq (set-var ptree) env)))
                 (if x (Scdr x) (var-name (set-var ptree))))
               (se (set-val ptree) env num)))
        ((def? ptree)
         (list define-sym
               (let ((x (Sassq (def-var ptree) env)))
                 (if x (Scdr x) (var-name (def-var ptree))))
               (se (def-val ptree) env num)))
        ((tst? ptree)
         (list if-sym
               (se (tst-pre ptree) env num)
               (se (tst-con ptree) env num)
               (se (tst-alt ptree) env num)))
        ((conj? ptree)
         (list and-sym
               (se (conj-pre ptree) env num)
               (se (conj-alt ptree) env num)))
        ((disj? ptree)
         (list or-sym
               (se (disj-pre ptree) env num)
               (se (disj-alt ptree) env num)))
        ((prc? ptree)
         (let ((new-env (se-rename (prc-parms ptree) env num)))
           (list lambda-sym
                 (se-parameters
                  (prc-parms ptree)
                  (prc-rest ptree)
                  (prc-min ptree)
                  new-env)
                 (se (prc-body ptree) new-env num))))
        ((app? ptree)
         (let ((oper (app-oper ptree)) (args (app-args ptree)))
           (if (and (prc? oper)
                    (not (prc-rest oper))
                    (SFX= (Slength (prc-parms oper)) (Slength args)))
               (let ((new-env (se-rename (prc-parms oper) env num)))
                 (list (if (set-empty?
                            (set-intersection
                             (list->set (prc-parms oper))
                             (apply set-union (Smap2 free-variables args))))
                           let-sym
                           letrec-sym)
                       (se-bindings (prc-parms oper) args new-env num)
                       (se (prc-body oper) new-env num)))
               (Smap2 (lambda (x) (set-bbv-version-limit! #f)  (se x env num)) (cons oper args)))))
        ((fut? ptree) (list future-sym (se (fut-val ptree) env num)))
        (else (compiler-internal-error "se, unknown parse tree node type"))))
(define (se-parameters parms rest min env) (set-bbv-version-limit! #f) 
  (define (se-parms parms rest n env) (set-bbv-version-limit! #f) 
    (cond ((null? parms) '())
          ((and rest (null? (Scdr parms))) (Scdr (Sassq (Scar parms) env)))
          (else
           (let ((parm (Scdr (Sassq (Scar parms) env))))
             (cons (if (SFX> n 0) parm (list parm))
                   (se-parms (Scdr parms) rest (SFX- n 1) env))))))
  (se-parms parms rest min env))
(define (se-bindings vars vals env num) (set-bbv-version-limit! #f) 
  (if (null? vars)
      '()
      (cons (list (Scdr (Sassq (Scar vars) env)) (se (Scar vals) env num))
            (se-bindings (Scdr vars) (Scdr vals) env num))))
(define (se-rename vars env num) (set-bbv-version-limit! #f) 
  (define (rename vars) (set-bbv-version-limit! #f) 
    (if (null? vars)
        env
        (cons (cons (Scar vars)
                    (string->canonical-symbol
                     (Sstring-append
                      (Ssymbol->string (var-name (Scar vars)))
                      "#"
                      (SFXnumber->string (Scar num)))))
              (rename (Scdr vars)))))
  (Sset-car! num (SFX+ (Scar num) 1))
  (rename vars))
(define *opnd-table* '())
(define *opnd-table-alloc* '())
(define opnd-table-size 10000)
(define (enter-opnd arg1 arg2) (set-bbv-version-limit! #f) 
  (let loop ((i 0))
    (if (SFX< i *opnd-table-alloc*)
        (let ((x (Svector-ref *opnd-table* i)))
          (if (and (eqv? (Scar x) arg1) (eqv? (Scdr x) arg2)) i (loop (SFX+ i 1))))
        (if (SFX< *opnd-table-alloc* opnd-table-size)
            (begin
              (set! *opnd-table-alloc* (SFX+ *opnd-table-alloc* 1))
              (Svector-set! *opnd-table* i (cons arg1 arg2))
              i)
            (compiler-limitation-error
             "program is too long [virtual machine operand table overflow]")))))
(define (contains-opnd? opnd1 opnd2) (set-bbv-version-limit! #f) 
  (cond ((eqv? opnd1 opnd2) #t)
        ((clo? opnd2) (contains-opnd? opnd1 (clo-base opnd2)))
        (else #f)))
(define (any-contains-opnd? opnd opnds) (set-bbv-version-limit! #f) 
  (if (null? opnds)
      #f
      (or (contains-opnd? opnd (Scar opnds))
          (any-contains-opnd? opnd (Scdr opnds)))))
(define (make-reg num) (set-bbv-version-limit! #f)  num)
(define (reg? x) (set-bbv-version-limit! #f)  (SFX< x 10000))
(define (reg-num x) (set-bbv-version-limit! #f)  (SFXmodulo x 10000))
(define (make-stk num) (set-bbv-version-limit! #f)  (SFX+ num 10000))
(define (stk? x) (set-bbv-version-limit! #f)  (SFX= (SFXquotient x 10000) 1))
(define (stk-num x) (set-bbv-version-limit! #f)  (SFXmodulo x 10000))
(define (make-glo name) (set-bbv-version-limit! #f)  (SFX+ (enter-opnd name #t) 30000))
(define (glo? x) (set-bbv-version-limit! #f)  (SFX= (SFXquotient x 10000) 3))
(define (glo-name x) (set-bbv-version-limit! #f)  (Scar (Svector-ref *opnd-table* (SFXmodulo x 10000))))
(define (make-clo base index) (set-bbv-version-limit! #f)  (SFX+ (enter-opnd base index) 40000))
(define (clo? x) (set-bbv-version-limit! #f)  (SFX= (SFXquotient x 10000) 4))
(define (clo-base x) (set-bbv-version-limit! #f)  (Scar (Svector-ref *opnd-table* (SFXmodulo x 10000))))
(define (clo-index x) (set-bbv-version-limit! #f)  (Scdr (Svector-ref *opnd-table* (SFXmodulo x 10000))))
(define (make-lbl num) (set-bbv-version-limit! #f)  (SFX+ num 20000))
(define (lbl? x) (set-bbv-version-limit! #f)  (SFX= (SFXquotient x 10000) 2))
(define (lbl-num x) (set-bbv-version-limit! #f)  (SFXmodulo x 10000))
(define label-limit 9999)
(define (make-obj val) (set-bbv-version-limit! #f)  (SFX+ (enter-opnd val #f) 50000))
(define (obj? x) (set-bbv-version-limit! #f)  (SFX= (SFXquotient x 10000) 5))
(define (obj-val x) (set-bbv-version-limit! #f)  (Scar (Svector-ref *opnd-table* (SFXmodulo x 10000))))
(define (make-pcontext fs map) (set-bbv-version-limit! #f)  (vector fs map))
(define (pcontext-fs x) (set-bbv-version-limit! #f)  (Svector-ref x 0))
(define (pcontext-map x) (set-bbv-version-limit! #f)  (Svector-ref x 1))
(define (make-frame size slots regs closed live) (set-bbv-version-limit! #f) 
  (vector size slots regs closed live))
(define (frame-size x) (set-bbv-version-limit! #f)  (Svector-ref x 0))
(define (frame-slots x) (set-bbv-version-limit! #f)  (Svector-ref x 1))
(define (frame-regs x) (set-bbv-version-limit! #f)  (Svector-ref x 2))
(define (frame-closed x) (set-bbv-version-limit! #f)  (Svector-ref x 3))
(define (frame-live x) (set-bbv-version-limit! #f)  (Svector-ref x 4))
(define (frame-eq? x y) (set-bbv-version-limit! #f)  (SFX= (frame-size x) (frame-size y)))
(define (frame-truncate frame nb-slots) (set-bbv-version-limit! #f) 
  (let ((fs (frame-size frame)))
    (make-frame
     nb-slots
     (nth-after (frame-slots frame) (SFX- fs nb-slots))
     (frame-regs frame)
     (frame-closed frame)
     (frame-live frame))))
(define (frame-live? var frame) (set-bbv-version-limit! #f) 
  (let ((live (frame-live frame)))
    (if (eq? var closure-env-var)
        (let ((closed (frame-closed frame)))
          (if (or (set-member? var live)
                  (not (set-empty?
                        (set-intersection live (list->set closed)))))
              closed
              #f))
        (if (set-member? var live) var #f))))
(define (frame-first-empty-slot frame) (set-bbv-version-limit! #f) 
  (let loop ((i 1) (s (Sreverse (frame-slots frame))))
    (if (pair? s)
        (if (frame-live? (Scar s) frame) (loop (SFX+ i 1) (Scdr s)) i)
        i)))
(define (make-proc-obj
         name
         primitive?
         code
         call-pat
         side-effects?
         strict-pat
         type)
  (let ((proc-obj
         (vector proc-obj-tag
                 name
                 primitive?
                 code
                 call-pat
                 #f
                 #f
                 #f
                 side-effects?
                 strict-pat
                 type)))
    (proc-obj-specialize-set! proc-obj (lambda (decls) (set-bbv-version-limit! #f)  proc-obj))
    proc-obj))
(define proc-obj-tag (list 'proc-obj))
(define (proc-obj? x) (set-bbv-version-limit! #f) 
  (and (vector? x)
       (SFX> (Svector-length x) 0)
       (eq? (Svector-ref x 0) proc-obj-tag)))
(define (proc-obj-name obj) (set-bbv-version-limit! #f)  (Svector-ref obj 1))
(define (proc-obj-primitive? obj) (set-bbv-version-limit! #f)  (Svector-ref obj 2))
(define (proc-obj-code obj) (set-bbv-version-limit! #f)  (Svector-ref obj 3))
(define (proc-obj-call-pat obj) (set-bbv-version-limit! #f)  (Svector-ref obj 4))
(define (proc-obj-test obj) (set-bbv-version-limit! #f)  (Svector-ref obj 5))
(define (proc-obj-inlinable obj) (set-bbv-version-limit! #f)  (Svector-ref obj 6))
(define (proc-obj-specialize obj) (set-bbv-version-limit! #f)  (Svector-ref obj 7))
(define (proc-obj-side-effects? obj) (set-bbv-version-limit! #f)  (Svector-ref obj 8))
(define (proc-obj-strict-pat obj) (set-bbv-version-limit! #f)  (Svector-ref obj 9))
(define (proc-obj-type obj) (set-bbv-version-limit! #f)  (Svector-ref obj 10))
(define (proc-obj-code-set! obj x) (set-bbv-version-limit! #f)  (Svector-set! obj 3 x))
(define (proc-obj-test-set! obj x) (set-bbv-version-limit! #f)  (Svector-set! obj 5 x))
(define (proc-obj-inlinable-set! obj x) (set-bbv-version-limit! #f)  (Svector-set! obj 6 x))
(define (proc-obj-specialize-set! obj x) (set-bbv-version-limit! #f)  (Svector-set! obj 7 x))
(define (make-pattern min-args nb-parms rest?) (set-bbv-version-limit! #f) 
  (let loop ((x (if rest? (SFX- nb-parms 1) (list nb-parms)))
             (y (if rest? (SFX- nb-parms 1) nb-parms)))
    (let ((z (SFX- y 1))) (if (SFX< z min-args) x (loop (cons z x) z)))))
(define (pattern-member? n pat) (set-bbv-version-limit! #f) 
  (cond ((pair? pat) (if (SFX= (Scar pat) n) #t (pattern-member? n (Scdr pat))))
        ((null? pat) #f)
        (else (SFX<= pat n))))
(define (type-name type) (set-bbv-version-limit! #f)  (if (pair? type) (Scar type) type))
(define (type-pot-fut? type) (set-bbv-version-limit! #f)  (pair? type))
(define (make-bbs) (set-bbv-version-limit! #f) 
  (vector (make-counter 1 label-limit bbs-limit-err) (queue-empty) '()))
(define (bbs-limit-err) (set-bbv-version-limit! #f) 
  (compiler-limitation-error "procedure is too long [too many labels]"))
(define (bbs-lbl-counter bbs) (set-bbv-version-limit! #f)  (Svector-ref bbs 0))
(define (bbs-lbl-counter-set! bbs cntr) (set-bbv-version-limit! #f)  (Svector-set! bbs 0 cntr))
(define (bbs-bb-queue bbs) (set-bbv-version-limit! #f)  (Svector-ref bbs 1))
(define (bbs-bb-queue-set! bbs bbq) (set-bbv-version-limit! #f)  (Svector-set! bbs 1 bbq))
(define (bbs-entry-lbl-num bbs) (set-bbv-version-limit! #f)  (Svector-ref bbs 2))
(define (bbs-entry-lbl-num-set! bbs lbl-num) (set-bbv-version-limit! #f)  (Svector-set! bbs 2 lbl-num))
(define (bbs-new-lbl! bbs) (set-bbv-version-limit! #f)  ((bbs-lbl-counter bbs)))
(define (lbl-num->bb lbl-num bbs) (set-bbv-version-limit! #f) 
  (let loop ((bb-list (queue->list (bbs-bb-queue bbs))))
    (if (SFX= (bb-lbl-num (Scar bb-list)) lbl-num)
        (Scar bb-list)
        (loop (Scdr bb-list)))))
(define (make-bb label-instr bbs) (set-bbv-version-limit! #f) 
  (let ((bb (vector label-instr (queue-empty) '() '() '())))
    (queue-put! (Svector-ref bbs 1) bb)
    bb))
(define (bb-lbl-num bb) (set-bbv-version-limit! #f)  (label-lbl-num (Svector-ref bb 0)))
(define (bb-label-type bb) (set-bbv-version-limit! #f)  (label-type (Svector-ref bb 0)))
(define (bb-label-instr bb) (set-bbv-version-limit! #f)  (Svector-ref bb 0))
(define (bb-label-instr-set! bb l) (set-bbv-version-limit! #f)  (Svector-set! bb 0 l))
(define (bb-non-branch-instrs bb) (set-bbv-version-limit! #f)  (queue->list (Svector-ref bb 1)))
(define (bb-non-branch-instrs-set! bb l) (set-bbv-version-limit! #f)  (Svector-set! bb 1 (list->queue l)))
(define (bb-branch-instr bb) (set-bbv-version-limit! #f)  (Svector-ref bb 2))
(define (bb-branch-instr-set! bb b) (set-bbv-version-limit! #f)  (Svector-set! bb 2 b))
(define (bb-references bb) (set-bbv-version-limit! #f)  (Svector-ref bb 3))
(define (bb-references-set! bb l) (set-bbv-version-limit! #f)  (Svector-set! bb 3 l))
(define (bb-precedents bb) (set-bbv-version-limit! #f)  (Svector-ref bb 4))
(define (bb-precedents-set! bb l) (set-bbv-version-limit! #f)  (Svector-set! bb 4 l))
(define (bb-entry-frame-size bb) (set-bbv-version-limit! #f) 
  (frame-size (gvm-instr-frame (bb-label-instr bb))))
(define (bb-exit-frame-size bb) (set-bbv-version-limit! #f) 
  (frame-size (gvm-instr-frame (bb-branch-instr bb))))
(define (bb-slots-gained bb) (set-bbv-version-limit! #f) 
  (SFX- (bb-exit-frame-size bb) (bb-entry-frame-size bb)))
(define (bb-put-non-branch! bb gvm-instr) (set-bbv-version-limit! #f) 
  (queue-put! (Svector-ref bb 1) gvm-instr))
(define (bb-put-branch! bb gvm-instr) (set-bbv-version-limit! #f)  (Svector-set! bb 2 gvm-instr))
(define (bb-add-reference! bb ref) (set-bbv-version-limit! #f) 
  (if (not (Smemq ref (Svector-ref bb 3)))
      (Svector-set! bb 3 (cons ref (Svector-ref bb 3)))))
(define (bb-add-precedent! bb prec) (set-bbv-version-limit! #f) 
  (if (not (Smemq prec (Svector-ref bb 4)))
      (Svector-set! bb 4 (cons prec (Svector-ref bb 4)))))
(define (bb-last-non-branch-instr bb) (set-bbv-version-limit! #f) 
  (let ((non-branch-instrs (bb-non-branch-instrs bb)))
    (if (null? non-branch-instrs)
        (bb-label-instr bb)
        (let loop ((l non-branch-instrs))
          (if (pair? (Scdr l)) (loop (Scdr l)) (Scar l))))))
(define (gvm-instr-type gvm-instr) (set-bbv-version-limit! #f)  (Svector-ref gvm-instr 0))
(define (gvm-instr-frame gvm-instr) (set-bbv-version-limit! #f)  (Svector-ref gvm-instr 1))
(define (gvm-instr-comment gvm-instr) (set-bbv-version-limit! #f)  (Svector-ref gvm-instr 2))
(define (make-label-simple lbl-num frame comment) (set-bbv-version-limit! #f) 
  (vector 'label frame comment lbl-num 'simple))
(define (make-label-entry lbl-num nb-parms min rest? closed? frame comment) (set-bbv-version-limit! #f) 
  (vector 'label frame comment lbl-num 'entry nb-parms min rest? closed?))
(define (make-label-return lbl-num frame comment) (set-bbv-version-limit! #f) 
  (vector 'label frame comment lbl-num 'return))
(define (make-label-task-entry lbl-num frame comment) (set-bbv-version-limit! #f) 
  (vector 'label frame comment lbl-num 'task-entry))
(define (make-label-task-return lbl-num frame comment) (set-bbv-version-limit! #f) 
  (vector 'label frame comment lbl-num 'task-return))
(define (label-lbl-num gvm-instr) (set-bbv-version-limit! #f)  (Svector-ref gvm-instr 3))
(define (label-lbl-num-set! gvm-instr n) (set-bbv-version-limit! #f)  (Svector-set! gvm-instr 3 n))
(define (label-type gvm-instr) (set-bbv-version-limit! #f)  (Svector-ref gvm-instr 4))
(define (label-entry-nb-parms gvm-instr) (set-bbv-version-limit! #f)  (Svector-ref gvm-instr 5))
(define (label-entry-min gvm-instr) (set-bbv-version-limit! #f)  (Svector-ref gvm-instr 6))
(define (label-entry-rest? gvm-instr) (set-bbv-version-limit! #f)  (Svector-ref gvm-instr 7))
(define (label-entry-closed? gvm-instr) (set-bbv-version-limit! #f)  (Svector-ref gvm-instr 8))
(define (make-apply prim opnds loc frame comment) (set-bbv-version-limit! #f) 
  (vector 'apply frame comment prim opnds loc))
(define (apply-prim gvm-instr) (set-bbv-version-limit! #f)  (Svector-ref gvm-instr 3))
(define (apply-opnds gvm-instr) (set-bbv-version-limit! #f)  (Svector-ref gvm-instr 4))
(define (apply-loc gvm-instr) (set-bbv-version-limit! #f)  (Svector-ref gvm-instr 5))
(define (make-copy opnd loc frame comment) (set-bbv-version-limit! #f) 
  (vector 'copy frame comment opnd loc))
(define (copy-opnd gvm-instr) (set-bbv-version-limit! #f)  (Svector-ref gvm-instr 3))
(define (copy-loc gvm-instr) (set-bbv-version-limit! #f)  (Svector-ref gvm-instr 4))
(define (make-close parms frame comment) (set-bbv-version-limit! #f)  (vector 'close frame comment parms))
(define (close-parms gvm-instr) (set-bbv-version-limit! #f)  (Svector-ref gvm-instr 3))
(define (make-closure-parms loc lbl opnds) (set-bbv-version-limit! #f)  (vector loc lbl opnds))
(define (closure-parms-loc x) (set-bbv-version-limit! #f)  (Svector-ref x 0))
(define (closure-parms-lbl x) (set-bbv-version-limit! #f)  (Svector-ref x 1))
(define (closure-parms-opnds x) (set-bbv-version-limit! #f)  (Svector-ref x 2))
(define (make-ifjump test opnds true false poll? frame comment) (set-bbv-version-limit! #f) 
  (vector 'ifjump frame comment test opnds true false poll?))
(define (ifjump-test gvm-instr) (set-bbv-version-limit! #f)  (Svector-ref gvm-instr 3))
(define (ifjump-opnds gvm-instr) (set-bbv-version-limit! #f)  (Svector-ref gvm-instr 4))
(define (ifjump-true gvm-instr) (set-bbv-version-limit! #f)  (Svector-ref gvm-instr 5))
(define (ifjump-false gvm-instr) (set-bbv-version-limit! #f)  (Svector-ref gvm-instr 6))
(define (ifjump-poll? gvm-instr) (set-bbv-version-limit! #f)  (Svector-ref gvm-instr 7))
(define (make-jump opnd nb-args poll? frame comment) (set-bbv-version-limit! #f) 
  (vector 'jump frame comment opnd nb-args poll?))
(define (jump-opnd gvm-instr) (set-bbv-version-limit! #f)  (Svector-ref gvm-instr 3))
(define (jump-nb-args gvm-instr) (set-bbv-version-limit! #f)  (Svector-ref gvm-instr 4))
(define (jump-poll? gvm-instr) (set-bbv-version-limit! #f)  (Svector-ref gvm-instr 5))
(define (first-class-jump? gvm-instr) (set-bbv-version-limit! #f)  (jump-nb-args gvm-instr))
(define (make-comment) (set-bbv-version-limit! #f)  (cons 'comment '()))
(define (comment-put! comment name val) (set-bbv-version-limit! #f) 
  (Sset-cdr! comment (cons (cons name val) (Scdr comment))))
(define (comment-get comment name) (set-bbv-version-limit! #f) 
  (and comment (let ((x (Sassq name (Scdr comment)))) (if x (Scdr x) #f))))
(define (bbs-purify! bbs) (set-bbv-version-limit! #f) 
  (let loop ()
    (bbs-remove-jump-cascades! bbs)
    (bbs-remove-dead-code! bbs)
    (let* ((changed1? (bbs-remove-common-code! bbs))
           (changed2? (bbs-remove-useless-jumps! bbs)))
      (if (or changed1? changed2?) (loop) (bbs-order! bbs)))))
(define (bbs-remove-jump-cascades! bbs) (set-bbv-version-limit! #f) 
  (define (empty-bb? bb) (set-bbv-version-limit! #f) 
    (and (eq? (bb-label-type bb) 'simple) (null? (bb-non-branch-instrs bb))))
  (define (jump-to-non-entry-lbl? branch) (set-bbv-version-limit! #f) 
    (and (eq? (gvm-instr-type branch) 'jump)
         (not (first-class-jump? branch))
         (jump-lbl? branch)))
  (define (jump-cascade-to lbl-num fs poll? seen thunk) (set-bbv-version-limit! #f) 
    (if (Smemq lbl-num seen)
        (thunk lbl-num fs poll?)
        (let ((bb (lbl-num->bb lbl-num bbs)))
          (if (and (empty-bb? bb) (SFX<= (bb-slots-gained bb) 0))
              (let ((jump-lbl-num
                     (jump-to-non-entry-lbl? (bb-branch-instr bb))))
                (if jump-lbl-num
                    (jump-cascade-to
                     jump-lbl-num
                     (SFX+ fs (bb-slots-gained bb))
                     (or poll? (jump-poll? (bb-branch-instr bb)))
                     (cons lbl-num seen)
                     thunk)
                    (thunk lbl-num fs poll?)))
              (thunk lbl-num fs poll?)))))
  (define (equiv-lbl lbl-num seen) (set-bbv-version-limit! #f) 
    (if (Smemq lbl-num seen)
        lbl-num
        (let ((bb (lbl-num->bb lbl-num bbs)))
          (if (empty-bb? bb)
              (let ((jump-lbl-num
                     (jump-to-non-entry-lbl? (bb-branch-instr bb))))
                (if (and jump-lbl-num
                         (not (jump-poll? (bb-branch-instr bb)))
                         (SFX= (bb-slots-gained bb) 0))
                    (equiv-lbl jump-lbl-num (cons lbl-num seen))
                    lbl-num))
              lbl-num))))
  (define (remove-cascade! bb) (set-bbv-version-limit! #f) 
    (let ((branch (bb-branch-instr bb)))
      (case (gvm-instr-type branch)
        ((ifjump)
         (bb-put-branch!
          bb
          (make-ifjump
           (ifjump-test branch)
           (ifjump-opnds branch)
           (equiv-lbl (ifjump-true branch) '())
           (equiv-lbl (ifjump-false branch) '())
           (ifjump-poll? branch)
           (gvm-instr-frame branch)
           (gvm-instr-comment branch))))
        ((jump)
         (if (not (first-class-jump? branch))
             (let ((dest-lbl-num (jump-lbl? branch)))
               (if dest-lbl-num
                   (jump-cascade-to
                    dest-lbl-num
                    (frame-size (gvm-instr-frame branch))
                    (jump-poll? branch)
                    '()
                    (lambda (lbl-num fs poll?) (set-bbv-version-limit! #f) 
                      (let* ((dest-bb (lbl-num->bb lbl-num bbs))
                             (last-branch (bb-branch-instr dest-bb)))
                        (if (and (empty-bb? dest-bb)
                                 (or (not poll?)
                                     put-poll-on-ifjump?
                                     (not (eq? (gvm-instr-type last-branch)
                                               'ifjump))))
                            (let* ((new-fs (SFX+ fs (bb-slots-gained dest-bb)))
                                   (new-frame
                                    (frame-truncate
                                     (gvm-instr-frame branch)
                                     new-fs)))
                              (define (adjust-opnd opnd) (set-bbv-version-limit! #f) 
                                (cond ((stk? opnd)
                                       (make-stk
                                        (SFX+ (SFX- fs (bb-entry-frame-size dest-bb))
                                           (stk-num opnd))))
                                      ((clo? opnd)
                                       (make-clo
                                        (adjust-opnd (clo-base opnd))
                                        (clo-index opnd)))
                                      (else opnd)))
                              (case (gvm-instr-type last-branch)
                                ((ifjump)
                                 (bb-put-branch!
                                  bb
                                  (make-ifjump
                                   (ifjump-test last-branch)
                                   (Smap2 adjust-opnd (ifjump-opnds last-branch))
                                   (equiv-lbl (ifjump-true last-branch) '())
                                   (equiv-lbl (ifjump-false last-branch) '())
                                   (or poll? (ifjump-poll? last-branch))
                                   new-frame
                                   (gvm-instr-comment last-branch))))
                                ((jump)
                                 (bb-put-branch!
                                  bb
                                  (make-jump
                                   (adjust-opnd (jump-opnd last-branch))
                                   (jump-nb-args last-branch)
                                   (or poll? (jump-poll? last-branch))
                                   new-frame
                                   (gvm-instr-comment last-branch))))
                                (else
                                 (compiler-internal-error
                                  "bbs-remove-jump-cascades!, unknown branch type"))))
                            (bb-put-branch!
                             bb
                             (make-jump
                              (make-lbl lbl-num)
                              (jump-nb-args branch)
                              (or poll? (jump-poll? branch))
                              (frame-truncate (gvm-instr-frame branch) fs)
                              (gvm-instr-comment branch)))))))))))
        (else
         (compiler-internal-error
          "bbs-remove-jump-cascades!, unknown branch type")))))
  (for-each remove-cascade! (queue->list (bbs-bb-queue bbs))))
(define (jump-lbl? branch) (set-bbv-version-limit! #f) 
  (let ((opnd (jump-opnd branch))) (if (lbl? opnd) (lbl-num opnd) #f)))
(define put-poll-on-ifjump? #f)
(set! put-poll-on-ifjump? #t)
(define (bbs-remove-dead-code! bbs) (set-bbv-version-limit! #f) 
  (let ((new-bb-queue (queue-empty)) (scan-queue (queue-empty)))
    (define (reachable ref bb) (set-bbv-version-limit! #f) 
      (if bb (bb-add-reference! bb ref))
      (if (not (Smemq ref (queue->list new-bb-queue)))
          (begin
            (bb-references-set! ref '())
            (bb-precedents-set! ref '())
            (queue-put! new-bb-queue ref)
            (queue-put! scan-queue ref))))
    (define (direct-jump to-bb from-bb) (set-bbv-version-limit! #f) 
      (reachable to-bb from-bb)
      (bb-add-precedent! to-bb from-bb))
    (define (scan-instr gvm-instr bb) (set-bbv-version-limit! #f) 
      (define (scan-opnd gvm-opnd) (set-bbv-version-limit! #f) 
        (cond ((lbl? gvm-opnd)
               (reachable (lbl-num->bb (lbl-num gvm-opnd) bbs) bb))
              ((clo? gvm-opnd) (scan-opnd (clo-base gvm-opnd)))))
      (case (gvm-instr-type gvm-instr)
        ((label) '())
        ((apply)
         (for-each scan-opnd (apply-opnds gvm-instr))
         (if (apply-loc gvm-instr) (scan-opnd (apply-loc gvm-instr))))
        ((copy)
         (scan-opnd (copy-opnd gvm-instr))
         (scan-opnd (copy-loc gvm-instr)))
        ((close)
         (for-each
          (lambda (parm) (set-bbv-version-limit! #f) 
            (reachable (lbl-num->bb (closure-parms-lbl parm) bbs) bb)
            (scan-opnd (closure-parms-loc parm))
            (for-each scan-opnd (closure-parms-opnds parm)))
          (close-parms gvm-instr)))
        ((ifjump)
         (for-each scan-opnd (ifjump-opnds gvm-instr))
         (direct-jump (lbl-num->bb (ifjump-true gvm-instr) bbs) bb)
         (direct-jump (lbl-num->bb (ifjump-false gvm-instr) bbs) bb))
        ((jump)
         (let ((opnd (jump-opnd gvm-instr)))
           (if (lbl? opnd)
               (direct-jump (lbl-num->bb (lbl-num opnd) bbs) bb)
               (scan-opnd (jump-opnd gvm-instr)))))
        (else
         (compiler-internal-error
          "bbs-remove-dead-code!, unknown GVM instruction type"))))
    (reachable (lbl-num->bb (bbs-entry-lbl-num bbs) bbs) #f)
    (let loop ()
      (if (not (queue-empty? scan-queue))
          (let ((bb (queue-get! scan-queue)))
            (begin
              (scan-instr (bb-label-instr bb) bb)
              (for-each
               (lambda (gvm-instr) (set-bbv-version-limit! #f)  (scan-instr gvm-instr bb))
               (bb-non-branch-instrs bb))
              (scan-instr (bb-branch-instr bb) bb)
              (loop)))))
    (bbs-bb-queue-set! bbs new-bb-queue)))
(define (bbs-remove-useless-jumps! bbs) (set-bbv-version-limit! #f) 
  (let ((changed? #f))
    (define (remove-useless-jump bb) (set-bbv-version-limit! #f) 
      (let ((branch (bb-branch-instr bb)))
        (if (and (eq? (gvm-instr-type branch) 'jump)
                 (not (first-class-jump? branch))
                 (not (jump-poll? branch))
                 (jump-lbl? branch))
            (let* ((dest-bb (lbl-num->bb (jump-lbl? branch) bbs))
                   (frame1 (gvm-instr-frame (bb-last-non-branch-instr bb)))
                   (frame2 (gvm-instr-frame (bb-label-instr dest-bb))))
              (if (and (eq? (bb-label-type dest-bb) 'simple)
                       (frame-eq? frame1 frame2)
                       (SFX= (Slength (bb-precedents dest-bb)) 1))
                  (begin
                    (set! changed? #t)
                    (bb-non-branch-instrs-set!
                     bb
                     (Sappend (bb-non-branch-instrs bb)
                             (Sappend (bb-non-branch-instrs dest-bb)
                             '())))
                    (bb-branch-instr-set! bb (bb-branch-instr dest-bb))
                    (remove-useless-jump bb)))))))
    (for-each remove-useless-jump (queue->list (bbs-bb-queue bbs)))
    changed?))
(define (bbs-remove-common-code! bbs) (set-bbv-version-limit! #f) 
  (let* ((bb-list (queue->list (bbs-bb-queue bbs)))
         (n (Slength bb-list))
         (hash-table-length (cond ((SFX< n 50) 43) ((SFX< n 500) 403) (else 4003)))
         (hash-table (Smake-vector2 hash-table-length '()))
         (prim-table '())
         (block-map '())
         (changed? #f))
    (define (hash-prim prim) (set-bbv-version-limit! #f) 
      (let ((n (Slength prim-table)) (i (pos-in-list prim prim-table)))
        (if i
            (SFX- n i)
            (begin (set! prim-table (cons prim prim-table)) (SFX+ n 1)))))
    (define (hash-opnds l) (set-bbv-version-limit! #f) 
      (let loop ((l l) (n 0))
        (if (pair? l)
            (loop (Scdr l)
                  (let ((x (Scar l)))
                    (if (lbl? x)
                        n
                        (SFXmodulo (SFX+ (SFX* n 10000) x) hash-table-length))))
            n)))
    (define (hash-bb bb) (set-bbv-version-limit! #f) 
      (let ((branch (bb-branch-instr bb)))
        (SFXmodulo (case (gvm-instr-type branch)
                  ((ifjump)
                   (SFX+ (hash-opnds (ifjump-opnds branch))
                      (SFX+ (SFX* 10 (hash-prim (ifjump-test branch)))
                            (SFX* 100 (frame-size (gvm-instr-frame branch))))))
                  ((jump)
                   (SFX+ (hash-opnds (list (jump-opnd branch)))
                      (SFX+ (SFX* 10 (or (jump-nb-args branch) -1))
                            (SFX* 100 (frame-size (gvm-instr-frame branch))))))
                  (else 0))
                hash-table-length)))
    (define (replacement-lbl-num lbl) (set-bbv-version-limit! #f) 
      (let ((x (Sassv lbl block-map))) (if x (Scdr x) lbl)))
    (define (fix-map! bb1 bb2) (set-bbv-version-limit! #f) 
      (let loop ((l block-map))
        (if (pair? l)
            (let ((x (Scar l)))
              (if (SFX= bb1 (Scdr x)) (Sset-cdr! x bb2))
              (loop (Scdr l))))))
    (define (enter-bb! bb) (set-bbv-version-limit! #f) 
      (let ((h (hash-bb bb)))
        (Svector-set! hash-table h (add-bb bb (Svector-ref hash-table h)))))
    (define (add-bb bb l) (set-bbv-version-limit! #f) 
      (if (pair? l)
          (let ((bb* (Scar l)))
            (set! block-map
                  (cons (cons (bb-lbl-num bb) (bb-lbl-num bb*)) block-map))
            (if (eqv-bb? bb bb*)
                (begin
                  (fix-map! (bb-lbl-num bb) (bb-lbl-num bb*))
                  (set! changed? #t)
                  l)
                (begin
                  (set! block-map (Scdr block-map))
                  (if (eqv-gvm-instr?
                       (bb-branch-instr bb)
                       (bb-branch-instr bb*))
                      (extract-common-tail
                       bb
                       bb*
                       (lambda (head head* tail) (set-bbv-version-limit! #f) 
                         (if (null? tail)
                             (cons bb* (add-bb bb (Scdr l)))
                             (let* ((lbl (bbs-new-lbl! bbs))
                                    (branch (bb-branch-instr bb))
                                    (fs** (need-gvm-instrs tail branch))
                                    (frame (frame-truncate
                                            (gvm-instr-frame
                                             (if (null? head)
                                                 (bb-label-instr bb)
                                                 (Scar head)))
                                            fs**))
                                    (bb** (make-bb (make-label-simple
                                                    lbl
                                                    frame
                                                    #f)
                                                   bbs)))
                               (bb-non-branch-instrs-set! bb** tail)
                               (bb-branch-instr-set! bb** branch)
                               (bb-non-branch-instrs-set! bb* (Sreverse head*))
                               (bb-branch-instr-set!
                                bb*
                                (make-jump (make-lbl lbl) #f #f frame #f))
                               (bb-non-branch-instrs-set! bb (Sreverse head))
                               (bb-branch-instr-set!
                                bb
                                (make-jump (make-lbl lbl) #f #f frame #f))
                               (set! changed? #t)
                               (cons bb (cons bb* (add-bb bb** (Scdr l))))))))
                      (cons bb* (add-bb bb (Scdr l)))))))
          (list bb)))
    (define (extract-common-tail bb1 bb2 cont) (set-bbv-version-limit! #f) 
      (let loop ((l1 (Sreverse (bb-non-branch-instrs bb1)))
                 (l2 (Sreverse (bb-non-branch-instrs bb2)))
                 (tail '()))
        (if (and (pair? l1) (pair? l2))
            (let ((i1 (Scar l1)) (i2 (Scar l2)))
              (if (eqv-gvm-instr? i1 i2)
                  (loop (Scdr l1) (Scdr l2) (cons i1 tail))
                  (cont l1 l2 tail)))
            (cont l1 l2 tail))))
    (define (eqv-bb? bb1 bb2) (set-bbv-version-limit! #f) 
      (let ((bb1-non-branch (bb-non-branch-instrs bb1))
            (bb2-non-branch (bb-non-branch-instrs bb2)))
        (and (SFX= (Slength bb1-non-branch) (Slength bb2-non-branch))
             (eqv-gvm-instr? (bb-label-instr bb1) (bb-label-instr bb2))
             (eqv-gvm-instr? (bb-branch-instr bb1) (bb-branch-instr bb2))
             (eqv-list? eqv-gvm-instr? bb1-non-branch bb2-non-branch))))
    (define (eqv-list? pred? l1 l2) (set-bbv-version-limit! #f) 
      (if (pair? l1)
          (and (pair? l2)
               (pred? (Scar l1) (Scar l2))
               (eqv-list? pred? (Scdr l1) (Scdr l2)))
          (not (pair? l2))))
    (define (eqv-lbl-num? lbl1 lbl2) (set-bbv-version-limit! #f) 
      (SFX= (replacement-lbl-num lbl1) (replacement-lbl-num lbl2)))
    (define (eqv-gvm-opnd? opnd1 opnd2) (set-bbv-version-limit! #f) 
      (if (not opnd1)
          (not opnd2)
          (and opnd2
               (cond ((lbl? opnd1)
                      (and (lbl? opnd2)
                           (eqv-lbl-num? (lbl-num opnd1) (lbl-num opnd2))))
                     ((clo? opnd1)
                      (and (clo? opnd2)
                           (SFX= (clo-index opnd1) (clo-index opnd2))
                           (eqv-gvm-opnd? (clo-base opnd1) (clo-base opnd2))))
                     (else (eqv? opnd1 opnd2))))))
    (define (eqv-gvm-instr? instr1 instr2) (set-bbv-version-limit! #f) 
      (define (eqv-closure-parms? p1 p2) (set-bbv-version-limit! #f) 
        (and (eqv-gvm-opnd? (closure-parms-loc p1) (closure-parms-loc p2))
             (eqv-lbl-num? (closure-parms-lbl p1) (closure-parms-lbl p2))
             (eqv-list?
              eqv-gvm-opnd?
              (closure-parms-opnds p1)
              (closure-parms-opnds p2))))
      (let ((type1 (gvm-instr-type instr1)) (type2 (gvm-instr-type instr2)))
        (and (eq? type1 type2)
             (frame-eq? (gvm-instr-frame instr1) (gvm-instr-frame instr2))
             (case type1
               ((label)
                (let ((ltype1 (label-type instr1))
                      (ltype2 (label-type instr2)))
                  (and (eq? ltype1 ltype2)
                       (case ltype1
                         ((simple return task-entry task-return) #t)
                         ((entry)
                          (and (SFX= (label-entry-min instr1)
                                  (label-entry-min instr2))
                               (SFX= (label-entry-nb-parms instr1)
                                  (label-entry-nb-parms instr2))
                               (eq? (label-entry-rest? instr1)
                                    (label-entry-rest? instr2))
                               (eq? (label-entry-closed? instr1)
                                    (label-entry-closed? instr2))))
                         (else
                          (compiler-internal-error
                           "eqv-gvm-instr?, unknown label type"))))))
               ((apply)
                (and (eq? (apply-prim instr1) (apply-prim instr2))
                     (eqv-list?
                      eqv-gvm-opnd?
                      (apply-opnds instr1)
                      (apply-opnds instr2))
                     (eqv-gvm-opnd? (apply-loc instr1) (apply-loc instr2))))
               ((copy)
                (and (eqv-gvm-opnd? (copy-opnd instr1) (copy-opnd instr2))
                     (eqv-gvm-opnd? (copy-loc instr1) (copy-loc instr2))))
               ((close)
                (eqv-list?
                 eqv-closure-parms?
                 (close-parms instr1)
                 (close-parms instr2)))
               ((ifjump)
                (and (eq? (ifjump-test instr1) (ifjump-test instr2))
                     (eqv-list?
                      eqv-gvm-opnd?
                      (ifjump-opnds instr1)
                      (ifjump-opnds instr2))
                     (eqv-lbl-num? (ifjump-true instr1) (ifjump-true instr2))
                     (eqv-lbl-num? (ifjump-false instr1) (ifjump-false instr2))
                     (eq? (ifjump-poll? instr1) (ifjump-poll? instr2))))
               ((jump)
                (and (eqv-gvm-opnd? (jump-opnd instr1) (jump-opnd instr2))
                     (eqv? (jump-nb-args instr1) (jump-nb-args instr2))
                     (eq? (jump-poll? instr1) (jump-poll? instr2))))
               (else
                (compiler-internal-error
                 "eqv-gvm-instr?, unknown 'gvm-instr':"
                 instr1))))))
    (define (update-bb! bb) (set-bbv-version-limit! #f)  (replace-label-references! bb replacement-lbl-num))
    (for-each enter-bb! bb-list)
    (bbs-entry-lbl-num-set! bbs (replacement-lbl-num (bbs-entry-lbl-num bbs)))
    (let loop ((i 0) (result '()))
      (if (SFX< i hash-table-length)
          (let ((bb-kept (Svector-ref hash-table i)))
            (for-each update-bb! bb-kept)
            (loop (SFX+ i 1) (Sappend bb-kept result)))
          (bbs-bb-queue-set! bbs (list->queue result))))
    changed?))
(define (replace-label-references! bb replacement-lbl-num) (set-bbv-version-limit! #f) 
  (define (update-gvm-opnd opnd) (set-bbv-version-limit! #f) 
    (if opnd
        (cond ((lbl? opnd) (make-lbl (replacement-lbl-num (lbl-num opnd))))
              ((clo? opnd)
               (make-clo (update-gvm-opnd (clo-base opnd)) (clo-index opnd)))
              (else opnd))
        opnd))
  (define (update-gvm-instr instr) (set-bbv-version-limit! #f) 
    (define (update-closure-parms p) (set-bbv-version-limit! #f) 
      (make-closure-parms
       (update-gvm-opnd (closure-parms-loc p))
       (replacement-lbl-num (closure-parms-lbl p))
       (Smap2 update-gvm-opnd (closure-parms-opnds p))))
    (case (gvm-instr-type instr)
      ((apply)
       (make-apply
        (apply-prim instr)
        (Smap2 update-gvm-opnd (apply-opnds instr))
        (update-gvm-opnd (apply-loc instr))
        (gvm-instr-frame instr)
        (gvm-instr-comment instr)))
      ((copy)
       (make-copy
        (update-gvm-opnd (copy-opnd instr))
        (update-gvm-opnd (copy-loc instr))
        (gvm-instr-frame instr)
        (gvm-instr-comment instr)))
      ((close)
       (make-close
        (Smap2 update-closure-parms (close-parms instr))
        (gvm-instr-frame instr)
        (gvm-instr-comment instr)))
      ((ifjump)
       (make-ifjump
        (ifjump-test instr)
        (Smap2 update-gvm-opnd (ifjump-opnds instr))
        (replacement-lbl-num (ifjump-true instr))
        (replacement-lbl-num (ifjump-false instr))
        (ifjump-poll? instr)
        (gvm-instr-frame instr)
        (gvm-instr-comment instr)))
      ((jump)
       (make-jump
        (update-gvm-opnd (jump-opnd instr))
        (jump-nb-args instr)
        (jump-poll? instr)
        (gvm-instr-frame instr)
        (gvm-instr-comment instr)))
      (else
       (compiler-internal-error "update-gvm-instr, unknown 'instr':" instr))))
  (bb-non-branch-instrs-set!
   bb
   (Smap2 update-gvm-instr (bb-non-branch-instrs bb)))
  (bb-branch-instr-set! bb (update-gvm-instr (bb-branch-instr bb))))
(define (bbs-order! bbs) (set-bbv-version-limit! #f) 
  (let ((new-bb-queue (queue-empty))
        (left-to-schedule (queue->list (bbs-bb-queue bbs))))
    (define (remove x l) (set-bbv-version-limit! #f) 
      (if (eq? (Scar l) x) (Scdr l) (cons (Scar l) (remove x (Scdr l)))))
    (define (remove-bb! bb) (set-bbv-version-limit! #f) 
      (set! left-to-schedule (remove bb left-to-schedule))
      bb)
    (define (prec-bb bb) (set-bbv-version-limit! #f) 
      (let loop ((l (bb-precedents bb)) (best #f) (best-fs #f))
        (if (null? l)
            best
            (let* ((x (Scar l)) (x-fs (bb-exit-frame-size x)))
              (if (and (Smemq x left-to-schedule)
                       (or (not best) (SFX< x-fs best-fs)))
                  (loop (Scdr l) x x-fs)
                  (loop (Scdr l) best best-fs))))))
    (define (succ-bb bb) (set-bbv-version-limit! #f) 
      (define (branches-to-lbl? bb) (set-bbv-version-limit! #f) 
        (let ((branch (bb-branch-instr bb)))
          (case (gvm-instr-type branch)
            ((ifjump) #t)
            ((jump) (lbl? (jump-opnd branch)))
            (else
             (compiler-internal-error "bbs-order!, unknown branch type")))))
      (define (best-succ bb1 bb2) (set-bbv-version-limit! #f) 
        (if (branches-to-lbl? bb1)
            bb1
            (if (branches-to-lbl? bb2)
                bb2
                (if (SFX< (bb-exit-frame-size bb1) (bb-exit-frame-size bb2))
                    bb2
                    bb1))))
      (let ((branch (bb-branch-instr bb)))
        (case (gvm-instr-type branch)
          ((ifjump)
           (let* ((true-bb (lbl-num->bb (ifjump-true branch) bbs))
                  (true-bb* (and (Smemq true-bb left-to-schedule) true-bb))
                  (false-bb (lbl-num->bb (ifjump-false branch) bbs))
                  (false-bb* (and (Smemq false-bb left-to-schedule) false-bb)))
             (if (and true-bb* false-bb*)
                 (best-succ true-bb* false-bb*)
                 (or true-bb* false-bb*))))
          ((jump)
           (let ((opnd (jump-opnd branch)))
             (and (lbl? opnd)
                  (let ((bb (lbl-num->bb (lbl-num opnd) bbs)))
                    (and (Smemq bb left-to-schedule) bb)))))
          (else (compiler-internal-error "bbs-order!, unknown branch type")))))
    (define (schedule-from bb) (set-bbv-version-limit! #f) 
      (queue-put! new-bb-queue bb)
      (let ((x (succ-bb bb)))
        (if x
            (begin
              (schedule-around (remove-bb! x))
              (let ((y (succ-bb bb)))
                (if y (schedule-around (remove-bb! y)))))))
      (schedule-refs bb))
    (define (schedule-around bb) (set-bbv-version-limit! #f) 
      (let ((x (prec-bb bb)))
        (if x
            (let ((bb-list (schedule-back (remove-bb! x) '())))
              (queue-put! new-bb-queue x)
              (schedule-forw bb)
              (for-each schedule-refs bb-list))
            (schedule-from bb))))
    (define (schedule-back bb bb-list) (set-bbv-version-limit! #f) 
      (let ((bb-list* (cons bb bb-list)) (x (prec-bb bb)))
        (if x
            (let ((bb-list (schedule-back (remove-bb! x) bb-list*)))
              (queue-put! new-bb-queue x)
              bb-list)
            bb-list*)))
    (define (schedule-forw bb) (set-bbv-version-limit! #f) 
      (queue-put! new-bb-queue bb)
      (let ((x (succ-bb bb)))
        (if x
            (begin
              (schedule-forw (remove-bb! x))
              (let ((y (succ-bb bb)))
                (if y (schedule-around (remove-bb! y)))))))
      (schedule-refs bb))
    (define (schedule-refs bb) (set-bbv-version-limit! #f) 
      (for-each
       (lambda (x) (set-bbv-version-limit! #f) 
         (if (Smemq x left-to-schedule) (schedule-around (remove-bb! x))))
       (bb-references bb)))
    (schedule-from (remove-bb! (lbl-num->bb (bbs-entry-lbl-num bbs) bbs)))
    (bbs-bb-queue-set! bbs new-bb-queue)
    (let ((bb-list (queue->list new-bb-queue)))
      (let loop ((l bb-list) (i 1) (lbl-map '()))
        (if (pair? l)
            (let* ((label-instr (bb-label-instr (Scar l)))
                   (old-lbl-num (label-lbl-num label-instr)))
              (label-lbl-num-set! label-instr i)
              (loop (Scdr l) (SFX+ i 1) (cons (cons old-lbl-num i) lbl-map)))
            (let ()
              (define (replacement-lbl-num x) (set-bbv-version-limit! #f)  (Scdr (Sassv x lbl-map)))
              (define (update-bb! bb) (set-bbv-version-limit! #f) 
                (replace-label-references! bb replacement-lbl-num))
              (for-each update-bb! bb-list)
              (bbs-lbl-counter-set!
               bbs
               (make-counter
                (SFX* (SFX+ 1 (SFXquotient (bbs-new-lbl! bbs) 1000)) 1000)
                label-limit
                bbs-limit-err))))))))
(define (make-code bb gvm-instr sn) (set-bbv-version-limit! #f)  (vector bb gvm-instr sn))
(define (code-bb code) (set-bbv-version-limit! #f)  (Svector-ref code 0))
(define (code-gvm-instr code) (set-bbv-version-limit! #f)  (Svector-ref code 1))
(define (code-slots-needed code) (set-bbv-version-limit! #f)  (Svector-ref code 2))
(define (code-slots-needed-set! code n) (set-bbv-version-limit! #f)  (Svector-set! code 2 n))
(define (bbs->code-list bbs) (set-bbv-version-limit! #f) 
  (let ((code-list (linearize bbs)))
    (setup-slots-needed! code-list)
    code-list))
(define (linearize bbs) (set-bbv-version-limit! #f) 
  (let ((code-queue (queue-empty)))
    (define (put-bb bb) (set-bbv-version-limit! #f) 
      (define (put-instr gvm-instr) (set-bbv-version-limit! #f) 
        (queue-put! code-queue (make-code bb gvm-instr #f)))
      (put-instr (bb-label-instr bb))
      (for-each put-instr (bb-non-branch-instrs bb))
      (put-instr (bb-branch-instr bb)))
    (for-each put-bb (queue->list (bbs-bb-queue bbs)))
    (queue->list code-queue)))
(define (setup-slots-needed! code-list) (set-bbv-version-limit! #f) 
  (if (null? code-list)
      #f
      (let* ((code (Scar code-list))
             (gvm-instr (code-gvm-instr code))
             (sn-rest (setup-slots-needed! (Scdr code-list))))
        (case (gvm-instr-type gvm-instr)
          ((label)
           (if (SFX> sn-rest (frame-size (gvm-instr-frame gvm-instr)))
               (compiler-internal-error
                "setup-slots-needed!, incoherent slots needed for LABEL"))
           (code-slots-needed-set! code sn-rest)
           #f)
          ((ifjump jump)
           (let ((sn (frame-size (gvm-instr-frame gvm-instr))))
             (code-slots-needed-set! code sn)
             (need-gvm-instr gvm-instr sn)))
          (else
           (code-slots-needed-set! code sn-rest)
           (need-gvm-instr gvm-instr sn-rest))))))
(define (need-gvm-instrs non-branch branch) (set-bbv-version-limit! #f) 
  (if (pair? non-branch)
      (need-gvm-instr
       (Scar non-branch)
       (need-gvm-instrs (Scdr non-branch) branch))
      (need-gvm-instr branch (frame-size (gvm-instr-frame branch)))))
(define (need-gvm-instr gvm-instr sn-rest) (set-bbv-version-limit! #f) 
  (case (gvm-instr-type gvm-instr)
    ((label) sn-rest)
    ((apply)
     (let ((loc (apply-loc gvm-instr)))
       (need-gvm-opnds
        (apply-opnds gvm-instr)
        (need-gvm-loc-opnd loc (need-gvm-loc loc sn-rest)))))
    ((copy)
     (let ((loc (copy-loc gvm-instr)))
       (need-gvm-opnd
        (copy-opnd gvm-instr)
        (need-gvm-loc-opnd loc (need-gvm-loc loc sn-rest)))))
    ((close)
     (let ((parms (close-parms gvm-instr)))
       (define (need-parms-opnds p) (set-bbv-version-limit! #f) 
         (if (null? p)
             sn-rest
             (need-gvm-opnds
              (closure-parms-opnds (Scar p))
              (need-parms-opnds (Scdr p)))))
       (define (need-parms-loc p) (set-bbv-version-limit! #f) 
         (if (null? p)
             (need-parms-opnds parms)
             (let ((loc (closure-parms-loc (Scar p))))
               (need-gvm-loc-opnd
                loc
                (need-gvm-loc loc (need-parms-loc (Scdr p)))))))
       (need-parms-loc parms)))
    ((ifjump) (need-gvm-opnds (ifjump-opnds gvm-instr) sn-rest))
    ((jump) (need-gvm-opnd (jump-opnd gvm-instr) sn-rest))
    (else
     (compiler-internal-error
      "need-gvm-instr, unknown 'gvm-instr':"
      gvm-instr))))
(define (need-gvm-loc loc sn-rest) (set-bbv-version-limit! #f) 
  (if (and loc (stk? loc) (SFX>= (stk-num loc) sn-rest))
      (SFX- (stk-num loc) 1)
      sn-rest))
(define (need-gvm-loc-opnd gvm-loc slots-needed) (set-bbv-version-limit! #f) 
  (if (and gvm-loc (clo? gvm-loc))
      (need-gvm-opnd (clo-base gvm-loc) slots-needed)
      slots-needed))
(define (need-gvm-opnd gvm-opnd slots-needed) (set-bbv-version-limit! #f) 
  (cond ((stk? gvm-opnd) (max (stk-num gvm-opnd) slots-needed))
        ((clo? gvm-opnd) (need-gvm-opnd (clo-base gvm-opnd) slots-needed))
        (else slots-needed)))
(define (need-gvm-opnds gvm-opnds slots-needed) (set-bbv-version-limit! #f) 
  (if (null? gvm-opnds)
      slots-needed
      (need-gvm-opnd
       (Scar gvm-opnds)
       (need-gvm-opnds (Scdr gvm-opnds) slots-needed))))
(define (write-bb bb port) (set-bbv-version-limit! #f) 
  (write-gvm-instr (bb-label-instr bb) port)
  (display " [precedents=" port)
  (write (Smap2 bb-lbl-num (bb-precedents bb)) port)
  (display "]" port)
  (newline port)
  (for-each
   (lambda (x) (set-bbv-version-limit! #f)  (write-gvm-instr x port) (newline port))
   (bb-non-branch-instrs bb))
  (write-gvm-instr (bb-branch-instr bb) port))
(define (write-bbs bbs port) (set-bbv-version-limit! #f) 
  (for-each
   (lambda (bb) (set-bbv-version-limit! #f) 
     (if (SFX= (bb-lbl-num bb) (bbs-entry-lbl-num bbs))
         (begin (display "**** Entry block:" port) (newline port)))
     (write-bb bb port)
     (newline port))
   (queue->list (bbs-bb-queue bbs))))
(define (virtual.dump proc port) (set-bbv-version-limit! #f) 
  (let ((proc-seen (queue-empty)) (proc-left (queue-empty)))
    (define (scan-opnd gvm-opnd) (set-bbv-version-limit! #f) 
      (cond ((obj? gvm-opnd)
             (let ((val (obj-val gvm-opnd)))
               (if (and (proc-obj? val)
                        (proc-obj-code val)
                        (not (Smemq val (queue->list proc-seen))))
                   (begin
                     (queue-put! proc-seen val)
                     (queue-put! proc-left val)))))
            ((clo? gvm-opnd) (scan-opnd (clo-base gvm-opnd)))))
    (define (dump-proc p) (set-bbv-version-limit! #f) 
      (define (scan-code code) (set-bbv-version-limit! #f) 
        (let ((gvm-instr (code-gvm-instr code)))
          (write-gvm-instr gvm-instr port)
          (newline port)
          (case (gvm-instr-type gvm-instr)
            ((apply)
             (for-each scan-opnd (apply-opnds gvm-instr))
             (if (apply-loc gvm-instr) (scan-opnd (apply-loc gvm-instr))))
            ((copy)
             (scan-opnd (copy-opnd gvm-instr))
             (scan-opnd (copy-loc gvm-instr)))
            ((close)
             (for-each
              (lambda (parms) (set-bbv-version-limit! #f) 
                (scan-opnd (closure-parms-loc parms))
                (for-each scan-opnd (closure-parms-opnds parms)))
              (close-parms gvm-instr)))
            ((ifjump) (for-each scan-opnd (ifjump-opnds gvm-instr)))
            ((jump) (scan-opnd (jump-opnd gvm-instr)))
            (else '()))))
      (if (proc-obj-primitive? p)
          (display "**** #[primitive " port)
          (display "**** #[procedure " port))
      (display (proc-obj-name p) port)
      (display "] =" port)
      (newline port)
      (let loop ((l (bbs->code-list (proc-obj-code p)))
                 (prev-filename "")
                 (prev-line 0))
        (if (pair? l)
            (let* ((code (Scar l))
                   (instr (code-gvm-instr code))
                   (src (comment-get (gvm-instr-comment instr) 'source))
                   (loc (and src (source-locat src)))
                   (filename
                    (if (and loc (eq? (Svector-ref loc 0) 'file))
                        (Svector-ref loc 1)
                        prev-filename))
                   (line (if (and loc (eq? (Svector-ref loc 0) 'file))
                             (Svector-ref loc 3)
                             prev-line)))
              (if (or (not (Sstring=? filename prev-filename))
                      (not (SFX= line prev-line)))
                  (begin
                    (display "#line " port)
                    (display line port)
                    (if (not (Sstring=? filename prev-filename))
                        (begin (display " " port) (write filename port)))
                    (newline port)))
              (scan-code code)
              (loop (Scdr l) filename line))
            (newline port))))
    (scan-opnd (make-obj proc))
    (let loop ()
      (if (not (queue-empty? proc-left))
          (begin (dump-proc (queue-get! proc-left)) (loop))))))
(define (write-gvm-instr gvm-instr port) (set-bbv-version-limit! #f) 
  (define (write-closure-parms parms) (set-bbv-version-limit! #f) 
    (display " " port)
    (let ((len (SFX+ 1 (write-gvm-opnd (closure-parms-loc parms) port))))
      (display " = (" port)
      (let ((len (SFX+ len (SFX+ 4 (write-gvm-lbl (closure-parms-lbl parms) port)))))
        (SFX+ len
           (write-terminated-opnd-list (closure-parms-opnds parms) port)))))
  (define (write-terminated-opnd-list l port) (set-bbv-version-limit! #f) 
    (let loop ((l l) (len 0))
      (if (pair? l)
          (let ((opnd (Scar l)))
            (display " " port)
            (loop (Scdr l) (SFX+ len (SFX+ 1 (write-gvm-opnd opnd port)))))
          (begin (display ")" port) (SFX+ len 1)))))
  (define (write-param-pattern gvm-instr port) (set-bbv-version-limit! #f) 
    (let ((len (if (not (SFX= (label-entry-min gvm-instr)
                           (label-entry-nb-parms gvm-instr)))
                   (let ((len (write-returning-len
                               (label-entry-min gvm-instr)
                               port)))
                     (display "-" port)
                     (SFX+ len 1))
                   0)))
      (let ((len (SFX+ len
                    (write-returning-len
                     (label-entry-nb-parms gvm-instr)
                     port))))
        (if (label-entry-rest? gvm-instr)
            (begin (display "+" port) (SFX+ len 1))
            len))))
  (define (write-prim-applic prim opnds port) (set-bbv-version-limit! #f) 
    (display "(" port)
    (let ((len (SFX+ 1 (display-returning-len (proc-obj-name prim) port))))
      (SFX+ len (write-terminated-opnd-list opnds port))))
  (define (write-instr gvm-instr) (set-bbv-version-limit! #f) 
    (case (gvm-instr-type gvm-instr)
      ((label)
       (let ((len (write-gvm-lbl (label-lbl-num gvm-instr) port)))
         (display " " port)
         (let ((len (SFX+ len
                       (SFX+ 1
                          (write-returning-len
                           (frame-size (gvm-instr-frame gvm-instr))
                           port)))))
           (case (label-type gvm-instr)
             ((simple) len)
             ((entry)
              (if (label-entry-closed? gvm-instr)
                  (begin
                    (display " closure-entry-point " port)
                    (SFX+ len (SFX+ 21 (write-param-pattern gvm-instr port))))
                  (begin
                    (display " entry-point " port)
                    (SFX+ len (SFX+ 13 (write-param-pattern gvm-instr port))))))
             ((return) (display " return-point" port) (SFX+ len 13))
             ((task-entry) (display " task-entry-point" port) (SFX+ len 17))
             ((task-return) (display " task-return-point" port) (SFX+ len 18))
             (else
              (compiler-internal-error
               "write-gvm-instr, unknown label type"))))))
      ((apply)
       (display "  " port)
       (let ((len (SFX+ 2
                     (if (apply-loc gvm-instr)
                         (let ((len (write-gvm-opnd
                                     (apply-loc gvm-instr)
                                     port)))
                           (display " = " port)
                           (SFX+ len 3))
                         0))))
         (SFX+ len
            (write-prim-applic
             (apply-prim gvm-instr)
             (apply-opnds gvm-instr)
             port))))
      ((copy)
       (display "  " port)
       (let ((len (SFX+ 2 (write-gvm-opnd (copy-loc gvm-instr) port))))
         (display " = " port)
         (SFX+ len (SFX+ 3 (write-gvm-opnd (copy-opnd gvm-instr) port)))))
      ((close)
       (display "  close" port)
       (let ((len (SFX+ 7 (write-closure-parms (Scar (close-parms gvm-instr))))))
         (let loop ((l (Scdr (close-parms gvm-instr))) (len len))
           (if (pair? l)
               (let ((x (Scar l)))
                 (display "," port)
                 (loop (Scdr l) (SFX+ len (SFX+ 1 (write-closure-parms x)))))
               len))))
      ((ifjump)
       (display "  if " port)
       (let ((len (SFX+ 5
                     (write-prim-applic
                      (ifjump-test gvm-instr)
                      (ifjump-opnds gvm-instr)
                      port))))
         (let ((len (SFX+ len
                       (if (ifjump-poll? gvm-instr)
                           (begin (display " jump* " port) 7)
                           (begin (display " jump " port) 6)))))
           (let ((len (SFX+ len
                         (write-returning-len
                          (frame-size (gvm-instr-frame gvm-instr))
                          port))))
             (display " " port)
             (let ((len (SFX+ len
                           (SFX+ 1
                              (write-gvm-lbl (ifjump-true gvm-instr) port)))))
               (display " else " port)
               (SFX+ len (SFX+ 6 (write-gvm-lbl (ifjump-false gvm-instr) port))))))))
      ((jump)
       (display "  " port)
       (let ((len (SFX+ 2
                     (if (jump-poll? gvm-instr)
                         (begin (display "jump* " port) 6)
                         (begin (display "jump " port) 5)))))
         (let ((len (SFX+ len
                       (write-returning-len
                        (frame-size (gvm-instr-frame gvm-instr))
                        port))))
           (display " " port)
           (let ((len (SFX+ len
                         (SFX+ 1 (write-gvm-opnd (jump-opnd gvm-instr) port)))))
             (SFX+ len
                (if (jump-nb-args gvm-instr)
                    (begin
                      (display " " port)
                      (SFX+ 1
                         (write-returning-len (jump-nb-args gvm-instr) port)))
                    0))))))
      (else
       (compiler-internal-error
        "write-gvm-instr, unknown 'gvm-instr':"
        gvm-instr))))
  (define (spaces n) (set-bbv-version-limit! #f) 
    (if (SFX> n 0)
        (if (SFX> n 7)
            (begin (display "        " port) (spaces (SFX- n 8)))
            (begin (display " " port) (spaces (SFX- n 1))))))
  (let ((len (write-instr gvm-instr)))
    (spaces (SFX- 40 len))
    (display " " port)
    (write-frame (gvm-instr-frame gvm-instr) port))
  (let ((x (gvm-instr-comment gvm-instr)))
    (if x
        (let ((y (comment-get x 'text)))
          (if y (begin (display " ; " port) (display y port)))))))
(define (write-frame frame port) (set-bbv-version-limit! #f) 
  (define (write-var var opnd sep) (set-bbv-version-limit! #f) 
    (display sep port)
    (write-gvm-opnd opnd port)
    (if var
        (begin
          (display "=" port)
          (cond ((eq? var closure-env-var)
                 (write (Smap2 (lambda (var) (set-bbv-version-limit! #f)  (var-name var))
                             (frame-closed frame))
                        port))
                ((eq? var ret-var) (display "#" port))
                ((temp-var? var) (display "." port))
                (else (write (var-name var) port))))))
  (define (live? var) (set-bbv-version-limit! #f) 
    (let ((live (frame-live frame)))
      (or (set-member? var live)
          (and (eq? var closure-env-var)
               (not (set-empty?
                     (set-intersection
                      live
                      (list->set (frame-closed frame)))))))))
  (let loop1 ((i 1) (l (Sreverse (frame-slots frame))) (sep "; "))
    (if (pair? l)
        (let ((var (Scar l)))
          (write-var (if (live? var) var #f) (make-stk i) sep)
          (loop1 (SFX+ i 1) (Scdr l) " "))
        (let loop2 ((i 0) (l (frame-regs frame)) (sep sep))
          (if (pair? l)
              (let ((var (Scar l)))
                (if (live? var)
                    (begin
                      (write-var var (make-reg i) sep)
                      (loop2 (SFX+ i 1) (Scdr l) " "))
                    (loop2 (SFX+ i 1) (Scdr l) sep))))))))
(define (write-gvm-opnd gvm-opnd port) (set-bbv-version-limit! #f) 
  (define (write-opnd) (set-bbv-version-limit! #f) 
    (cond ((reg? gvm-opnd)
           (display "+" port)
           (SFX+ 1 (write-returning-len (reg-num gvm-opnd) port)))
          ((stk? gvm-opnd)
           (display "-" port)
           (SFX+ 1 (write-returning-len (stk-num gvm-opnd) port)))
          ((glo? gvm-opnd) (write-returning-len (glo-name gvm-opnd) port))
          ((clo? gvm-opnd)
           (let ((len (write-gvm-opnd (clo-base gvm-opnd) port)))
             (display "(" port)
             (let ((len (SFX+ len
                           (SFX+ 1
                              (write-returning-len
                               (clo-index gvm-opnd)
                               port)))))
               (display ")" port)
               (SFX+ len 1))))
          ((lbl? gvm-opnd) (write-gvm-lbl (lbl-num gvm-opnd) port))
          ((obj? gvm-opnd)
           (display "'" port)
           (SFX+ (write-gvm-obj (obj-val gvm-opnd) port) 1))
          (else
           (compiler-internal-error
            "write-gvm-opnd, unknown 'gvm-opnd':"
            gvm-opnd))))
  (write-opnd))
(define (write-gvm-lbl lbl port) (set-bbv-version-limit! #f) 
  (display "#" port)
  (SFX+ (write-returning-len lbl port) 1))
(define (write-gvm-obj val port) (set-bbv-version-limit! #f) 
  (cond ((false-object? val) (display "#f" port) 2)
        ((undef-object? val) (display "#[undefined]" port) 12)
        ((proc-obj? val)
         (if (proc-obj-primitive? val)
             (display "#[primitive " port)
             (display "#[procedure " port))
         (let ((len (display-returning-len (proc-obj-name val) port)))
           (display "]" port)
           (SFX+ len 13)))
        (else (write-returning-len val port))))
(define (virtual.begin!) (set-bbv-version-limit! #f) 
  (set! *opnd-table* (Smake-vector1 opnd-table-size))
  (set! *opnd-table-alloc* 0)
  '())
(define (virtual.end!) (set-bbv-version-limit! #f)  (set! *opnd-table* '()) '())
(define (make-target version name) (set-bbv-version-limit! #f) 
  (define current-target-version 4)
  (if (not (SFX= version current-target-version))
      (compiler-internal-error
       "make-target, version of target package is not current"
       name))
  (let ((x (Smake-vector1 11))) (Svector-set! x 1 name) x))
(define (target-name x) (set-bbv-version-limit! #f)  (Svector-ref x 1))
(define (target-begin! x) (set-bbv-version-limit! #f)  (Svector-ref x 2))
(define (target-begin!-set! x y) (set-bbv-version-limit! #f)  (Svector-set! x 2 y))
(define (target-end! x) (set-bbv-version-limit! #f)  (Svector-ref x 3))
(define (target-end!-set! x y) (set-bbv-version-limit! #f)  (Svector-set! x 3 y))
(define (target-dump x) (set-bbv-version-limit! #f)  (Svector-ref x 4))
(define (target-dump-set! x y) (set-bbv-version-limit! #f)  (Svector-set! x 4 y))
(define (target-nb-regs x) (set-bbv-version-limit! #f)  (Svector-ref x 5))
(define (target-nb-regs-set! x y) (set-bbv-version-limit! #f)  (Svector-set! x 5 y))
(define (target-prim-info x) (set-bbv-version-limit! #f)  (Svector-ref x 6))
(define (target-prim-info-set! x y) (set-bbv-version-limit! #f)  (Svector-set! x 6 y))
(define (target-label-info x) (set-bbv-version-limit! #f)  (Svector-ref x 7))
(define (target-label-info-set! x y) (set-bbv-version-limit! #f)  (Svector-set! x 7 y))
(define (target-jump-info x) (set-bbv-version-limit! #f)  (Svector-ref x 8))
(define (target-jump-info-set! x y) (set-bbv-version-limit! #f)  (Svector-set! x 8 y))
(define (target-proc-result x) (set-bbv-version-limit! #f)  (Svector-ref x 9))
(define (target-proc-result-set! x y) (set-bbv-version-limit! #f)  (Svector-set! x 9 y))
(define (target-task-return x) (set-bbv-version-limit! #f)  (Svector-ref x 10))
(define (target-task-return-set! x y) (set-bbv-version-limit! #f)  (Svector-set! x 10 y))
(define targets-loaded '())
(define (get-target name) (set-bbv-version-limit! #f) 
  (let ((x (Sassq name targets-loaded)))
    (if x (Scdr x) (compiler-error "Target package is not available" name))))
(define (put-target targ) (set-bbv-version-limit! #f) 
  (let* ((name (target-name targ)) (x (Sassq name targets-loaded)))
    (if x
        (Sset-cdr! x targ)
        (set! targets-loaded (cons (cons name targ) targets-loaded)))
    '()))
(define (default-target) (set-bbv-version-limit! #f) 
  (if (null? targets-loaded)
      (compiler-error "No target package is available")
      (Scar (Scar targets-loaded))))
(define (select-target! name info-port) (set-bbv-version-limit! #f) 
  (set! target (get-target name))
  ((target-begin! target) info-port)
  (set! target.dump (target-dump target))
  (set! target.nb-regs (target-nb-regs target))
  (set! target.prim-info (target-prim-info target))
  (set! target.label-info (target-label-info target))
  (set! target.jump-info (target-jump-info target))
  (set! target.proc-result (target-proc-result target))
  (set! target.task-return (target-task-return target))
  (set! **not-proc-obj (target.prim-info **not-sym))
  '())
(define (unselect-target!) (set-bbv-version-limit! #f)  ((target-end! target)) '())
(define target '())
(define target.dump '())
(define target.nb-regs '())
(define target.prim-info '())
(define target.label-info '())
(define target.jump-info '())
(define target.proc-result '())
(define target.task-return '())
(define **not-proc-obj '())
(define (target.specialized-prim-info* name decl) (set-bbv-version-limit! #f) 
  (let ((x (target.prim-info* name decl)))
    (and x ((proc-obj-specialize x) decl))))
(define (target.prim-info* name decl) (set-bbv-version-limit! #f) 
  (and (if (standard-procedure name decl)
           (standard-binding? name decl)
           (extended-binding? name decl))
       (target.prim-info name)))
(define generic-sym (string->canonical-symbol "GENERIC"))
(define fixnum-sym (string->canonical-symbol "FIXNUM"))
(define flonum-sym (string->canonical-symbol "FLONUM"))
(define-namable-decl generic-sym 'arith)
(define-namable-decl fixnum-sym 'arith)
(define-namable-decl flonum-sym 'arith)
(define (arith-implementation name decls) (set-bbv-version-limit! #f) 
  (declaration-value 'arith name generic-sym decls))
(define (cf source target-name . opts) (set-bbv-version-limit! #f) 
  (let* ((dest (file-root source))
         (module-name (file-name dest))
         (info-port (if (Smemq 'verbose opts) (current-output-port) #f))
         (result (compile-program
                  (list **include-sym source)
                  (if target-name target-name (default-target))
                  opts
                  module-name
                  dest
                  info-port)))
    (if (and info-port (not (eq? info-port (current-output-port))))
        (close-output-port info-port))
    result))
(define (ce source target-name . opts) (set-bbv-version-limit! #f) 
  (let* ((dest "program")
         (module-name "program")
         (info-port (if (Smemq 'verbose opts) (current-output-port) #f))
         (result (compile-program
                  source
                  (if target-name target-name (default-target))
                  opts
                  module-name
                  dest
                  info-port)))
    (if (and info-port (not (eq? info-port (current-output-port))))
        (close-output-port info-port))
    result))
(define wrap-program #f)
(set! wrap-program (lambda (program) (set-bbv-version-limit! #f)  program))
(define (compile-program program target-name opts module-name dest info-port) (set-bbv-version-limit! #f) 
  (define (compiler-body) (set-bbv-version-limit! #f) 
    (if (not (valid-module-name? module-name))
        (compiler-error
         "Invalid characters in file name (must be a symbol with no \"#\")")
        (begin
          (ptree.begin! info-port)
          (virtual.begin!)
          (select-target! target-name info-port)
          (parse-program
           (list (expression->source (wrap-program program) #f))
           (make-global-environment)
           module-name
           (lambda (lst env c-intf) (set-bbv-version-limit! #f) 
             (let ((parsed-program
                    (Smap2 (lambda (x) (set-bbv-version-limit! #f)  (normalize-parse-tree (Scar x) (Scdr x)))
                         lst)))
               (if (Smemq 'expansion opts)
                   (let ((port (current-output-port)))
                     (display "Expansion:" port)
                     (newline port)
                     (let loop ((l parsed-program))
                       (if (pair? l)
                           (let ((ptree (Scar l)))
                             (pp-expression
                              (parse-tree->expression ptree)
                              port)
                             (loop (Scdr l)))))
                     (newline port)))
               (let ((module-init-proc
                      (compile-parsed-program
                       module-name
                       parsed-program
                       env
                       c-intf
                       info-port)))
                 (if (Smemq 'report opts) (generate-report env))
                 (if (Smemq 'gvm opts)
                     (let ((gvm-port
                            (open-output-file (Sstring-append dest ".gvm"))))
                       (virtual.dump module-init-proc gvm-port)
                       (close-output-port gvm-port)))
                 (target.dump module-init-proc dest c-intf opts)
                 (dump-c-intf module-init-proc dest c-intf)))))
          (unselect-target!)
          (virtual.end!)
          (ptree.end!)
          #t)))
  (let ((successful (with-exception-handling compiler-body)))
    (if info-port
        (if successful
            (begin
              (display "Compilation finished." info-port)
              (newline info-port))
            (begin
              (display "Compilation terminated abnormally." info-port)
              (newline info-port))))
    successful))
(define (valid-module-name? module-name) (set-bbv-version-limit! #f) 
  (define (valid-char? c) (set-bbv-version-limit! #f) 
    (and (not (Smemv c
                    '(#\#
                      #\;
                      #\(
                      #\)
                      #\space
                      #\[
                      #\]
                      #\{
                      #\}
                      #\"
                      #\'
                      #\`
                      #\,)))
         (not (Schar-whitespace? c))))
  (let ((n (Sstring-length module-name)))
    (and (SFX> n 0)
         (not (Sstring=? module-name "."))
         (not (Sstring->number2 module-name 10))
         (let loop ((i 0))
           (if (SFX< i n)
               (if (valid-char? (Sstring-ref module-name i)) (loop (SFX+ i 1)) #f)
               #t)))))
(define (dump-c-intf module-init-proc dest c-intf) (set-bbv-version-limit! #f) 
  (let ((decls (c-intf-decls c-intf))
        (procs (c-intf-procs c-intf))
        (inits (c-intf-inits c-intf)))
    (if (or (not (null? decls)) (not (null? procs)) (not (null? inits)))
        (let* ((module-name (proc-obj-name module-init-proc))
               (filename (Sstring-append dest ".c"))
               (port (open-output-file filename)))
          (display "/* File: \"" port)
          (display filename port)
          (display "\", C-interface file produced by Gambit " port)
          (display compiler-version port)
          (display " */" port)
          (newline port)
          (display "#define " port)
          (display c-id-prefix port)
          (display "MODULE_NAME \"" port)
          (display module-name port)
          (display "\"" port)
          (newline port)
          (display "#define " port)
          (display c-id-prefix port)
          (display "MODULE_LINKER " port)
          (display c-id-prefix port)
          (display (scheme-id->c-id module-name) port)
          (newline port)
          (display "#define " port)
          (display c-id-prefix port)
          (display "VERSION \"" port)
          (display compiler-version port)
          (display "\"" port)
          (newline port)
          (if (not (null? procs))
              (begin
                (display "#define " port)
                (display c-id-prefix port)
                (display "C_PRC_COUNT " port)
                (display (Slength procs) port)
                (newline port)))
          (display "#include \"gambit.h\"" port)
          (newline port)
          (display c-id-prefix port)
          (display "BEGIN_MODULE" port)
          (newline port)
          (for-each
           (lambda (x) (set-bbv-version-limit! #f) 
             (let ((scheme-name (Svector-ref x 0)))
               (display c-id-prefix port)
               (display "SUPPLY_PRM(" port)
               (display c-id-prefix port)
               (display "P_" port)
               (display (scheme-id->c-id scheme-name) port)
               (display ")" port)
               (newline port)))
           procs)
          (newline port)
          (for-each (lambda (x) (set-bbv-version-limit! #f)  (display x port) (newline port)) decls)
          (if (not (null? procs))
              (begin
                (for-each
                 (lambda (x) (set-bbv-version-limit! #f) 
                   (let ((scheme-name (Svector-ref x 0))
                         (c-name (Svector-ref x 1))
                         (arity (Svector-ref x 2))
                         (def (Svector-ref x 3)))
                     (display c-id-prefix port)
                     (display "BEGIN_C_COD(" port)
                     (display c-name port)
                     (display "," port)
                     (display c-id-prefix port)
                     (display "P_" port)
                     (display (scheme-id->c-id scheme-name) port)
                     (display "," port)
                     (display arity port)
                     (display ")" port)
                     (newline port)
                     (display "#undef ___ARG1" port)
                     (newline port)
                     (display "#define ___ARG1 ___R1" port)
                     (newline port)
                     (display "#undef ___ARG2" port)
                     (newline port)
                     (display "#define ___ARG2 ___R2" port)
                     (newline port)
                     (display "#undef ___ARG3" port)
                     (newline port)
                     (display "#define ___ARG3 ___R3" port)
                     (newline port)
                     (display "#undef ___RESULT" port)
                     (newline port)
                     (display "#define ___RESULT ___R1" port)
                     (newline port)
                     (display def port)
                     (display c-id-prefix port)
                     (display "END_C_COD" port)
                     (newline port)))
                 procs)
                (newline port)
                (display c-id-prefix port)
                (display "BEGIN_C_PRC" port)
                (newline port)
                (let loop ((i 0) (lst procs))
                  (if (not (null? lst))
                      (let* ((x (Scar lst))
                             (scheme-name (Svector-ref x 0))
                             (c-name (Svector-ref x 1))
                             (arity (Svector-ref x 2)))
                        (if (SFX= i 0) (display " " port) (display "," port))
                        (display c-id-prefix port)
                        (display "DEF_C_PRC(" port)
                        (display c-name port)
                        (display "," port)
                        (display c-id-prefix port)
                        (display "P_" port)
                        (display (scheme-id->c-id scheme-name) port)
                        (display "," port)
                        (display arity port)
                        (display ")" port)
                        (newline port)
                        (loop (SFX+ i 1) (Scdr lst)))))
                (display c-id-prefix port)
                (display "END_C_PRC" port)
                (newline port)))
          (newline port)
          (display c-id-prefix port)
          (display "BEGIN_PRM" port)
          (newline port)
          (for-each (lambda (x) (set-bbv-version-limit! #f)  (display x port) (newline port)) inits)
          (display c-id-prefix port)
          (display "END_PRM" port)
          (newline port)
          (close-output-port port)))))
(define (generate-report env) (set-bbv-version-limit! #f) 
  (let ((vars (sort-variables (env-global-variables env)))
        (decl (env-declarations env)))
    (define (report title pred? vars wrote-something?) (set-bbv-version-limit! #f) 
      (if (pair? vars)
          (let ((var (Scar vars)))
            (if (pred? var)
                (begin
                  (if (not wrote-something?)
                      (begin (display " ") (display title) (newline)))
                  (let loop1 ((l (var-refs var)) (r? #f) (c? #f))
                    (if (pair? l)
                        (let* ((x (Scar l)) (y (node-parent x)))
                          (if (and y (app? y) (eq? x (app-oper y)))
                              (loop1 (Scdr l) r? #t)
                              (loop1 (Scdr l) #t c?)))
                        (let loop2 ((l (var-sets var)) (d? #f) (a? #f))
                          (if (pair? l)
                              (if (set? (Scar l))
                                  (loop2 (Scdr l) d? #t)
                                  (loop2 (Scdr l) #t a?))
                              (begin
                                (display "  [")
                                (if d? (display "D") (display " "))
                                (if a? (display "A") (display " "))
                                (if r? (display "R") (display " "))
                                (if c? (display "C") (display " "))
                                (display "] ")
                                (display (var-name var))
                                (newline))))))
                  (report title pred? (Scdr vars) #t))
                (cons (Scar vars)
                      (report title pred? (Scdr vars) wrote-something?))))
          (begin (if wrote-something? (newline)) '())))
    (display "Global variable usage:")
    (newline)
    (newline)
    (report "OTHERS"
            (lambda (x) (set-bbv-version-limit! #f)  #t)
            (report "EXTENDED"
                    (lambda (x) (set-bbv-version-limit! #f)  (target.prim-info (var-name x)))
                    (report "STANDARD"
                            (lambda (x) (set-bbv-version-limit! #f)  (standard-procedure (var-name x) decl))
                            vars
                            #f)
                    #f)
            #f)))
(define (compile-parsed-program module-name program env c-intf info-port) (set-bbv-version-limit! #f) 
  (if info-port (display "Compiling:" info-port))
  (set! trace-indentation 0)
  (set! *bbs* (make-bbs))
  (set! *global-env* env)
  (set! proc-queue '())
  (set! constant-vars '())
  (set! known-procs '())
  (restore-context (make-context 0 '() (list ret-var) '() (entry-poll) #f))
  (let* ((entry-lbl (bbs-new-lbl! *bbs*))
         (body-lbl (bbs-new-lbl! *bbs*))
         (frame (current-frame ret-var-set))
         (comment (if (null? program) #f (source-comment (Scar program)))))
    (bbs-entry-lbl-num-set! *bbs* entry-lbl)
    (set! entry-bb
          (make-bb (make-label-entry entry-lbl 0 0 #f #f frame comment) *bbs*))
    (bb-put-branch! entry-bb (make-jump (make-lbl body-lbl) #f #f frame #f))
    (set! *bb* (make-bb (make-label-simple body-lbl frame comment) *bbs*))
    (let loop1 ((l (c-intf-procs c-intf)))
      (if (not (null? l))
          (let* ((x (Scar l))
                 (name (Svector-ref x 0))
                 (sym (string->canonical-symbol name))
                 (var (env-lookup-global-var *global-env* sym)))
            (add-constant-var
             var
             (make-obj (make-proc-obj name #t #f 0 #t '() '(#f))))
            (loop1 (Scdr l)))))
    (let loop2 ((l program))
      (if (not (null? l))
          (let ((node (Scar l)))
            (if (def? node)
                (let* ((var (def-var node)) (val (global-val var)))
                  (if (and val (prc? val))
                      (add-constant-var
                       var
                       (make-obj
                        (make-proc-obj
                         (Ssymbol->string (var-name var))
                         #t
                         #f
                         (call-pattern val)
                         #t
                         '()
                         '(#f)))))))
            (loop2 (Scdr l)))))
    (let loop3 ((l program))
      (if (null? l)
          (let ((ret-opnd (var->opnd ret-var)))
            (seal-bb #t 'return)
            (dealloc-slots nb-slots)
            (bb-put-branch!
             *bb*
             (make-jump ret-opnd #f #f (current-frame (set-empty)) #f)))
          (let ((node (Scar l)))
            (if (def? node)
                (begin
                  (gen-define (def-var node) (def-val node) info-port)
                  (loop3 (Scdr l)))
                (if (null? (Scdr l))
                    (gen-node node ret-var-set 'tail)
                    (begin
                      (gen-node node ret-var-set 'need)
                      (loop3 (Scdr l))))))))
    (let loop4 ()
      (if (pair? proc-queue)
          (let ((x (Scar proc-queue)))
            (set! proc-queue (Scdr proc-queue))
            (gen-proc (Scar x) (Scadr x) (Scaddr x) info-port)
            (trace-unindent info-port)
            (loop4))))
    (if info-port (begin (newline info-port) (newline info-port)))
    (bbs-purify! *bbs*)
    (let ((proc (make-proc-obj
                 (Sstring-append "#!" module-name)
                 #t
                 *bbs*
                 '(0)
                 #t
                 '()
                 '(#f))))
      (set! *bb* '())
      (set! *bbs* '())
      (set! *global-env* '())
      (set! proc-queue '())
      (set! constant-vars '())
      (set! known-procs '())
      (clear-context)
      proc)))
(define *bb* '())
(define *bbs* '())
(define *global-env* '())
(define proc-queue '())
(define constant-vars '())
(define known-procs '())
(define trace-indentation '())
(define (trace-indent info-port) (set-bbv-version-limit! #f) 
  (set! trace-indentation (SFX+ trace-indentation 1))
  (if info-port
      (begin
        (newline info-port)
        (let loop ((i trace-indentation))
          (if (SFX> i 0) (begin (display "  " info-port) (loop (SFX- i 1))))))))
(define (trace-unindent info-port) (set-bbv-version-limit! #f) 
  (set! trace-indentation (SFX- trace-indentation 1)))
(define (gen-define var node info-port) (set-bbv-version-limit! #f) 
  (if (prc? node)
      (let* ((p-bbs *bbs*)
             (p-bb *bb*)
             (p-proc-queue proc-queue)
             (p-known-procs known-procs)
             (p-context (current-context))
             (bbs (make-bbs))
             (lbl1 (bbs-new-lbl! bbs))
             (lbl2 (bbs-new-lbl! bbs))
             (context (entry-context node '()))
             (frame (context->frame
                     context
                     (set-union (free-variables (prc-body node)) ret-var-set)))
             (bb1 (make-bb (make-label-entry
                            lbl1
                            (Slength (prc-parms node))
                            (prc-min node)
                            (prc-rest node)
                            #f
                            frame
                            (source-comment node))
                           bbs))
             (bb2 (make-bb (make-label-simple lbl2 frame (source-comment node))
                           bbs)))
        (define (do-body) (set-bbv-version-limit! #f) 
          (gen-proc node bb2 context info-port)
          (let loop ()
            (if (pair? proc-queue)
                (let ((x (Scar proc-queue)))
                  (set! proc-queue (Scdr proc-queue))
                  (gen-proc (Scar x) (Scadr x) (Scaddr x) info-port)
                  (trace-unindent info-port)
                  (loop))))
          (trace-unindent info-port)
          (bbs-purify! *bbs*))
        (context-entry-bb-set! context bb1)
        (bbs-entry-lbl-num-set! bbs lbl1)
        (bb-put-branch! bb1 (make-jump (make-lbl lbl2) #f #f frame #f))
        (set! *bbs* bbs)
        (set! proc-queue '())
        (set! known-procs '())
        (if (constant-var? var)
            (let-constant-var
             var
             (make-lbl lbl1)
             (lambda () (add-known-proc lbl1 node) (do-body)))
            (do-body))
        (set! *bbs* p-bbs)
        (set! *bb* p-bb)
        (set! proc-queue p-proc-queue)
        (set! known-procs p-known-procs)
        (restore-context p-context)
        (let* ((x (Sassq var constant-vars))
               (proc (if x
                         (let ((p (Scdr x)))
                           (proc-obj-code-set! (obj-val p) bbs)
                           p)
                         (make-obj
                          (make-proc-obj
                           (Ssymbol->string (var-name var))
                           #f
                           bbs
                           (call-pattern node)
                           #t
                           '()
                           '(#f))))))
          (put-copy
           proc
           (make-glo (var-name var))
           #f
           ret-var-set
           (source-comment node))))
      (put-copy
       (gen-node node ret-var-set 'need)
       (make-glo (var-name var))
       #f
       ret-var-set
       (source-comment node))))
(define (call-pattern node) (set-bbv-version-limit! #f) 
  (make-pattern (prc-min node) (Slength (prc-parms node)) (prc-rest node)))
(define (make-context nb-slots slots regs closed poll entry-bb) (set-bbv-version-limit! #f) 
  (vector nb-slots slots regs closed poll entry-bb))
(define (context-nb-slots x) (set-bbv-version-limit! #f)  (Svector-ref x 0))
(define (context-slots x) (set-bbv-version-limit! #f)  (Svector-ref x 1))
(define (context-regs x) (set-bbv-version-limit! #f)  (Svector-ref x 2))
(define (context-closed x) (set-bbv-version-limit! #f)  (Svector-ref x 3))
(define (context-poll x) (set-bbv-version-limit! #f)  (Svector-ref x 4))
(define (context-entry-bb x) (set-bbv-version-limit! #f)  (Svector-ref x 5))
(define (context-entry-bb-set! x y) (set-bbv-version-limit! #f)  (Svector-set! x 5 y))
(define nb-slots '())
(define slots '())
(define regs '())
(define closed '())
(define poll '())
(define entry-bb '())
(define (restore-context context) (set-bbv-version-limit! #f) 
  (set! nb-slots (context-nb-slots context))
  (set! slots (context-slots context))
  (set! regs (context-regs context))
  (set! closed (context-closed context))
  (set! poll (context-poll context))
  (set! entry-bb (context-entry-bb context)))
(define (clear-context) (set-bbv-version-limit! #f) 
  (restore-context (make-context '() '() '() '() '() '())))
(define (current-context) (set-bbv-version-limit! #f) 
  (make-context nb-slots slots regs closed poll entry-bb))
(define (current-frame live) (set-bbv-version-limit! #f)  (make-frame nb-slots slots regs closed live))
(define (context->frame context live) (set-bbv-version-limit! #f) 
  (make-frame
   (context-nb-slots context)
   (context-slots context)
   (context-regs context)
   (context-closed context)
   live))
(define (make-poll since-entry? delta) (set-bbv-version-limit! #f)  (cons since-entry? delta))
(define (poll-since-entry? x) (set-bbv-version-limit! #f)  (Scar x))
(define (poll-delta x) (set-bbv-version-limit! #f)  (Scdr x))
(define (entry-poll) (set-bbv-version-limit! #f)  (make-poll #f (SFX- poll-period poll-head)))
(define (return-poll poll) (set-bbv-version-limit! #f) 
  (let ((delta (poll-delta poll)))
    (make-poll (poll-since-entry? poll) (SFX+ poll-head (max delta poll-tail)))))
(define (poll-merge poll other-poll) (set-bbv-version-limit! #f) 
  (make-poll
   (or (poll-since-entry? poll) (poll-since-entry? other-poll))
   (max (poll-delta poll) (poll-delta other-poll))))
(define poll-period #f)
(set! poll-period 90)
(define poll-head #f)
(set! poll-head 15)
(define poll-tail #f)
(set! poll-tail 15)
(define (entry-context proc closed) (set-bbv-version-limit! #f) 
  (define (empty-vars-list n) (set-bbv-version-limit! #f) 
    (if (SFX> n 0) (cons empty-var (empty-vars-list (SFX- n 1))) '()))
  (let* ((parms (prc-parms proc))
         (pc (target.label-info
              (prc-min proc)
              (Slength parms)
              (prc-rest proc)
              (not (null? closed))))
         (fs (pcontext-fs pc))
         (slots-list (empty-vars-list fs))
         (regs-list (empty-vars-list target.nb-regs)))
    (define (assign-var-to-loc var loc) (set-bbv-version-limit! #f) 
      (let ((x (cond ((reg? loc)
                      (let ((i (reg-num loc)))
                        (if (SFX<= i target.nb-regs)
                            (nth-after regs-list i)
                            (compiler-internal-error
                             "entry-context, reg out of bound in back-end's pcontext"))))
                     ((stk? loc)
                      (let ((i (stk-num loc)))
                        (if (SFX<= i fs)
                            (nth-after slots-list (SFX- fs i))
                            (compiler-internal-error
                             "entry-context, stk out of bound in back-end's pcontext"))))
                     (else
                      (compiler-internal-error
                       "entry-context, loc other than reg or stk in back-end's pcontext")))))
        (if (eq? (Scar x) empty-var)
            (Sset-car! x var)
            (compiler-internal-error
             "entry-context, duplicate location in back-end's pcontext"))))
    (let loop ((l (pcontext-map pc)))
      (if (not (null? l))
          (let* ((couple (Scar l)) (name (Scar couple)) (loc (Scdr couple)))
            (cond ((eq? name 'return) (assign-var-to-loc ret-var loc))
                  ((eq? name 'closure-env)
                   (assign-var-to-loc closure-env-var loc))
                  (else (assign-var-to-loc (Slist-ref parms (SFX- name 1)) loc)))
            (loop (Scdr l)))))
    (make-context fs slots-list regs-list closed (entry-poll) #f)))
(define (get-var opnd) (set-bbv-version-limit! #f) 
  (cond ((glo? opnd) (env-lookup-global-var *global-env* (glo-name opnd)))
        ((reg? opnd) (Slist-ref regs (reg-num opnd)))
        ((stk? opnd) (Slist-ref slots (SFX- nb-slots (stk-num opnd))))
        (else
         (compiler-internal-error
          "get-var, location must be global, register or stack slot"))))
(define (put-var opnd new) (set-bbv-version-limit! #f) 
  (define (put-v opnd new) (set-bbv-version-limit! #f) 
    (cond ((reg? opnd) (set! regs (replace-nth regs (reg-num opnd) new)))
          ((stk? opnd)
           (set! slots (replace-nth slots (SFX- nb-slots (stk-num opnd)) new)))
          (else
           (compiler-internal-error
            "put-var, location must be register or stack slot, for var:"
            (var-name new)))))
  (if (eq? new ret-var)
      (let ((x (var->opnd ret-var))) (and x (put-v x empty-var))))
  (put-v opnd new))
(define (flush-regs) (set-bbv-version-limit! #f)  (set! regs '()))
(define (push-slot) (set-bbv-version-limit! #f) 
  (set! nb-slots (SFX+ nb-slots 1))
  (set! slots (cons empty-var slots)))
(define (dealloc-slots n) (set-bbv-version-limit! #f) 
  (set! nb-slots (SFX- nb-slots n))
  (set! slots (nth-after slots n)))
(define (pop-slot) (set-bbv-version-limit! #f)  (dealloc-slots 1))
(define (replace-nth l i v) (set-bbv-version-limit! #f) 
  (if (null? l)
      (if (SFX= i 0) (list v) (cons empty-var (replace-nth l (SFX- i 1) v)))
      (if (SFX= i 0)
          (cons v (Scdr l))
          (cons (Scar l) (replace-nth (Scdr l) (SFX- i 1) v)))))
(define (live-vars live) (set-bbv-version-limit! #f) 
  (if (not (set-empty? (set-intersection live (list->set closed))))
      (set-adjoin live closure-env-var)
      live))
(define (dead-slots live) (set-bbv-version-limit! #f) 
  (let ((live-v (live-vars live)))
    (define (loop s l i) (set-bbv-version-limit! #f) 
      (cond ((null? l) (list->set (Sreverse s)))
            ((set-member? (Scar l) live-v) (loop s (Scdr l) (SFX- i 1)))
            (else (loop (cons i s) (Scdr l) (SFX- i 1)))))
    (loop '() slots nb-slots)))
(define (live-slots live) (set-bbv-version-limit! #f) 
  (let ((live-v (live-vars live)))
    (define (loop s l i) (set-bbv-version-limit! #f) 
      (cond ((null? l) (list->set (Sreverse s)))
            ((set-member? (Scar l) live-v) (loop (cons i s) (Scdr l) (SFX- i 1)))
            (else (loop s (Scdr l) (SFX- i 1)))))
    (loop '() slots nb-slots)))
(define (dead-regs live) (set-bbv-version-limit! #f) 
  (let ((live-v (live-vars live)))
    (define (loop s l i) (set-bbv-version-limit! #f) 
      (cond ((SFX>= i target.nb-regs) (list->set (Sreverse s)))
            ((null? l) (loop (cons i s) l (SFX+ i 1)))
            ((and (set-member? (Scar l) live-v) (not (Smemq (Scar l) slots)))
             (loop s (Scdr l) (SFX+ i 1)))
            (else (loop (cons i s) (Scdr l) (SFX+ i 1)))))
    (loop '() regs 0)))
(define (live-regs live) (set-bbv-version-limit! #f) 
  (let ((live-v (live-vars live)))
    (define (loop s l i) (set-bbv-version-limit! #f) 
      (cond ((null? l) (list->set (Sreverse s)))
            ((and (set-member? (Scar l) live-v) (not (Smemq (Scar l) slots)))
             (loop (cons i s) (Scdr l) (SFX+ i 1)))
            (else (loop s (Scdr l) (SFX+ i 1)))))
    (loop '() regs 0)))
(define (lowest-dead-slot live) (set-bbv-version-limit! #f) 
  (make-stk (or (lowest (dead-slots live)) (SFX+ nb-slots 1))))
(define (highest-live-slot live) (set-bbv-version-limit! #f)  (make-stk (or (highest (live-slots live)) 0)))
(define (lowest-dead-reg live) (set-bbv-version-limit! #f) 
  (let ((x (lowest (set-remove (dead-regs live) 0)))) (if x (make-reg x) #f)))
(define (highest-dead-reg live) (set-bbv-version-limit! #f) 
  (let ((x (highest (dead-regs live)))) (if x (make-reg x) #f)))
(define (highest set) (set-bbv-version-limit! #f)  (if (set-empty? set) #f (apply max (set->list set))))
(define (lowest set) (set-bbv-version-limit! #f)  (if (set-empty? set) #f (apply min (set->list set))))
(define (above set n) (set-bbv-version-limit! #f)  (set-keep (lambda (x) (set-bbv-version-limit! #f)  (SFX> x n)) set))
(define (below set n) (set-bbv-version-limit! #f)  (set-keep (lambda (x) (set-bbv-version-limit! #f)  (SFX< x n)) set))
(define (var->opnd var) (set-bbv-version-limit! #f) 
  (let ((x (Sassq var constant-vars)))
    (if x
        (Scdr x)
        (if (global? var)
            (make-glo (var-name var))
            (let ((n (pos-in-list var regs)))
              (if n
                  (make-reg n)
                  (let ((n (pos-in-list var slots)))
                    (if n
                        (make-stk (SFX- nb-slots n))
                        (let ((n (pos-in-list var closed)))
                          (if n
                              (make-clo (var->opnd closure-env-var) (SFX+ n 1))
                              (compiler-internal-error
                               "var->opnd, variable is not accessible:"
                               (var-name var))))))))))))
(define (source-comment node) (set-bbv-version-limit! #f) 
  (let ((x (make-comment))) (comment-put! x 'source (node-source node)) x))
(define (sort-variables lst) (set-bbv-version-limit! #f) 
  (sort-list
   lst
   (lambda (x y) (set-bbv-version-limit! #f) 
     (Sstring<? (Ssymbol->string (var-name x)) (Ssymbol->string (var-name y))))))
(define (add-constant-var var opnd) (set-bbv-version-limit! #f) 
  (set! constant-vars (cons (cons var opnd) constant-vars)))
(define (let-constant-var var opnd thunk) (set-bbv-version-limit! #f) 
  (let* ((x (Sassq var constant-vars)) (temp (Scdr x)))
    (Sset-cdr! x opnd)
    (thunk)
    (Sset-cdr! x temp)))
(define (constant-var? var) (set-bbv-version-limit! #f)  (Sassq var constant-vars))
(define (not-constant-var? var) (set-bbv-version-limit! #f)  (not (constant-var? var)))
(define (add-known-proc label proc) (set-bbv-version-limit! #f) 
  (set! known-procs (cons (cons label proc) known-procs)))
(define (gen-proc proc bb context info-port) (set-bbv-version-limit! #f) 
  (trace-indent info-port)
  (if info-port
      (if (prc-name proc)
          (display (prc-name proc) info-port)
          (display "\"unknown\"" info-port)))
  (let ((lbl (bb-lbl-num bb))
        (live (set-union (free-variables (prc-body proc)) ret-var-set)))
    (set! *bb* bb)
    (restore-context context)
    (gen-node (prc-body proc) ret-var-set 'tail)))
(define (schedule-gen-proc proc closed-list) (set-bbv-version-limit! #f) 
  (let* ((lbl1 (bbs-new-lbl! *bbs*))
         (lbl2 (bbs-new-lbl! *bbs*))
         (context (entry-context proc closed-list))
         (frame (context->frame
                 context
                 (set-union (free-variables (prc-body proc)) ret-var-set)))
         (bb1 (make-bb (make-label-entry
                        lbl1
                        (Slength (prc-parms proc))
                        (prc-min proc)
                        (prc-rest proc)
                        (not (null? closed-list))
                        frame
                        (source-comment proc))
                       *bbs*))
         (bb2 (make-bb (make-label-simple lbl2 frame (source-comment proc))
                       *bbs*)))
    (context-entry-bb-set! context bb1)
    (bb-put-branch! bb1 (make-jump (make-lbl lbl2) #f #f frame #f))
    (set! proc-queue (cons (list proc bb2 context) proc-queue))
    (make-lbl lbl1)))
(define (gen-node node live why) (set-bbv-version-limit! #f) 
  (cond ((cst? node) (gen-return (make-obj (cst-val node)) why node))
        ((ref? node)
         (let* ((var (ref-var node)) (name (var-name var)))
           (gen-return
            (cond ((eq? why 'side) (make-obj undef-object))
                  ((global? var)
                   (let ((prim (target.prim-info* name (node-decl node))))
                     (if prim (make-obj prim) (var->opnd var))))
                  (else (var->opnd var)))
            why
            node)))
        ((set? node)
         (let* ((src (gen-node
                      (set-val node)
                      (set-adjoin live (set-var node))
                      'keep))
                (dst (var->opnd (set-var node))))
           (put-copy src dst #f live (source-comment node))
           (gen-return (make-obj undef-object) why node)))
        ((def? node)
         (compiler-internal-error
          "gen-node, 'def' node not at root of parse tree"))
        ((tst? node) (gen-tst node live why))
        ((conj? node) (gen-conj/disj node live why))
        ((disj? node) (gen-conj/disj node live why))
        ((prc? node)
         (let* ((closed (not-constant-closed-vars node))
                (closed-list (sort-variables (set->list closed)))
                (proc-lbl (schedule-gen-proc node closed-list)))
           (let ((opnd (if (null? closed-list)
                           (begin
                             (add-known-proc (lbl-num proc-lbl) node)
                             proc-lbl)
                           (begin
                             (dealloc-slots
                              (SFX- nb-slots
                                 (stk-num (highest-live-slot
                                           (set-union closed live)))))
                             (push-slot)
                             (let ((slot (make-stk nb-slots))
                                   (var (make-temp-var 'closure)))
                               (put-var slot var)
                               (bb-put-non-branch!
                                *bb*
                                (make-close
                                 (list (make-closure-parms
                                        slot
                                        (lbl-num proc-lbl)
                                        (Smap2 var->opnd closed-list)))
                                 (current-frame (set-adjoin live var))
                                 (source-comment node)))
                               slot)))))
             (gen-return opnd why node))))
        ((app? node) (gen-call node live why))
        ((fut? node) (gen-fut node live why))
        (else
         (compiler-internal-error
          "gen-node, unknown parse tree node type:"
          node))))
(define (gen-return opnd why node) (set-bbv-version-limit! #f) 
  (cond ((eq? why 'tail)
         (let ((var (make-temp-var 'result)))
           (put-copy
            opnd
            target.proc-result
            var
            ret-var-set
            (source-comment node))
           (let ((ret-opnd (var->opnd ret-var)))
             (seal-bb (intrs-enabled? (node-decl node)) 'return)
             (dealloc-slots nb-slots)
             (bb-put-branch!
              *bb*
              (make-jump
               ret-opnd
               #f
               #f
               (current-frame (set-singleton var))
               #f)))))
        (else opnd)))
(define (not-constant-closed-vars val) (set-bbv-version-limit! #f) 
  (set-keep not-constant-var? (free-variables val)))
(define (predicate node live cont) (set-bbv-version-limit! #f) 
  (define (cont* true-lbl false-lbl) (set-bbv-version-limit! #f)  (cont false-lbl true-lbl))
  (define (generic-true-test) (set-bbv-version-limit! #f) 
    (predicate-test node live **not-proc-obj '0 (list node) cont*))
  (cond ((or (conj? node) (disj? node)) (predicate-conj/disj node live cont))
        ((app? node)
         (let ((proc (node->proc (app-oper node))))
           (if proc
               (let ((spec (specialize-for-call proc (node-decl node))))
                 (if (and (proc-obj-test spec)
                          (nb-args-conforms?
                           (Slength (app-args node))
                           (proc-obj-call-pat spec)))
                     (if (eq? spec **not-proc-obj)
                         (predicate (Scar (app-args node)) live cont*)
                         (predicate-test
                          node
                          live
                          spec
                          (proc-obj-strict-pat proc)
                          (app-args node)
                          cont))
                     (generic-true-test)))
               (generic-true-test))))
        (else (generic-true-test))))
(define (predicate-conj/disj node live cont) (set-bbv-version-limit! #f) 
  (let* ((pre (if (conj? node) (conj-pre node) (disj-pre node)))
         (alt (if (conj? node) (conj-alt node) (disj-alt node)))
         (alt-live (set-union live (free-variables alt))))
    (predicate
     pre
     alt-live
     (lambda (true-lbl false-lbl) (set-bbv-version-limit! #f) 
       (let ((pre-context (current-context)))
         (set! *bb*
               (make-bb (make-label-simple
                         (if (conj? node) true-lbl false-lbl)
                         (current-frame alt-live)
                         (source-comment alt))
                        *bbs*))
         (predicate
          alt
          live
          (lambda (true-lbl2 false-lbl2) (set-bbv-version-limit! #f) 
            (let ((alt-context (current-context)))
              (restore-context pre-context)
              (set! *bb*
                    (make-bb (make-label-simple
                              (if (conj? node) false-lbl true-lbl)
                              (current-frame live)
                              (source-comment alt))
                             *bbs*))
              (merge-contexts-and-seal-bb
               alt-context
               live
               (intrs-enabled? (node-decl node))
               'internal
               (source-comment node))
              (bb-put-branch!
               *bb*
               (make-jump
                (make-lbl (if (conj? node) false-lbl2 true-lbl2))
                #f
                #f
                (current-frame live)
                #f))
              (cont true-lbl2 false-lbl2)))))))))
(define (predicate-test node live test strict-pat args cont) (set-bbv-version-limit! #f) 
  (let loop ((args* args) (liv live) (vars* '()))
    (if (not (null? args*))
        (let* ((needed (vals-live-vars liv (Scdr args*)))
               (var (save-var
                     (gen-node (Scar args*) needed 'need)
                     (make-temp-var 'predicate)
                     needed
                     (source-comment (Scar args*)))))
          (loop (Scdr args*) (set-adjoin liv var) (cons var vars*)))
        (let* ((true-lbl (bbs-new-lbl! *bbs*))
               (false-lbl (bbs-new-lbl! *bbs*)))
          (seal-bb (intrs-enabled? (node-decl node)) 'internal)
          (bb-put-branch!
           *bb*
           (make-ifjump
            test
            (Smap2 var->opnd (Sreverse vars*))
            true-lbl
            false-lbl
            #f
            (current-frame live)
            (source-comment node)))
          (cont true-lbl false-lbl)))))
(define (gen-tst node live why) (set-bbv-version-limit! #f) 
  (let ((pre (tst-pre node)) (con (tst-con node)) (alt (tst-alt node)))
    (predicate
     pre
     (set-union live (free-variables con) (free-variables alt))
     (lambda (true-lbl false-lbl) (set-bbv-version-limit! #f) 
       (let ((pre-context (current-context))
             (true-bb (make-bb (make-label-simple
                                true-lbl
                                (current-frame
                                 (set-union live (free-variables con)))
                                (source-comment con))
                               *bbs*))
             (false-bb
              (make-bb (make-label-simple
                        false-lbl
                        (current-frame (set-union live (free-variables alt)))
                        (source-comment alt))
                       *bbs*)))
         (set! *bb* true-bb)
         (let ((con-opnd (gen-node con live why)))
           (if (eq? why 'tail)
               (begin
                 (restore-context pre-context)
                 (set! *bb* false-bb)
                 (gen-node alt live why))
               (let* ((result-var (make-temp-var 'result))
                      (live-after (set-adjoin live result-var)))
                 (save-opnd-to-reg
                  con-opnd
                  target.proc-result
                  result-var
                  live
                  (source-comment con))
                 (let ((con-context (current-context)) (con-bb *bb*))
                   (restore-context pre-context)
                   (set! *bb* false-bb)
                   (save-opnd-to-reg
                    (gen-node alt live why)
                    target.proc-result
                    result-var
                    live
                    (source-comment alt))
                   (let ((next-lbl (bbs-new-lbl! *bbs*)) (alt-bb *bb*))
                     (if (SFX> (context-nb-slots con-context) nb-slots)
                         (begin
                           (seal-bb (intrs-enabled? (node-decl node))
                                    'internal)
                           (let ((alt-context (current-context)))
                             (restore-context con-context)
                             (set! *bb* con-bb)
                             (merge-contexts-and-seal-bb
                              alt-context
                              live-after
                              (intrs-enabled? (node-decl node))
                              'internal
                              (source-comment node))))
                         (let ((alt-context (current-context)))
                           (restore-context con-context)
                           (set! *bb* con-bb)
                           (seal-bb (intrs-enabled? (node-decl node))
                                    'internal)
                           (let ((con-context* (current-context)))
                             (restore-context alt-context)
                             (set! *bb* alt-bb)
                             (merge-contexts-and-seal-bb
                              con-context*
                              live-after
                              (intrs-enabled? (node-decl node))
                              'internal
                              (source-comment node)))))
                     (let ((frame (current-frame live-after)))
                       (bb-put-branch!
                        con-bb
                        (make-jump (make-lbl next-lbl) #f #f frame #f))
                       (bb-put-branch!
                        alt-bb
                        (make-jump (make-lbl next-lbl) #f #f frame #f))
                       (set! *bb*
                             (make-bb (make-label-simple
                                       next-lbl
                                       frame
                                       (source-comment node))
                                      *bbs*))
                       target.proc-result)))))))))))
(define (nb-args-conforms? n call-pat) (set-bbv-version-limit! #f)  (pattern-member? n call-pat))
(define (merge-contexts-and-seal-bb other-context live poll? where comment) (set-bbv-version-limit! #f) 
  (let ((live-v (live-vars live))
        (other-nb-slots (context-nb-slots other-context))
        (other-regs (context-regs other-context))
        (other-slots (context-slots other-context))
        (other-poll (context-poll other-context))
        (other-entry-bb (context-entry-bb other-context)))
    (let loop1 ((i (SFX- target.nb-regs 1)))
      (if (SFX>= i 0)
          (let ((other-var (reg->var other-regs i)) (var (reg->var regs i)))
            (if (and (not (eq? var other-var)) (set-member? other-var live-v))
                (let ((r (make-reg i)))
                  (put-var r empty-var)
                  (if (not (or (not (set-member? var live-v))
                               (Smemq var regs)
                               (Smemq var slots)))
                      (let ((top (make-stk (SFX+ nb-slots 1))))
                        (put-copy r top var live-v comment)))
                  (put-copy (var->opnd other-var) r other-var live-v comment)))
            (loop1 (SFX- i 1)))))
    (let loop2 ((i 1))
      (if (SFX<= i other-nb-slots)
          (let ((other-var (stk->var other-slots i)) (var (stk->var slots i)))
            (if (and (not (eq? var other-var)) (set-member? other-var live-v))
                (let ((s (make-stk i)))
                  (if (SFX<= i nb-slots) (put-var s empty-var))
                  (if (not (or (not (set-member? var live-v))
                               (Smemq var regs)
                               (Smemq var slots)))
                      (let ((top (make-stk (SFX+ nb-slots 1))))
                        (put-copy s top var live-v comment)))
                  (put-copy (var->opnd other-var) s other-var live-v comment))
                (if (SFX> i nb-slots)
                    (let ((top (make-stk (SFX+ nb-slots 1))))
                      (put-copy
                       (make-obj undef-object)
                       top
                       empty-var
                       live-v
                       comment))))
            (loop2 (SFX+ i 1)))))
    (dealloc-slots (SFX- nb-slots other-nb-slots))
    (let loop3 ((i (SFX- target.nb-regs 1)))
      (if (SFX>= i 0)
          (let ((other-var (reg->var other-regs i)) (var (reg->var regs i)))
            (if (not (eq? var other-var)) (put-var (make-reg i) empty-var))
            (loop3 (SFX- i 1)))))
    (let loop4 ((i 1))
      (if (SFX<= i other-nb-slots)
          (let ((other-var (stk->var other-slots i)) (var (stk->var slots i)))
            (if (not (eq? var other-var)) (put-var (make-stk i) empty-var))
            (loop4 (SFX+ i 1)))))
    (seal-bb poll? where)
    (set! poll (poll-merge poll other-poll))
    (if (not (eq? entry-bb other-entry-bb))
        (compiler-internal-error
         "merge-contexts-and-seal-bb, entry-bb's do not agree"))))
(define (seal-bb poll? where) (set-bbv-version-limit! #f) 
  (define (my-last-pair l) (set-bbv-version-limit! #f)  (if (pair? (Scdr l)) (my-last-pair (Scdr l)) l))
  (define (poll-at split-point) (set-bbv-version-limit! #f) 
    (let loop ((i 0) (l1 (bb-non-branch-instrs *bb*)) (l2 '()))
      (if (SFX< i split-point)
          (loop (SFX+ i 1) (Scdr l1) (cons (Scar l1) l2))
          (let* ((label-instr (bb-label-instr *bb*))
                 (non-branch-instrs1 (Sreverse l2))
                 (non-branch-instrs2 l1)
                 (frame (gvm-instr-frame
                         (Scar (my-last-pair
                               (cons label-instr non-branch-instrs1)))))
                 (prec-bb (make-bb label-instr *bbs*))
                 (new-lbl (bbs-new-lbl! *bbs*)))
            (bb-non-branch-instrs-set! prec-bb non-branch-instrs1)
            (bb-put-branch!
             prec-bb
             (make-jump (make-lbl new-lbl) #f #t frame #f))
            (bb-label-instr-set! *bb* (make-label-simple new-lbl frame #f))
            (bb-non-branch-instrs-set! *bb* non-branch-instrs2)
            (set! poll (make-poll #t 0))))))
  (define (poll-at-end) (set-bbv-version-limit! #f)  (poll-at (Slength (bb-non-branch-instrs *bb*))))
  (define (impose-polling-constraints) (set-bbv-version-limit! #f) 
    (let ((n (SFX+ (Slength (bb-non-branch-instrs *bb*)) 1))
          (delta (poll-delta poll)))
      (if (SFX> (SFX+ delta n) poll-period)
          (begin
            (poll-at (max (SFX- poll-period delta) 0))
            (impose-polling-constraints)))))
  (if poll? (impose-polling-constraints))
  (let* ((n (SFX+ (Slength (bb-non-branch-instrs *bb*)) 1))
         (delta (SFX+ (poll-delta poll) n))
         (since-entry? (poll-since-entry? poll)))
    (if (and poll?
             (case where
               ((call) (SFX> delta (SFX- poll-period poll-head)))
               ((tail-call) (SFX> delta poll-tail))
               ((return) (and since-entry? (SFX> delta (SFX+ poll-head poll-tail))))
               ((internal) #f)
               (else
                (compiler-internal-error "seal-bb, unknown 'where':" where))))
        (poll-at-end)
        (set! poll (make-poll since-entry? delta)))))
(define (reg->var regs i) (set-bbv-version-limit! #f) 
  (cond ((null? regs) '())
        ((SFX> i 0) (reg->var (Scdr regs) (SFX- i 1)))
        (else (Scar regs))))
(define (stk->var slots i) (set-bbv-version-limit! #f) 
  (let ((j (SFX- (Slength slots) i))) (if (SFX< j 0) '() (Slist-ref slots j))))
(define (gen-conj/disj node live why) (set-bbv-version-limit! #f) 
  (let ((pre (if (conj? node) (conj-pre node) (disj-pre node)))
        (alt (if (conj? node) (conj-alt node) (disj-alt node))))
    (let ((needed (set-union live (free-variables alt)))
          (bool? (boolean-value? pre))
          (predicate-var (make-temp-var 'predicate)))
      (define (general-predicate node live cont) (set-bbv-version-limit! #f) 
        (let* ((con-lbl (bbs-new-lbl! *bbs*)) (alt-lbl (bbs-new-lbl! *bbs*)))
          (save-opnd-to-reg
           (gen-node pre live 'need)
           target.proc-result
           predicate-var
           live
           (source-comment pre))
          (seal-bb (intrs-enabled? (node-decl node)) 'internal)
          (bb-put-branch!
           *bb*
           (make-ifjump
            **not-proc-obj
            (list target.proc-result)
            alt-lbl
            con-lbl
            #f
            (current-frame (set-adjoin live predicate-var))
            (source-comment node)))
          (cont con-lbl alt-lbl)))
      (define (alternative con-lbl alt-lbl) (set-bbv-version-limit! #f) 
        (let* ((pre-context (current-context))
               (result-var (make-temp-var 'result))
               (con-live (if bool? live (set-adjoin live predicate-var)))
               (alt-live (set-union live (free-variables alt)))
               (con-bb (make-bb (make-label-simple
                                 con-lbl
                                 (current-frame con-live)
                                 (source-comment alt))
                                *bbs*))
               (alt-bb (make-bb (make-label-simple
                                 alt-lbl
                                 (current-frame alt-live)
                                 (source-comment alt))
                                *bbs*)))
          (if bool?
              (begin
                (set! *bb* con-bb)
                (save-opnd-to-reg
                 (make-obj (if (conj? node) false-object #t))
                 target.proc-result
                 result-var
                 live
                 (source-comment node)))
              (put-var (var->opnd predicate-var) result-var))
          (let ((con-context (current-context)))
            (set! *bb* alt-bb)
            (restore-context pre-context)
            (let ((alt-opnd (gen-node alt live why)))
              (if (eq? why 'tail)
                  (begin
                    (restore-context con-context)
                    (set! *bb* con-bb)
                    (let ((ret-opnd (var->opnd ret-var))
                          (result-set (set-singleton result-var)))
                      (seal-bb (intrs-enabled? (node-decl node)) 'return)
                      (dealloc-slots nb-slots)
                      (bb-put-branch!
                       *bb*
                       (make-jump
                        ret-opnd
                        #f
                        #f
                        (current-frame result-set)
                        #f))))
                  (let ((alt-context* (current-context)) (alt-bb* *bb*))
                    (restore-context con-context)
                    (set! *bb* con-bb)
                    (seal-bb (intrs-enabled? (node-decl node)) 'internal)
                    (let ((con-context* (current-context))
                          (next-lbl (bbs-new-lbl! *bbs*)))
                      (restore-context alt-context*)
                      (set! *bb* alt-bb*)
                      (save-opnd-to-reg
                       alt-opnd
                       target.proc-result
                       result-var
                       live
                       (source-comment alt))
                      (merge-contexts-and-seal-bb
                       con-context*
                       (set-adjoin live result-var)
                       (intrs-enabled? (node-decl node))
                       'internal
                       (source-comment node))
                      (let ((frame (current-frame
                                    (set-adjoin live result-var))))
                        (bb-put-branch!
                         *bb*
                         (make-jump (make-lbl next-lbl) #f #f frame #f))
                        (bb-put-branch!
                         con-bb
                         (make-jump (make-lbl next-lbl) #f #f frame #f))
                        (set! *bb*
                              (make-bb (make-label-simple
                                        next-lbl
                                        frame
                                        (source-comment node))
                                       *bbs*))
                        target.proc-result))))))))
      ((if bool? predicate general-predicate)
       pre
       needed
       (lambda (true-lbl false-lbl) (set-bbv-version-limit! #f) 
         (if (conj? node)
             (alternative false-lbl true-lbl)
             (alternative true-lbl false-lbl)))))))
(define (gen-call node live why) (set-bbv-version-limit! #f) 
  (let* ((oper (app-oper node)) (args (app-args node)) (nb-args (Slength args)))
    (if (and (prc? oper)
             (not (prc-rest oper))
             (SFX= (Slength (prc-parms oper)) nb-args))
        (gen-let (prc-parms oper) args (prc-body oper) live why)
        (if (inlinable-app? node)
            (let ((eval-order (arg-eval-order #f args))
                  (vars (Smap2 (lambda (x) (set-bbv-version-limit! #f)  (cons x #f)) args)))
              (let loop ((l eval-order) (liv live))
                (if (not (null? l))
                    (let* ((needed (vals-live-vars liv (Smap2 (lambda (x) (set-bbv-version-limit! #f)  (Scar x)) (Scdr l))))
                           (arg (Scar (Scar l)))
                           (pos (Scdr (Scar l)))
                           (var (save-var
                                 (gen-node arg needed 'need)
                                 (make-temp-var pos)
                                 needed
                                 (source-comment arg))))
                      (Sset-cdr! (Sassq arg vars) var)
                      (loop (Scdr l) (set-adjoin liv var)))
                    (let ((loc (if (eq? why 'side)
                                   (make-reg 0)
                                   (or (lowest-dead-reg live)
                                       (lowest-dead-slot live)))))
                      (if (and (stk? loc) (SFX> (stk-num loc) nb-slots))
                          (push-slot))
                      (let* ((args (Smap2 var->opnd (Smap2 (lambda (x) (set-bbv-version-limit! #f)  (Scdr x)) vars)))
                             (var (make-temp-var 'result))
                             (proc (node->proc oper))
                             (strict-pat (proc-obj-strict-pat proc)))
                        (if (not (eq? why 'side)) (put-var loc var))
                        (bb-put-non-branch!
                         *bb*
                         (make-apply
                          (specialize-for-call proc (node-decl node))
                          args
                          (if (eq? why 'side) #f loc)
                          (current-frame
                           (if (eq? why 'side) live (set-adjoin live var)))
                          (source-comment node)))
                        (gen-return loc why node))))))
            (let* ((calling-local-proc?
                    (and (ref? oper)
                         (let ((opnd (var->opnd (ref-var oper))))
                           (and (lbl? opnd)
                                (let ((x (Sassq (lbl-num opnd) known-procs)))
                                  (and x
                                       (let ((proc (Scdr x)))
                                         (and (not (prc-rest proc))
                                              (SFX= (prc-min proc) nb-args)
                                              (SFX= (Slength (prc-parms proc))
                                                 nb-args)
                                              (lbl-num opnd)))))))))
                   (jstate (get-jump-state
                            args
                            (if calling-local-proc?
                                (target.label-info nb-args nb-args #f #f)
                                (target.jump-info nb-args))))
                   (in-stk (jump-state-in-stk jstate))
                   (in-reg (jump-state-in-reg jstate))
                   (eval-order
                    (arg-eval-order (if calling-local-proc? #f oper) in-reg))
                   (live-after
                    (if (eq? why 'tail) (set-remove live ret-var) live))
                   (live-for-regs (args-live-vars live eval-order))
                   (return-lbl (if (eq? why 'tail) #f (bbs-new-lbl! *bbs*))))
              (save-regs
               (live-regs live-after)
               (stk-live-vars live-for-regs in-stk why)
               (source-comment node))
              (let ((frame-start (stk-num (highest-live-slot live-after))))
                (let loop1 ((l in-stk) (liv live-after) (i (SFX+ frame-start 1)))
                  (if (not (null? l))
                      (let ((arg (Scar l))
                            (slot (make-stk i))
                            (needed (set-union
                                     (stk-live-vars liv (Scdr l) why)
                                     live-for-regs)))
                        (if arg
                            (let ((var (if (and (eq? arg 'return)
                                                (eq? why 'tail))
                                           ret-var
                                           (make-temp-var (SFX- frame-start i)))))
                              (save-opnd-to-stk
                               (if (eq? arg 'return)
                                   (if (eq? why 'tail)
                                       (var->opnd ret-var)
                                       (make-lbl return-lbl))
                                   (gen-node arg needed 'need))
                               slot
                               var
                               needed
                               (source-comment
                                (if (eq? arg 'return) node arg)))
                              (loop1 (Scdr l) (set-adjoin liv var) (SFX+ i 1)))
                            (begin
                              (if (SFX> i nb-slots)
                                  (put-copy
                                   (make-obj undef-object)
                                   slot
                                   empty-var
                                   liv
                                   (source-comment node)))
                              (loop1 (Scdr l) liv (SFX+ i 1)))))
                      (let loop2 ((l eval-order)
                                  (liv liv)
                                  (reg-map '())
                                  (oper-var '()))
                        (if (not (null? l))
                            (let* ((arg (Scar (Scar l)))
                                   (pos (Scdr (Scar l)))
                                   (needed (args-live-vars liv (Scdr l)))
                                   (var (if (and (eq? arg 'return)
                                                 (eq? why 'tail))
                                            ret-var
                                            (make-temp-var pos)))
                                   (opnd (if (eq? arg 'return)
                                             (if (eq? why 'tail)
                                                 (var->opnd ret-var)
                                                 (make-lbl return-lbl))
                                             (gen-node arg needed 'need))))
                              (if (eq? pos 'operator)
                                  (if (and (ref? arg)
                                           (not (or (obj? opnd) (lbl? opnd))))
                                      (loop2 (Scdr l)
                                             (set-adjoin liv (ref-var arg))
                                             reg-map
                                             (ref-var arg))
                                      (begin
                                        (save-arg
                                         opnd
                                         var
                                         needed
                                         (source-comment
                                          (if (eq? arg 'return) node arg)))
                                        (loop2 (Scdr l)
                                               (set-adjoin liv var)
                                               reg-map
                                               var)))
                                  (let ((reg (make-reg pos)))
                                    (if (all-args-trivial? (Scdr l))
                                        (save-opnd-to-reg
                                         opnd
                                         reg
                                         var
                                         needed
                                         (source-comment
                                          (if (eq? arg 'return) node arg)))
                                        (save-in-slot
                                         opnd
                                         var
                                         needed
                                         (source-comment
                                          (if (eq? arg 'return) node arg))))
                                    (loop2 (Scdr l)
                                           (set-adjoin liv var)
                                           (cons (cons pos var) reg-map)
                                           oper-var))))
                            (let loop3 ((i (SFX- target.nb-regs 1)))
                              (if (SFX>= i 0)
                                  (let ((couple (Sassq i reg-map)))
                                    (if couple
                                        (let ((var (Scdr couple)))
                                          (if (not (eq? (reg->var regs i) var))
                                              (save-opnd-to-reg
                                               (var->opnd var)
                                               (make-reg i)
                                               var
                                               liv
                                               (source-comment node)))))
                                    (loop3 (SFX- i 1)))
                                  (let ((opnd (if calling-local-proc?
                                                  (make-lbl
                                                   (SFX+ calling-local-proc? 1))
                                                  (var->opnd oper-var))))
                                    (seal-bb (intrs-enabled? (node-decl node))
                                             (if return-lbl 'call 'tail-call))
                                    (dealloc-slots
                                     (SFX- nb-slots
                                        (SFX+ frame-start (Slength in-stk))))
                                    (bb-put-branch!
                                     *bb*
                                     (make-jump
                                      opnd
                                      (if calling-local-proc? #f nb-args)
                                      #f
                                      (current-frame liv)
                                      (source-comment node)))
                                    (let ((result-var (make-temp-var 'result)))
                                      (dealloc-slots (SFX- nb-slots frame-start))
                                      (flush-regs)
                                      (put-var target.proc-result result-var)
                                      (if return-lbl
                                          (begin
                                            (set! poll (return-poll poll))
                                            (set! *bb*
                                                  (make-bb (make-label-return
                                                            return-lbl
                                                            (current-frame
                                                             (set-adjoin
                                                              live
                                                              result-var))
                                                            (source-comment
                                                             node))
                                                           *bbs*))))
                                      target.proc-result))))))))))))))
(define (contained-reg/slot opnd) (set-bbv-version-limit! #f) 
  (cond ((reg? opnd) opnd)
        ((stk? opnd) opnd)
        ((clo? opnd) (contained-reg/slot (clo-base opnd)))
        (else #f)))
(define (opnd-needed opnd needed) (set-bbv-version-limit! #f) 
  (let ((x (contained-reg/slot opnd)))
    (if x (set-adjoin needed (get-var x)) needed)))
(define (save-opnd opnd live comment) (set-bbv-version-limit! #f) 
  (let ((slot (lowest-dead-slot live)))
    (put-copy opnd slot (get-var opnd) live comment)))
(define (save-regs regs live comment) (set-bbv-version-limit! #f) 
  (for-each
   (lambda (i) (set-bbv-version-limit! #f)  (save-opnd (make-reg i) live comment))
   (set->list regs)))
(define (save-opnd-to-reg opnd reg var live comment) (set-bbv-version-limit! #f) 
  (if (set-member? (reg-num reg) (live-regs live))
      (save-opnd reg (opnd-needed opnd live) comment))
  (put-copy opnd reg var live comment))
(define (save-opnd-to-stk opnd stk var live comment) (set-bbv-version-limit! #f) 
  (if (set-member? (stk-num stk) (live-slots live))
      (save-opnd stk (opnd-needed opnd live) comment))
  (put-copy opnd stk var live comment))
(define (all-args-trivial? l) (set-bbv-version-limit! #f) 
  (if (null? l)
      #t
      (let ((arg (Scar (Scar l))))
        (or (eq? arg 'return)
            (and (trivial? arg) (all-args-trivial? (Scdr l)))))))
(define (every-trivial? l) (set-bbv-version-limit! #f) 
  (or (null? l) (and (trivial? (Scar l)) (every-trivial? (Scdr l)))))
(define (trivial? node) (set-bbv-version-limit! #f) 
  (or (cst? node)
      (ref? node)
      (and (set? node) (trivial? (set-val node)))
      (and (inlinable-app? node) (every-trivial? (app-args node)))))
(define (inlinable-app? node) (set-bbv-version-limit! #f) 
  (if (app? node)
      (let ((proc (node->proc (app-oper node))))
        (and proc
             (let ((spec (specialize-for-call proc (node-decl node))))
               (and (proc-obj-inlinable spec)
                    (nb-args-conforms?
                     (Slength (app-args node))
                     (proc-obj-call-pat spec))))))
      #f))
(define (boolean-value? node) (set-bbv-version-limit! #f) 
  (or (and (conj? node)
           (boolean-value? (conj-pre node))
           (boolean-value? (conj-alt node)))
      (and (disj? node)
           (boolean-value? (disj-pre node))
           (boolean-value? (disj-alt node)))
      (boolean-app? node)))
(define (boolean-app? node) (set-bbv-version-limit! #f) 
  (if (app? node)
      (let ((proc (node->proc (app-oper node))))
        (if proc (eq? (type-name (proc-obj-type proc)) 'boolean) #f))
      #f))
(define (node->proc node) (set-bbv-version-limit! #f) 
  (cond ((cst? node) (if (proc-obj? (cst-val node)) (cst-val node) #f))
        ((ref? node)
         (if (global? (ref-var node))
             (target.prim-info* (var-name (ref-var node)) (node-decl node))
             #f))
        (else #f)))
(define (specialize-for-call proc decl) (set-bbv-version-limit! #f)  ((proc-obj-specialize proc) decl))
(define (get-jump-state args pc) (set-bbv-version-limit! #f) 
  (define (empty-node-list n) (set-bbv-version-limit! #f) 
    (if (SFX> n 0) (cons #f (empty-node-list (SFX- n 1))) '()))
  (let* ((fs (pcontext-fs pc))
         (slots-list (empty-node-list fs))
         (regs-list (empty-node-list target.nb-regs)))
    (define (assign-node-to-loc var loc) (set-bbv-version-limit! #f) 
      (let ((x (cond ((reg? loc)
                      (let ((i (reg-num loc)))
                        (if (SFX<= i target.nb-regs)
                            (nth-after regs-list i)
                            (compiler-internal-error
                             "jump-state, reg out of bound in back-end's pcontext"))))
                     ((stk? loc)
                      (let ((i (stk-num loc)))
                        (if (SFX<= i fs)
                            (nth-after slots-list (SFX- i 1))
                            (compiler-internal-error
                             "jump-state, stk out of bound in back-end's pcontext"))))
                     (else
                      (compiler-internal-error
                       "jump-state, loc other than reg or stk in back-end's pcontext")))))
        (if (not (Scar x))
            (Sset-car! x var)
            (compiler-internal-error
             "jump-state, duplicate location in back-end's pcontext"))))
    (let loop ((l (pcontext-map pc)))
      (if (not (null? l))
          (let* ((couple (Scar l)) (name (Scar couple)) (loc (Scdr couple)))
            (cond ((eq? name 'return) (assign-node-to-loc 'return loc))
                  (else (assign-node-to-loc (Slist-ref args (SFX- name 1)) loc)))
            (loop (Scdr l)))))
    (vector slots-list regs-list)))
(define (jump-state-in-stk x) (set-bbv-version-limit! #f)  (Svector-ref x 0))
(define (jump-state-in-reg x) (set-bbv-version-limit! #f)  (Svector-ref x 1))
(define (arg-eval-order oper nodes) (set-bbv-version-limit! #f) 
  (define (loop nodes pos part1 part2) (set-bbv-version-limit! #f) 
    (cond ((null? nodes)
           (let ((p1 (Sreverse part1)) (p2 (free-vars-order part2)))
             (cond ((not oper) (Sappend p1 p2))
                   ((trivial? oper)
                    (Sappend p1 (Sappend p2 (list (cons oper 'operator)))))
                   (else (Sappend (cons (cons oper 'operator) p1) p2)))))
          ((not (Scar nodes)) (loop (Scdr nodes) (SFX+ pos 1) part1 part2))
          ((or (eq? (Scar nodes) 'return) (trivial? (Scar nodes)))
           (loop (Scdr nodes)
                 (SFX+ pos 1)
                 part1
                 (cons (cons (Scar nodes) pos) part2)))
          (else
           (loop (Scdr nodes)
                 (SFX+ pos 1)
                 (cons (cons (Scar nodes) pos) part1)
                 part2))))
  (loop nodes 0 '() '()))
(define (free-vars-order l) (set-bbv-version-limit! #f) 
  (let ((bins '()) (ordered-args '()))
    (define (free-v x) (set-bbv-version-limit! #f)  (if (eq? x 'return) (set-empty) (free-variables x)))
    (define (add-to-bin! x) (set-bbv-version-limit! #f) 
      (let ((y (Sassq x bins)))
        (if y (Sset-cdr! y (SFX+ (Scdr y) 1)) (set! bins (cons (cons x 1) bins)))))
    (define (payoff-if-removed node) (set-bbv-version-limit! #f) 
      (let ((x (free-v node)))
        (let loop ((l (set->list x)) (r 0))
          (if (null? l)
              r
              (let ((y (Scdr (Sassq (Scar l) bins))))
                (loop (Scdr l) (SFX+ r (SFXquotient 1000 (SFX* y y)))))))))
    (define (remove-free-vars! x) (set-bbv-version-limit! #f) 
      (let loop ((l (set->list x)))
        (if (not (null? l))
            (let ((y (Sassq (Scar l) bins)))
              (Sset-cdr! y (SFX- (Scdr y) 1))
              (loop (Scdr l))))))
    (define (find-max-payoff l thunk) (set-bbv-version-limit! #f) 
      (if (null? l)
          (thunk '() -1)
          (find-max-payoff
           (Scdr l)
           (lambda (best-arg best-payoff) (set-bbv-version-limit! #f) 
             (let ((payoff (payoff-if-removed (Scar (Scar l)))))
               (if (SFX>= payoff best-payoff)
                   (thunk (Scar l) payoff)
                   (thunk best-arg best-payoff)))))))
    (define (remove x l) (set-bbv-version-limit! #f) 
      (cond ((null? l) '())
            ((eq? x (Scar l)) (Scdr l))
            (else (cons (Scar l) (remove x (Scdr l))))))
    (for-each
     (lambda (x) (set-bbv-version-limit! #f)  (for-each add-to-bin! (set->list (free-v (Scar x)))))
     l)
    (let loop ((args l) (ordered-args '()))
      (if (null? args)
          (Sreverse ordered-args)
          (find-max-payoff
           args
           (lambda (best-arg best-payoff) (set-bbv-version-limit! #f) 
             (remove-free-vars! (free-v (Scar best-arg)))
             (loop (remove best-arg args) (cons best-arg ordered-args))))))))
(define (args-live-vars live order) (set-bbv-version-limit! #f) 
  (cond ((null? order) live)
        ((eq? (Scar (Scar order)) 'return)
         (args-live-vars (set-adjoin live ret-var) (Scdr order)))
        (else
         (args-live-vars
          (set-union live (free-variables (Scar (Scar order))))
          (Scdr order)))))
(define (stk-live-vars live slots why) (set-bbv-version-limit! #f) 
  (cond ((null? slots) live)
        ((not (Scar slots)) (stk-live-vars live (Scdr slots) why))
        ((eq? (Scar slots) 'return)
         (stk-live-vars
          (if (eq? why 'tail) (set-adjoin live ret-var) live)
          (Scdr slots)
          why))
        (else
         (stk-live-vars
          (set-union live (free-variables (Scar slots)))
          (Scdr slots)
          why))))
(define (gen-let vars vals node live why) (set-bbv-version-limit! #f) 
  (let ((var-val-map (pair-up vars vals))
        (var-set (list->set vars))
        (all-live
         (set-union
          live
          (free-variables node)
          (apply set-union (Smap2 free-variables vals)))))
    (define (var->val var) (set-bbv-version-limit! #f)  (Scdr (Sassq var var-val-map)))
    (define (proc-var? var) (set-bbv-version-limit! #f)  (prc? (var->val var)))
    (define (closed-vars var const-proc-vars) (set-bbv-version-limit! #f) 
      (set-difference
       (not-constant-closed-vars (var->val var))
       const-proc-vars))
    (define (no-closed-vars? var const-proc-vars) (set-bbv-version-limit! #f) 
      (set-empty? (closed-vars var const-proc-vars)))
    (define (closed-vars? var const-proc-vars) (set-bbv-version-limit! #f) 
      (not (no-closed-vars? var const-proc-vars)))
    (define (compute-const-proc-vars proc-vars) (set-bbv-version-limit! #f) 
      (let loop1 ((const-proc-vars proc-vars))
        (let ((new-const-proc-vars
               (set-keep
                (lambda (x) (set-bbv-version-limit! #f)  (no-closed-vars? x const-proc-vars))
                const-proc-vars)))
          (if (not (set-equal? new-const-proc-vars const-proc-vars))
              (loop1 new-const-proc-vars)
              const-proc-vars))))
    (let* ((proc-vars (set-keep proc-var? var-set))
           (const-proc-vars (compute-const-proc-vars proc-vars))
           (clo-vars
            (set-keep (lambda (x) (set-bbv-version-limit! #f)  (closed-vars? x const-proc-vars)) proc-vars))
           (clo-vars-list (set->list clo-vars)))
      (for-each
       (lambda (proc-var) (set-bbv-version-limit! #f) 
         (let ((label (schedule-gen-proc (var->val proc-var) '())))
           (add-known-proc (lbl-num label) (var->val proc-var))
           (add-constant-var proc-var label)))
       (set->list const-proc-vars))
      (let ((non-clo-vars-list
             (set->list
              (set-keep
               (lambda (var) (set-bbv-version-limit! #f) 
                 (and (not (set-member? var const-proc-vars))
                      (not (set-member? var clo-vars))))
               vars)))
            (liv (set-union
                  live
                  (apply set-union
                         (Smap2 (lambda (x) (set-bbv-version-limit! #f)  (closed-vars x const-proc-vars))
                              clo-vars-list))
                  (free-variables node))))
        (let loop2 ((vars* non-clo-vars-list))
          (if (not (null? vars*))
              (let* ((var (Scar vars*))
                     (val (var->val var))
                     (needed (vals-live-vars liv (Smap2 var->val (Scdr vars*)))))
                (if (var-useless? var)
                    (gen-node val needed 'side)
                    (save-val
                     (gen-node val needed 'need)
                     var
                     needed
                     (source-comment val)))
                (loop2 (Scdr vars*)))))
        (if (pair? clo-vars-list)
            (begin
              (dealloc-slots (SFX- nb-slots (stk-num (highest-live-slot liv))))
              (let loop3 ((l clo-vars-list))
                (if (not (null? l))
                    (begin
                      (push-slot)
                      (let ((var (Scar l)) (slot (make-stk nb-slots)))
                        (put-var slot var)
                        (loop3 (Scdr l))))))
              (bb-put-non-branch!
               *bb*
               (make-close
                (Smap2 (lambda (var) (set-bbv-version-limit! #f) 
                       (let ((closed-list
                              (sort-variables
                               (set->list (closed-vars var const-proc-vars)))))
                         (if (null? closed-list)
                             (compiler-internal-error
                              "gen-let, no closed variables:"
                              (var-name var))
                             (make-closure-parms
                              (var->opnd var)
                              (lbl-num (schedule-gen-proc
                                        (var->val var)
                                        closed-list))
                              (Smap2 var->opnd closed-list)))))
                     clo-vars-list)
                (current-frame liv)
                (source-comment node)))))
        (gen-node node live why)))))
(define (save-arg opnd var live comment) (set-bbv-version-limit! #f) 
  (if (glo? opnd)
      (add-constant-var var opnd)
      (save-val opnd var live comment)))
(define (save-val opnd var live comment) (set-bbv-version-limit! #f) 
  (cond ((or (obj? opnd) (lbl? opnd)) (add-constant-var var opnd))
        ((and (reg? opnd) (not (set-member? (reg-num opnd) (live-regs live))))
         (put-var opnd var))
        ((and (stk? opnd) (not (set-member? (stk-num opnd) (live-slots live))))
         (put-var opnd var))
        (else (save-in-slot opnd var live comment))))
(define (save-in-slot opnd var live comment) (set-bbv-version-limit! #f) 
  (let ((slot (lowest-dead-slot live))) (put-copy opnd slot var live comment)))
(define (save-var opnd var live comment) (set-bbv-version-limit! #f) 
  (cond ((or (obj? opnd) (lbl? opnd)) (add-constant-var var opnd) var)
        ((or (glo? opnd) (reg? opnd) (stk? opnd)) (get-var opnd))
        (else
         (let ((dest (or (highest-dead-reg live) (lowest-dead-slot live))))
           (put-copy opnd dest var live comment)
           var))))
(define (put-copy opnd loc var live comment) (set-bbv-version-limit! #f) 
  (if (and (stk? loc) (SFX> (stk-num loc) nb-slots)) (push-slot))
  (if var (put-var loc var))
  (if (not (eq? opnd loc))
      (bb-put-non-branch!
       *bb*
       (make-copy
        opnd
        loc
        (current-frame (if var (set-adjoin live var) live))
        comment))))
(define (var-useless? var) (set-bbv-version-limit! #f) 
  (and (set-empty? (var-refs var)) (set-empty? (var-sets var))))
(define (vals-live-vars live vals) (set-bbv-version-limit! #f) 
  (if (null? vals)
      live
      (vals-live-vars
       (set-union live (free-variables (Scar vals)))
       (Scdr vals))))
(define (gen-fut node live why) (set-bbv-version-limit! #f) 
  (let* ((val (fut-val node))
         (clo-vars (not-constant-closed-vars val))
         (clo-vars-list (set->list clo-vars))
         (ret-var* (make-temp-var 0))
         (live-after live)
         (live-starting-task
          (set-adjoin (set-union live-after clo-vars) ret-var*))
         (task-lbl (bbs-new-lbl! *bbs*))
         (return-lbl (bbs-new-lbl! *bbs*)))
    (save-regs (live-regs live-after) live-starting-task (source-comment node))
    (let ((frame-start (stk-num (highest-live-slot live-after))))
      (save-opnd-to-reg
       (make-lbl return-lbl)
       target.task-return
       ret-var*
       (set-remove live-starting-task ret-var*)
       (source-comment node))
      (let loop1 ((l clo-vars-list) (i 0))
        (if (null? l)
            (dealloc-slots (SFX- nb-slots (SFX+ frame-start i)))
            (let ((var (Scar l)) (rest (Scdr l)))
              (if (Smemq var regs)
                  (loop1 rest i)
                  (let loop2 ((j (SFX- target.nb-regs 1)))
                    (if (SFX>= j 0)
                        (if (or (SFX>= j (Slength regs))
                                (not (set-member?
                                      (Slist-ref regs j)
                                      live-starting-task)))
                            (let ((reg (make-reg j)))
                              (put-copy
                               (var->opnd var)
                               reg
                               var
                               live-starting-task
                               (source-comment node))
                              (loop1 rest i))
                            (loop2 (SFX- j 1)))
                        (let ((slot (make-stk (SFX+ frame-start (SFX+ i 1))))
                              (needed (list->set rest)))
                          (if (and (or (SFX> (stk-num slot) nb-slots)
                                       (not (Smemq (Slist-ref
                                                   slots
                                                   (SFX- nb-slots (stk-num slot)))
                                                  regs)))
                                   (set-member?
                                    (stk-num slot)
                                    (live-slots needed)))
                              (save-opnd
                               slot
                               live-starting-task
                               (source-comment node)))
                          (put-copy
                           (var->opnd var)
                           slot
                           var
                           live-starting-task
                           (source-comment node))
                          (loop1 rest (SFX+ i 1)))))))))
      (seal-bb (intrs-enabled? (node-decl node)) 'call)
      (bb-put-branch!
       *bb*
       (make-jump
        (make-lbl task-lbl)
        #f
        #f
        (current-frame live-starting-task)
        #f))
      (let ((task-context
             (make-context
              (SFX- nb-slots frame-start)
              (Sreverse (nth-after (Sreverse slots) frame-start))
              (cons ret-var (Scdr regs))
              '()
              poll
              entry-bb))
            (return-context
             (make-context
              frame-start
              (nth-after slots (SFX- nb-slots frame-start))
              '()
              closed
              (return-poll poll)
              entry-bb)))
        (restore-context task-context)
        (set! *bb*
              (make-bb (make-label-task-entry
                        task-lbl
                        (current-frame live-starting-task)
                        (source-comment node))
                       *bbs*))
        (gen-node val ret-var-set 'tail)
        (let ((result-var (make-temp-var 'future)))
          (restore-context return-context)
          (put-var target.proc-result result-var)
          (set! *bb*
                (make-bb (make-label-task-return
                          return-lbl
                          (current-frame (set-adjoin live result-var))
                          (source-comment node))
                         *bbs*))
          (gen-return target.proc-result why node))))))
(define prim-procs
  '(("not" (1) #f 0 boolean)
    ("boolean?" (1) #f 0 boolean)
    ("eqv?" (2) #f 0 boolean)
    ("eq?" (2) #f 0 boolean)
    ("equal?" (2) #f 0 boolean)
    ("pair?" (1) #f 0 boolean)
    ("cons" (2) #f () pair)
    ("car" (1) #f 0 (#f))
    ("cdr" (1) #f 0 (#f))
    ("set-car!" (2) #t (1) pair)
    ("set-cdr!" (2) #t (1) pair)
    ("caar" (1) #f 0 (#f))
    ("cadr" (1) #f 0 (#f))
    ("cdar" (1) #f 0 (#f))
    ("cddr" (1) #f 0 (#f))
    ("caaar" (1) #f 0 (#f))
    ("caadr" (1) #f 0 (#f))
    ("cadar" (1) #f 0 (#f))
    ("caddr" (1) #f 0 (#f))
    ("cdaar" (1) #f 0 (#f))
    ("cdadr" (1) #f 0 (#f))
    ("cddar" (1) #f 0 (#f))
    ("cdddr" (1) #f 0 (#f))
    ("caaaar" (1) #f 0 (#f))
    ("caaadr" (1) #f 0 (#f))
    ("caadar" (1) #f 0 (#f))
    ("caaddr" (1) #f 0 (#f))
    ("cadaar" (1) #f 0 (#f))
    ("cadadr" (1) #f 0 (#f))
    ("caddar" (1) #f 0 (#f))
    ("cadddr" (1) #f 0 (#f))
    ("cdaaar" (1) #f 0 (#f))
    ("cdaadr" (1) #f 0 (#f))
    ("cdadar" (1) #f 0 (#f))
    ("cdaddr" (1) #f 0 (#f))
    ("cddaar" (1) #f 0 (#f))
    ("cddadr" (1) #f 0 (#f))
    ("cdddar" (1) #f 0 (#f))
    ("cddddr" (1) #f 0 (#f))
    ("null?" (1) #f 0 boolean)
    ("list?" (1) #f 0 boolean)
    ("list" 0 #f () list)
    ("length" (1) #f 0 integer)
    ("append" 0 #f 0 list)
    ("reverse" (1) #f 0 list)
    ("list-ref" (2) #f 0 (#f))
    ("memq" (2) #f 0 list)
    ("memv" (2) #f 0 list)
    ("member" (2) #f 0 list)
    ("assq" (2) #f 0 #f)
    ("assv" (2) #f 0 #f)
    ("assoc" (2) #f 0 #f)
    ("symbol?" (1) #f 0 boolean)
    ("symbol->string" (1) #f 0 string)
    ("string->symbol" (1) #f 0 symbol)
    ("number?" (1) #f 0 boolean)
    ("complex?" (1) #f 0 boolean)
    ("real?" (1) #f 0 boolean)
    ("rational?" (1) #f 0 boolean)
    ("integer?" (1) #f 0 boolean)
    ("exact?" (1) #f 0 boolean)
    ("inexact?" (1) #f 0 boolean)
    ("=" 0 #f 0 boolean)
    ("<" 0 #f 0 boolean)
    (">" 0 #f 0 boolean)
    ("<=" 0 #f 0 boolean)
    (">=" 0 #f 0 boolean)
    ("zero?" (1) #f 0 boolean)
    ("positive?" (1) #f 0 boolean)
    ("negative?" (1) #f 0 boolean)
    ("odd?" (1) #f 0 boolean)
    ("even?" (1) #f 0 boolean)
    ("max" 1 #f 0 number)
    ("min" 1 #f 0 number)
    ("+" 0 #f 0 number)
    ("*" 0 #f 0 number)
    ("-" 1 #f 0 number)
    ("/" 1 #f 0 number)
    ("abs" (1) #f 0 number)
    ("quotient" 1 #f 0 integer)
    ("remainder" (2) #f 0 integer)
    ("modulo" (2) #f 0 integer)
    ("gcd" 1 #f 0 integer)
    ("lcm" 1 #f 0 integer)
    ("numerator" (1) #f 0 integer)
    ("denominator" (1) #f 0 integer)
    ("floor" (1) #f 0 integer)
    ("ceiling" (1) #f 0 integer)
    ("truncate" (1) #f 0 integer)
    ("round" (1) #f 0 integer)
    ("rationalize" (2) #f 0 number)
    ("exp" (1) #f 0 number)
    ("log" (1) #f 0 number)
    ("sin" (1) #f 0 number)
    ("cos" (1) #f 0 number)
    ("tan" (1) #f 0 number)
    ("asin" (1) #f 0 number)
    ("acos" (1) #f 0 number)
    ("atan" (1 2) #f 0 number)
    ("sqrt" (1) #f 0 number)
    ("expt" (2) #f 0 number)
    ("make-rectangular" (2) #f 0 number)
    ("make-polar" (2) #f 0 number)
    ("real-part" (1) #f 0 real)
    ("imag-part" (1) #f 0 real)
    ("magnitude" (1) #f 0 real)
    ("angle" (1) #f 0 real)
    ("exact->inexact" (1) #f 0 number)
    ("inexact->exact" (1) #f 0 number)
    ("number->string" (1 2) #f 0 string)
    ("string->number" (1 2) #f 0 number)
    ("char?" (1) #f 0 boolean)
    ("char=?" 0 #f 0 boolean)
    ("char<?" 0 #f 0 boolean)
    ("char>?" 0 #f 0 boolean)
    ("char<=?" 0 #f 0 boolean)
    ("char>=?" 0 #f 0 boolean)
    ("char-ci=?" 0 #f 0 boolean)
    ("char-ci<?" 0 #f 0 boolean)
    ("char-ci>?" 0 #f 0 boolean)
    ("char-ci<=?" 0 #f 0 boolean)
    ("char-ci>=?" 0 #f 0 boolean)
    ("char-alphabetic?" (1) #f 0 boolean)
    ("char-numeric?" (1) #f 0 boolean)
    ("char-whitespace?" (1) #f 0 boolean)
    ("char-upper-case?" (1) #f 0 boolean)
    ("char-lower-case?" (1) #f 0 boolean)
    ("char->integer" (1) #f 0 integer)
    ("integer->char" (1) #f 0 char)
    ("char-upcase" (1) #f 0 char)
    ("char-downcase" (1) #f 0 char)
    ("string?" (1) #f 0 boolean)
    ("make-string" (1 2) #f 0 string)
    ("string" 0 #f 0 string)
    ("string-length" (1) #f 0 integer)
    ("string-ref" (2) #f 0 char)
    ("string-set!" (3) #t 0 string)
    ("string=?" 0 #f 0 boolean)
    ("string<?" 0 #f 0 boolean)
    ("string>?" 0 #f 0 boolean)
    ("string<=?" 0 #f 0 boolean)
    ("string>=?" 0 #f 0 boolean)
    ("string-ci=?" 0 #f 0 boolean)
    ("string-ci<?" 0 #f 0 boolean)
    ("string-ci>?" 0 #f 0 boolean)
    ("string-ci<=?" 0 #f 0 boolean)
    ("string-ci>=?" 0 #f 0 boolean)
    ("substring" (3) #f 0 string)
    ("string-append" 0 #f 0 string)
    ("vector?" (1) #f 0 boolean)
    ("make-vector" (1 2) #f (1) vector)
    ("vector" 0 #f () vector)
    ("vector-length" (1) #f 0 integer)
    ("vector-ref" (2) #f 0 (#f))
    ("vector-set!" (3) #t (1 2) vector)
    ("procedure?" (1) #f 0 boolean)
    ("apply" 2 #t 0 (#f))
    ("map" 2 #t 0 list)
    ("for-each" 2 #t 0 #f)
    ("call-with-current-continuation" (1) #t 0 (#f))
    ("call-with-input-file" (2) #t 0 (#f))
    ("call-with-output-file" (2) #t 0 (#f))
    ("input-port?" (1) #f 0 boolean)
    ("output-port?" (1) #f 0 boolean)
    ("current-input-port" (0) #f 0 port)
    ("current-output-port" (0) #f 0 port)
    ("open-input-file" (1) #t 0 port)
    ("open-output-file" (1) #t 0 port)
    ("close-input-port" (1) #t 0 #f)
    ("close-output-port" (1) #t 0 #f)
    ("eof-object?" (1) #f 0 boolean)
    ("read" (0 1) #t 0 #f)
    ("read-char" (0 1) #t 0 #f)
    ("peek-char" (0 1) #t 0 #f)
    ("write" (0 1) #t 0 #f)
    ("display" (0 1) #t 0 #f)
    ("newline" (0 1) #t 0 #f)
    ("write-char" (1 2) #t 0 #f)
    ("list-tail" (2) #f 0 (#f))
    ("string->list" (1) #f 0 list)
    ("list->string" (1) #f 0 string)
    ("string-copy" (1) #f 0 string)
    ("string-fill!" (2) #t 0 string)
    ("vector->list" (1) #f 0 list)
    ("list->vector" (1) #f 0 vector)
    ("vector-fill!" (2) #t 0 vector)
    ("force" (1) #t 0 #f)
    ("with-input-from-file" (2) #t 0 (#f))
    ("with-output-to-file" (2) #t 0 (#f))
    ("char-ready?" (0 1) #f 0 boolean)
    ("load" (1) #t 0 (#f))
    ("transcript-on" (1) #t 0 #f)
    ("transcript-off" (0) #t 0 #f)
    ("touch" (1) #t 0 #f)
    ("##type" (1) #f () integer)
    ("##type-cast" (2) #f () (#f))
    ("##subtype" (1) #f () integer)
    ("##subtype-set!" (2) #t () #f)
    ("##not" (1) #f () boolean)
    ("##null?" (1) #f () boolean)
    ("##unassigned?" (1) #f () boolean)
    ("##unbound?" (1) #f () boolean)
    ("##eq?" (2) #f () boolean)
    ("##fixnum?" (1) #f () boolean)
    ("##flonum?" (1) #f () boolean)
    ("##special?" (1) #f () boolean)
    ("##pair?" (1) #f () boolean)
    ("##subtyped?" (1) #f () boolean)
    ("##procedure?" (1) #f () boolean)
    ("##placeholder?" (1) #f () boolean)
    ("##vector?" (1) #f () boolean)
    ("##symbol?" (1) #f () boolean)
    ("##ratnum?" (1) #f () boolean)
    ("##cpxnum?" (1) #f () boolean)
    ("##string?" (1) #f () boolean)
    ("##bignum?" (1) #f () boolean)
    ("##char?" (1) #f () boolean)
    ("##closure?" (1) #f () boolean)
    ("##subprocedure?" (1) #f () boolean)
    ("##return-dynamic-env-bind?" (1) #f () boolean)
    ("##fixnum.+" 0 #f () integer)
    ("##fixnum.*" 0 #f () integer)
    ("##fixnum.-" 1 #f () integer)
    ("##fixnum.quotient" (2) #f () integer)
    ("##fixnum.remainder" (2) #f () integer)
    ("##fixnum.modulo" (2) #f () integer)
    ("##fixnum.logior" 0 #f () integer)
    ("##fixnum.logxor" 0 #f () integer)
    ("##fixnum.logand" 0 #f () integer)
    ("##fixnum.lognot" (1) #f () integer)
    ("##fixnum.ash" (2) #f () integer)
    ("##fixnum.lsh" (2) #f () integer)
    ("##fixnum.zero?" (1) #f () boolean)
    ("##fixnum.positive?" (1) #f () boolean)
    ("##fixnum.negative?" (1) #f () boolean)
    ("##fixnum.odd?" (1) #f () boolean)
    ("##fixnum.even?" (1) #f () boolean)
    ("##fixnum.=" 0 #f () boolean)
    ("##fixnum.<" 0 #f () boolean)
    ("##fixnum.>" 0 #f () boolean)
    ("##fixnum.<=" 0 #f () boolean)
    ("##fixnum.>=" 0 #f () boolean)
    ("##flonum.->fixnum" (1) #f () integer)
    ("##flonum.<-fixnum" (1) #f () real)
    ("##flonum.+" 0 #f () real)
    ("##flonum.*" 0 #f () real)
    ("##flonum.-" 1 #f () real)
    ("##flonum./" 1 #f () real)
    ("##flonum.abs" (1) #f () real)
    ("##flonum.truncate" (1) #f () real)
    ("##flonum.round" (1) #f () real)
    ("##flonum.exp" (1) #f () real)
    ("##flonum.log" (1) #f () real)
    ("##flonum.sin" (1) #f () real)
    ("##flonum.cos" (1) #f () real)
    ("##flonum.tan" (1) #f () real)
    ("##flonum.asin" (1) #f () real)
    ("##flonum.acos" (1) #f () real)
    ("##flonum.atan" (1) #f () real)
    ("##flonum.sqrt" (1) #f () real)
    ("##flonum.zero?" (1) #f () boolean)
    ("##flonum.positive?" (1) #f () boolean)
    ("##flonum.negative?" (1) #f () boolean)
    ("##flonum.=" 0 #f () boolean)
    ("##flonum.<" 0 #f () boolean)
    ("##flonum.>" 0 #f () boolean)
    ("##flonum.<=" 0 #f () boolean)
    ("##flonum.>=" 0 #f () boolean)
    ("##char=?" 0 #f () boolean)
    ("##char<?" 0 #f () boolean)
    ("##char>?" 0 #f () boolean)
    ("##char<=?" 0 #f () boolean)
    ("##char>=?" 0 #f () boolean)
    ("##cons" (2) #f () pair)
    ("##set-car!" (2) #t () pair)
    ("##set-cdr!" (2) #t () pair)
    ("##car" (1) #f () (#f))
    ("##cdr" (1) #f () (#f))
    ("##caar" (1) #f () (#f))
    ("##cadr" (1) #f () (#f))
    ("##cdar" (1) #f () (#f))
    ("##cddr" (1) #f () (#f))
    ("##caaar" (1) #f () (#f))
    ("##caadr" (1) #f () (#f))
    ("##cadar" (1) #f () (#f))
    ("##caddr" (1) #f () (#f))
    ("##cdaar" (1) #f () (#f))
    ("##cdadr" (1) #f () (#f))
    ("##cddar" (1) #f () (#f))
    ("##cdddr" (1) #f () (#f))
    ("##caaaar" (1) #f () (#f))
    ("##caaadr" (1) #f () (#f))
    ("##caadar" (1) #f () (#f))
    ("##caaddr" (1) #f () (#f))
    ("##cadaar" (1) #f () (#f))
    ("##cadadr" (1) #f () (#f))
    ("##caddar" (1) #f () (#f))
    ("##cadddr" (1) #f () (#f))
    ("##cdaaar" (1) #f () (#f))
    ("##cdaadr" (1) #f () (#f))
    ("##cdadar" (1) #f () (#f))
    ("##cdaddr" (1) #f () (#f))
    ("##cddaar" (1) #f () (#f))
    ("##cddadr" (1) #f () (#f))
    ("##cdddar" (1) #f () (#f))
    ("##cddddr" (1) #f () (#f))
    ("##make-cell" (1) #f () pair)
    ("##cell-ref" (1) #f () (#f))
    ("##cell-set!" (2) #t () pair)
    ("##vector" 0 #f () vector)
    ("##make-vector" (2) #f () vector)
    ("##vector-length" (1) #f () integer)
    ("##vector-ref" (2) #f () (#f))
    ("##vector-set!" (3) #t () vector)
    ("##vector-shrink!" (2) #t () vector)
    ("##string" 0 #f () string)
    ("##make-string" (2) #f () string)
    ("##string-length" (1) #f () integer)
    ("##string-ref" (2) #f () char)
    ("##string-set!" (3) #t () string)
    ("##string-shrink!" (2) #t () string)
    ("##vector8" 0 #f () string)
    ("##make-vector8" (2) #f () string)
    ("##vector8-length" (1) #f () integer)
    ("##vector8-ref" (2) #f () integer)
    ("##vector8-set!" (3) #t () string)
    ("##vector8-shrink!" (2) #t () string)
    ("##vector16" 0 #f () string)
    ("##make-vector16" (2) #f () string)
    ("##vector16-length" (1) #f () integer)
    ("##vector16-ref" (2) #f () integer)
    ("##vector16-set!" (3) #t () string)
    ("##vector16-shrink!" (2) #t () string)
    ("##closure-code" (1) #f () #f)
    ("##closure-ref" (2) #f () (#f))
    ("##closure-set!" (3) #t () #f)
    ("##subprocedure-id" (1) #f () #f)
    ("##subprocedure-parent" (1) #f () #f)
    ("##return-fs" (1) #f () #f)
    ("##return-link" (1) #f () #f)
    ("##procedure-info" (1) #f () #f)
    ("##pstate" (0) #f () #f)
    ("##make-placeholder" (1) #f 0 (#f))
    ("##touch" (1) #t 0 #f)
    ("##apply" (2) #t () (#f))
    ("##call-with-current-continuation" (1) #t () (#f))
    ("##global-var" (1) #t () #f)
    ("##global-var-ref" (1) #f () (#f))
    ("##global-var-set!" (2) #t () #f)
    ("##atomic-car" (1) #f () (#f))
    ("##atomic-cdr" (1) #f () (#f))
    ("##atomic-set-car!" (2) #t () pair)
    ("##atomic-set-cdr!" (2) #t () pair)
    ("##atomic-set-car-if-eq?!" (3) #t () boolean)
    ("##atomic-set-cdr-if-eq?!" (3) #t () boolean)
    ("##quasi-append" 0 #f 0 list)
    ("##quasi-list" 0 #f () list)
    ("##quasi-cons" (2) #f () pair)
    ("##quasi-list->vector" (1) #f 0 vector)
    ("##case-memv" (2) #f 0 list)))
(define ofile-version-major 5)
(define ofile-version-minor 0)
(define prim-proc-prefix 1)
(define user-proc-prefix 2)
(define pair-prefix 3)
(define flonum-prefix 4)
(define local-object-bits -524281)
(define symbol-object-bits -393209)
(define prim-proc-object-bits -262137)
(define padding-tag 0)
(define end-of-code-tag 32768)
(define m68020-proc-code-tag 32769)
(define m68881-proc-code-tag 32770)
(define stat-tag 32771)
(define global-var-ref-tag 34816)
(define global-var-set-tag 36864)
(define global-var-ref-jump-tag 38912)
(define prim-proc-ref-tag 40960)
(define local-proc-ref-tag 49152)
(define long-index-mask 16383)
(define word-index-mask 2047)
(define (ofile.begin! filename add-obj) (set-bbv-version-limit! #f) 
  (set! ofile-add-obj add-obj)
  (set! ofile-syms (queue-empty))
;  (set! *ofile-port1* (open-output-file (Sstring-append filename ".O")))
  (if ofile-asm?
      (begin
        (set! *ofile-port2*
              (asm-open-output-file (Sstring-append filename ".asm")))
        (set! *ofile-pos* 0)))
  (ofile-word ofile-version-major)
  (ofile-word ofile-version-minor)
  '())
(define (ofile.end!) (set-bbv-version-limit! #f) 
  (ofile-line "")
;  (close-output-port *ofile-port1*)
  (if ofile-asm? (asm-close-output-port *ofile-port2*))
  '())
(define asm-output '())
(define asm-line '())
(define (asm-open-output-file filename) (set-bbv-version-limit! #f) 
  (set! asm-output '())
  (set! asm-line '()))
(define (asm-close-output-port asm-port) (set-bbv-version-limit! #f)  #f)
(define (asm-newline asm-port) (set-bbv-version-limit! #f)  (asm-display char-newline asm-port))
(define (asm-display obj asm-port) (set-bbv-version-limit! #f) 
  (if (eqv? obj char-newline)
      (begin
        (set! asm-output
              (cons (LIBstring-concatenate (Sreverse asm-line)) asm-output))
        (set! asm-line '()))
      (set! asm-line
            (cons (cond ((string? obj) obj)
                        ((char? obj) (if (eqv? obj char-tab) " " (string obj)))
                        ((number? obj) (SFXnumber->string obj))
                        (else (compiler-internal-error "asm-display" obj)))
                  asm-line))))
(define (asm-output-get) (set-bbv-version-limit! #f)  (Sreverse asm-output))
(define *ofile-port1* '())
(define *ofile-port2* '())
(define *ofile-pos* '())
(define ofile-nl char-newline)
(define ofile-tab char-tab)
(define ofile-asm? '())
(set! ofile-asm? '())
(define ofile-asm-bits? '())
(set! ofile-asm-bits? #f)
(define ofile-asm-gvm? '())
(set! ofile-asm-gvm? #f)
(define ofile-stats? '())
(set! ofile-stats? '())
(define ofile-add-obj '())
(set! ofile-add-obj '())
(define ofile-syms '())
(set! ofile-syms '())
(define (ofile-word n) (set-bbv-version-limit! #f) 
  (let ((n (SFXmodulo n 65536)))
    (if (and ofile-asm? ofile-asm-bits?)
        (let ()
          (define (ofile-display x) (set-bbv-version-limit! #f) 
            (asm-display x *ofile-port2*)
            (cond ((eq? x ofile-nl) (set! *ofile-pos* 0))
                  ((eq? x ofile-tab)
                   (set! *ofile-pos* (SFX* (SFXquotient (SFX+ *ofile-pos* 8) 8) 8)))
                  (else (set! *ofile-pos* (SFX+ *ofile-pos* (Sstring-length x))))))
          (if (SFX> *ofile-pos* 64) (ofile-display ofile-nl))
          (if (SFX= *ofile-pos* 0) (ofile-display " .word") (ofile-display ","))
          (ofile-display ofile-tab)
          (let ((s (Smake-string2 6 #\0)))
            (Sstring-set! s 1 #\x)
            (let loop ((i 5) (n n))
              (if (SFX> n 0)
                  (begin
                    (Sstring-set!
                     s
                     i
                     (Sstring-ref "0123456789ABCDEF" (SFXremainder n 16)))
                    (loop (SFX- i 1) (SFXquotient n 16)))))
            (ofile-display s))))
'    (write-word n *ofile-port1*)))
(define (ofile-long x) (set-bbv-version-limit! #f)  (ofile-word (upper-16bits x)) (ofile-word x))
(define (ofile-string s) (set-bbv-version-limit! #f) 
  (let ((len (Sstring-length s)))
    (define (ref i) (set-bbv-version-limit! #f)  (if (SFX>= i len) 0 (character-encoding (Sstring-ref s i))))
    (let loop ((i 0))
      (if (SFX< i len)
          (begin
            (ofile-word (SFX+ (SFX* (ref i) 256) (ref (SFX+ i 1))))
            (loop (SFX+ i 2)))))
    (if (SFX= (SFXremainder len 2) 0) (ofile-word 0))))
(define (ofile-wsym tag name) (set-bbv-version-limit! #f) 
  (let ((n (string-pos-in-list name (queue->list ofile-syms))))
    (if n
        (ofile-word (SFX+ tag n))
        (let ((m (Slength (queue->list ofile-syms))))
          (queue-put! ofile-syms name)
          (ofile-word (SFX+ tag word-index-mask))
          (ofile-string name)))))
(define (ofile-lsym tag name) (set-bbv-version-limit! #f) 
  (let ((n (string-pos-in-list name (queue->list ofile-syms))))
    (if n
        (ofile-long (SFX+ tag (SFX* n 8)))
        (let ((m (Slength (queue->list ofile-syms))))
          (queue-put! ofile-syms name)
          (ofile-long (SFX+ tag (SFX* long-index-mask 8)))
          (ofile-string name)))))
(define (ofile-ref obj) (set-bbv-version-limit! #f) 
  (let ((n (obj-encoding obj)))
    (if n
        (ofile-long n)
        (if (symbol-object? obj)
            (begin (ofile-lsym symbol-object-bits (Ssymbol->string obj)))
            (let ((m (ofile-add-obj obj)))
              (if m
                  (ofile-long (SFX+ local-object-bits (SFX* m 8)))
                  (begin
                    (ofile-lsym
                     prim-proc-object-bits
                     (proc-obj-name obj)))))))))
(define (ofile-prim-proc s) (set-bbv-version-limit! #f) 
  (ofile-long prim-proc-prefix)
  (ofile-wsym 0 s)
  (ofile-comment (list "| #[primitive " s "] =")))
(define (ofile-user-proc) (set-bbv-version-limit! #f)  (ofile-long user-proc-prefix))
(define (ofile-line s) (set-bbv-version-limit! #f) 
  (if ofile-asm?
      (begin
        (if (SFX> *ofile-pos* 0) (asm-newline *ofile-port2*))
        (asm-display s *ofile-port2*)
        (asm-newline *ofile-port2*)
        (set! *ofile-pos* 0))))
(define (ofile-tabs-to n) (set-bbv-version-limit! #f) 
  (let loop ()
    (if (SFX< *ofile-pos* n)
        (begin
          (asm-display ofile-tab *ofile-port2*)
          (set! *ofile-pos* (SFX* (SFXquotient (SFX+ *ofile-pos* 8) 8) 8))
          (loop)))))
(define (ofile-comment l) (set-bbv-version-limit! #f) 
  (if ofile-asm?
      (let ()
        (if ofile-asm-bits?
            (begin (ofile-tabs-to 32) (asm-display "|" *ofile-port2*)))
        (for-each (lambda (x) (set-bbv-version-limit! #f)  (asm-display x *ofile-port2*)) l)
        (asm-newline *ofile-port2*)
        (set! *ofile-pos* 0))))
(define (ofile-gvm-instr code) (set-bbv-version-limit! #f) 
  (if (and ofile-asm? ofile-asm-gvm?)
      (let ((gvm-instr (code-gvm-instr code)) (sn (code-slots-needed code)))
        (if (SFX> *ofile-pos* 0)
            (begin (asm-newline *ofile-port2*) (set! *ofile-pos* 0)))
        (if ofile-asm-bits? (ofile-tabs-to 32))
        (asm-display "| GVM: [" *ofile-port2*)
        (asm-display sn *ofile-port2*)
        (asm-display "] " *ofile-port2*)
        (asm-newline *ofile-port2*)
        (set! *ofile-pos* 0))))
(define (ofile-stat stat) (set-bbv-version-limit! #f) 
  (define (obj->string x) (set-bbv-version-limit! #f) 
    (cond ((string? x) x)
          ((symbol-object? x) (Ssymbol->string x))
          ((number? x) (SFXnumber->string x))
          ((false-object? x) "#f")
          ((eq? x #t) "#t")
          ((null? x) "()")
          ((pair? x)
           (let loop ((l1 (Scdr x)) (l2 (list (obj->string (Scar x)) "(")))
             (cond ((pair? l1)
                    (loop (Scdr l1)
                          (cons (obj->string (Scar l1)) (cons " " l2))))
                   ((null? l1) (LIBstring-concatenate (Sreverse (cons ")" l2))))
                   (else
                    (LIBstring-concatenate
                           (Sreverse (cons ")"
                                          (cons (obj->string l1)
                                                (cons " . " l2)))))))))
          (else
           (compiler-internal-error
            "ofile-stat, can't convert to string 'x'"
            x))))
  (ofile-string (obj->string stat)))
(define (upper-16bits x) (set-bbv-version-limit! #f) 
  (cond ((SFX>= x 0) (SFXquotient x 65536))
        ((SFX>= x (SFX- 65536)) -1)
        (else (SFX- (SFXquotient (SFX+ x 65537) 65536) 2))))
(define type-fixnum 0)
(define type-flonum 1)
(define type-special 7)
(define type-pair 4)
(define type-placeholder 5)
(define type-subtyped 3)
(define type-procedure 2)
(define subtype-vector 0)
(define subtype-symbol 1)
(define subtype-port 2)
(define subtype-ratnum 3)
(define subtype-cpxnum 4)
(define subtype-string 16)
(define subtype-bignum 17)
(define data-false (SFX- 33686019))
(define data-null (SFX- 67372037))
(define data-true -2)
(define data-undef -3)
(define data-unass -4)
(define data-unbound -5)
(define data-eof -6)
(define data-max-fixnum 268435455)
(define data-min-fixnum (SFX- 268435456))
(define (make-encoding data type) (set-bbv-version-limit! #f)  (SFX+ (SFX* data 8) type))
(define (obj-type obj) (set-bbv-version-limit! #f) 
  (cond ((false-object? obj) 'special)
        ((undef-object? obj) 'special)
        ((symbol-object? obj) 'subtyped)
        ((proc-obj? obj) 'procedure)
        ((eq? obj #t) 'special)
        ((null? obj) 'special)
        ((pair? obj) 'pair)
        ((number? obj)
         (cond ((and (integer? obj)
                     (exact? obj)
                     (SFX>= obj data-min-fixnum)
                     (SFX<= obj data-max-fixnum))
                'fixnum)
               (
#t
;;                (and (inexact? (real-part obj))
;;                     (zero? (imag-part obj))
;;                     (exact? (imag-part obj)))
                'flonum)
               (else 'subtyped)))
        ((char? obj) 'special)
        (else 'subtyped)))
(define (obj-subtype obj) (set-bbv-version-limit! #f) 
  (cond ((symbol-object? obj) 'symbol)
        ((number? obj)
         (cond ((and (integer? obj) (exact? obj)) 'bignum)
               ((and (rational? obj) (exact? obj)) 'ratnum)
               (else 'cpxnum)))
        ((vector? obj) 'vector)
        ((string? obj) 'string)
        (else
         (compiler-internal-error "obj-subtype, unknown object 'obj'" obj))))
(define (obj-type-tag obj) (set-bbv-version-limit! #f) 
  (case (obj-type obj)
    ((fixnum) type-fixnum)
    ((flonum) type-flonum)
    ((special) type-special)
    ((pair) type-pair)
    ((subtyped) type-subtyped)
    ((procedure) type-procedure)
    (else (compiler-internal-error "obj-type-tag, unknown object 'obj'" obj))))
(define (obj-encoding obj) (set-bbv-version-limit! #f) 
  (case (obj-type obj)
    ((fixnum) (make-encoding obj type-fixnum))
    ((special)
     (make-encoding
      (cond ((false-object? obj) data-false)
            ((undef-object? obj) data-undef)
            ((eq? obj #t) data-true)
            ((null? obj) data-null)
            ((char? obj) (character-encoding obj))
            (else
             (compiler-internal-error
              "obj-encoding, unknown SPECIAL object 'obj'"
              obj)))
      type-special))
    (else #f)))
(define bits-false (make-encoding data-false type-special))
(define bits-null (make-encoding data-null type-special))
(define bits-true (make-encoding data-true type-special))
(define bits-unass (make-encoding data-unass type-special))
(define bits-unbound (make-encoding data-unbound type-special))
(define (asm.begin!) (set-bbv-version-limit! #f) 
  (set! asm-code-queue (queue-empty))
  (set! asm-const-queue (queue-empty))
  '())
(define (asm.end! debug-info) (set-bbv-version-limit! #f) 
  (asm-assemble! debug-info)
  (set! asm-code-queue '())
  (set! asm-const-queue '())
  '())
(define asm-code-queue '())
(define asm-const-queue '())
(define (asm-word x) (set-bbv-version-limit! #f)  (queue-put! asm-code-queue (SFXmodulo x 65536)))
(define (asm-long x) (set-bbv-version-limit! #f)  (asm-word (upper-16bits x)) (asm-word x))
(define (asm-label lbl label-descr) (set-bbv-version-limit! #f) 
  (queue-put! asm-code-queue (cons 'label (cons lbl label-descr))))
(define (asm-comment x) (set-bbv-version-limit! #f)  (queue-put! asm-code-queue (cons 'comment x)))
(define (asm-align n offset) (set-bbv-version-limit! #f) 
  (queue-put! asm-code-queue (cons 'align (cons n offset))))
(define (asm-ref-glob glob) (set-bbv-version-limit! #f) 
  (queue-put!
   asm-code-queue
   (cons 'ref-glob (Ssymbol->string (glob-name glob)))))
(define (asm-set-glob glob) (set-bbv-version-limit! #f) 
  (queue-put!
   asm-code-queue
   (cons 'set-glob (Ssymbol->string (glob-name glob)))))
(define (asm-ref-glob-jump glob) (set-bbv-version-limit! #f) 
  (queue-put!
   asm-code-queue
   (cons 'ref-glob-jump (Ssymbol->string (glob-name glob)))))
(define (asm-proc-ref num offset) (set-bbv-version-limit! #f) 
  (queue-put! asm-code-queue (cons 'proc-ref (cons num offset))))
(define (asm-prim-ref proc offset) (set-bbv-version-limit! #f) 
  (queue-put!
   asm-code-queue
   (cons 'prim-ref (cons (proc-obj-name proc) offset))))
(define (asm-m68020-proc) (set-bbv-version-limit! #f)  (queue-put! asm-code-queue '(m68020-proc)))
(define (asm-m68881-proc) (set-bbv-version-limit! #f)  (queue-put! asm-code-queue '(m68881-proc)))
(define (asm-stat x) (set-bbv-version-limit! #f)  (queue-put! asm-code-queue (cons 'stat x)))
(define (asm-brel type lbl) (set-bbv-version-limit! #f) 
  (queue-put! asm-code-queue (cons 'brab (cons type lbl))))
(define (asm-wrel lbl offs) (set-bbv-version-limit! #f) 
  (queue-put! asm-code-queue (cons 'wrel (cons lbl offs))))
(define (asm-lrel lbl offs n) (set-bbv-version-limit! #f) 
  (queue-put! asm-code-queue (cons 'lrel (cons lbl (cons offs n)))))
(define (asm-assemble! debug-info) (set-bbv-version-limit! #f) 
  (define header-offset 2)
  (define ref-glob-len 2)
  (define set-glob-len 10)
  (define ref-glob-jump-len 2)
  (define proc-ref-len 4)
  (define prim-ref-len 4)
  (define stat-len 4)
  (define (padding loc n offset) (set-bbv-version-limit! #f)  (SFXmodulo (SFX- offset loc) n))
  (queue-put! asm-const-queue debug-info)
  (asm-align 4 0)
  (emit-label const-lbl)
  (let ((code-list (queue->list asm-code-queue))
        (const-list (queue->list asm-const-queue)))
    (let* ((fix-list
            (let loop ((l code-list) (len header-offset) (x '()))
              (if (null? l)
                  (Sreverse x)
                  (let ((part (Scar l)) (rest (Scdr l)))
                    (if (pair? part)
                        (case (Scar part)
                          ((label align brab)
                           (loop rest 0 (cons (cons len part) x)))
                          ((wrel) (loop rest (SFX+ len 2) x))
                          ((lrel) (loop rest (SFX+ len 4) x))
                          ((ref-glob) (loop rest (SFX+ len ref-glob-len) x))
                          ((set-glob) (loop rest (SFX+ len set-glob-len) x))
                          ((ref-glob-jump)
                           (loop rest (SFX+ len ref-glob-jump-len) x))
                          ((proc-ref) (loop rest (SFX+ len proc-ref-len) x))
                          ((prim-ref) (loop rest (SFX+ len prim-ref-len) x))
                          ((stat) (loop rest (SFX+ len stat-len) x))
                          ((comment m68020-proc m68881-proc) (loop rest len x))
                          (else
                           (compiler-internal-error
                            "asm-assemble!, unknown code list element"
                            part)))
                        (loop rest (SFX+ len 2) x))))))
           (lbl-list
            (let loop ((l fix-list) (x '()))
              (if (null? l)
                  x
                  (let ((part (Scdar l)) (rest (Scdr l)))
                    (if (eq? (Scar part) 'label)
                        (loop rest (cons (cons (Scadr part) part) x))
                        (loop rest x)))))))
      (define (replace-lbl-refs-by-pointer-to-label) (set-bbv-version-limit! #f) 
        (let loop ((l code-list))
          (if (not (null? l))
              (let ((part (Scar l)) (rest (Scdr l)))
                (if (pair? part)
                    (case (Scar part)
                      ((brab)
                       (Sset-cdr! (Scdr part) (Scdr (Sassq (Scddr part) lbl-list))))
                      ((wrel)
                       (Sset-car! (Scdr part) (Scdr (Sassq (Scadr part) lbl-list))))
                      ((lrel)
                       (Sset-car!
                        (Scdr part)
                        (Scdr (Sassq (Scadr part) lbl-list))))))
                (loop rest)))))
      (define (assign-loc-to-labels) (set-bbv-version-limit! #f) 
        (let loop ((l fix-list) (loc 0))
          (if (not (null? l))
              (let* ((first (Scar l))
                     (rest (Scdr l))
                     (len (Scar first))
                     (cur-loc (SFX+ loc len))
                     (part (Scdr first)))
                (case (Scar part)
                  ((label)
                   (if (Scddr part)
                       (Svector-set!
                        (Scddr part)
                        0
                        (SFXquotient (SFX- cur-loc header-offset) 8)))
                   (Sset-car! (Scdr part) cur-loc)
                   (loop rest cur-loc))
                  ((align)
                   (loop rest
                         (SFX+ cur-loc
                            (padding cur-loc (Scadr part) (Scddr part)))))
                  ((brab) (loop rest (SFX+ cur-loc 2)))
                  ((braw) (loop rest (SFX+ cur-loc 4)))
                  (else
                   (compiler-internal-error
                    "assign-loc-to-labels, unknown code list element"
                    part)))))))
      (define (branch-tensioning-pass) (set-bbv-version-limit! #f) 
        (assign-loc-to-labels)
        (let loop ((changed? #f) (l fix-list) (loc 0))
          (if (null? l)
              (if changed? (branch-tensioning-pass))
              (let* ((first (Scar l))
                     (rest (Scdr l))
                     (len (Scar first))
                     (cur-loc (SFX+ loc len))
                     (part (Scdr first)))
                (case (Scar part)
                  ((label) (loop changed? rest cur-loc))
                  ((align)
                   (loop changed?
                         rest
                         (SFX+ cur-loc
                            (padding cur-loc (Scadr part) (Scddr part)))))
                  ((brab)
                   (let ((dist (SFX- (Scadr (Scddr part)) (SFX+ cur-loc 2))))
                     (if (or (SFX< dist -128) (SFX> dist 127) (SFX= dist 0))
                         (begin
                           (Sset-car! part 'braw)
                           (loop #t rest (SFX+ cur-loc 2)))
                         (loop changed? rest (SFX+ cur-loc 2)))))
                  ((braw) (loop changed? rest (SFX+ cur-loc 4)))
                  (else
                   (compiler-internal-error
                    "branch-tensioning-pass, unknown code list element"
                    part)))))))
      (define (write-block start-loc end-loc start end) (set-bbv-version-limit! #f) 
        (if (SFX> end-loc start-loc)
            (ofile-word (SFXquotient (SFX- end-loc start-loc) 2)))
        (let loop ((loc start-loc) (l start))
          (if (not (eq? l end))
              (let ((part (Scar l)) (rest (Scdr l)))
                (if (pair? part)
                    (case (Scar part)
                      ((label) (loop loc rest))
                      ((align)
                       (let ((n (padding loc (Scadr part) (Scddr part))))
                         (let pad ((i 0))
                           (if (SFX< i n)
                               (begin (ofile-word 0) (pad (SFX+ i 2)))
                               (loop (SFX+ loc n) rest)))))
                      ((brab)
                       (let ((dist (SFX- (Scadr (Scddr part)) (SFX+ loc 2))))
                         (ofile-word (SFX+ (Scadr part) (SFXmodulo dist 256)))
                         (loop (SFX+ loc 2) rest)))
                      ((braw)
                       (let ((dist (SFX- (Scadr (Scddr part)) (SFX+ loc 2))))
                         (ofile-word (Scadr part))
                         (ofile-word (SFXmodulo dist 65536))
                         (loop (SFX+ loc 4) rest)))
                      ((wrel)
                       (let ((dist (SFX+ (SFX- (Scadr (Scadr part)) loc) (Scddr part))))
                         (ofile-word (SFXmodulo dist 65536))
                         (loop (SFX+ loc 2) rest)))
                      ((lrel)
                       (let ((dist (SFX+ (SFX- (Scadr (Scadr part)) loc)
                                      (Scaddr part))))
                         (ofile-long (SFX+ (SFX* dist 65536) (Scdddr part)))
                         (loop (SFX+ loc 4) rest)))
                      ((comment)
                       (let ((x (Scdr part)))
                         (if (pair? x) (ofile-comment x) (ofile-gvm-instr x))
                         (loop loc rest))))
                    (begin (ofile-word part) (loop (SFX+ loc 2) rest)))))))
      (define (write-code) (set-bbv-version-limit! #f) 
        (let ((proc-len
               (SFX+ (Scadr (Scdr (Sassq const-lbl lbl-list)))
                  (SFX* (Slength const-list) 4))))
          (if (SFX>= proc-len 32768)
              (compiler-limitation-error
               "procedure is too big (32K bytes limit per procedure)"))
          (ofile-word (SFX+ 32768 proc-len)))
        (let loop1 ((start code-list) (start-loc header-offset))
          (let loop2 ((end start) (loc start-loc))
            (if (null? end)
                (write-block start-loc loc start end)
                (let ((part (Scar end)) (rest (Scdr end)))
                  (if (pair? part)
                      (case (Scar part)
                        ((label comment) (loop2 rest loc))
                        ((align)
                         (loop2 rest
                                (SFX+ loc (padding loc (Scadr part) (Scddr part)))))
                        ((brab wrel) (loop2 rest (SFX+ loc 2)))
                        ((braw) (loop2 rest (SFX+ loc 4)))
                        ((lrel) (loop2 rest (SFX+ loc 4)))
                        (else
                         (write-block start-loc loc start end)
                         (case (Scar part)
                           ((ref-glob)
                            (ofile-wsym global-var-ref-tag (Scdr part))
                            (loop1 rest (SFX+ loc ref-glob-len)))
                           ((set-glob)
                            (ofile-wsym global-var-set-tag (Scdr part))
                            (loop1 rest (SFX+ loc set-glob-len)))
                           ((ref-glob-jump)
                            (ofile-wsym global-var-ref-jump-tag (Scdr part))
                            (loop1 rest (SFX+ loc ref-glob-jump-len)))
                           ((proc-ref)
                            (ofile-word (SFX+ local-proc-ref-tag (Scadr part)))
                            (ofile-word (Scddr part))
                            (loop1 rest (SFX+ loc proc-ref-len)))
                           ((prim-ref)
                            (ofile-wsym prim-proc-ref-tag (Scadr part))
                            (ofile-word (Scddr part))
                            (loop1 rest (SFX+ loc prim-ref-len)))
                           ((m68020-proc)
                            (ofile-word m68020-proc-code-tag)
                            (loop1 rest loc))
                           ((m68881-proc)
                            (ofile-word m68881-proc-code-tag)
                            (loop1 rest loc))
                           ((stat)
                            (ofile-word stat-tag)
                            (ofile-stat (Scdr part))
                            (loop1 rest (SFX+ loc stat-len))))))
                      (loop2 rest (SFX+ loc 2)))))))
        (ofile-word end-of-code-tag)
        (for-each ofile-ref const-list)
        (ofile-long (obj-encoding (SFX+ (Slength const-list) 1))))
      (replace-lbl-refs-by-pointer-to-label)
      (branch-tensioning-pass)
      (write-code))))
(define const-lbl 0)
(define (identical-opnd68? opnd1 opnd2) (set-bbv-version-limit! #f)  (eqv? opnd1 opnd2))
(define (reg68? x) (set-bbv-version-limit! #f)  (or (dreg? x) (areg? x)))
(define (make-dreg num) (set-bbv-version-limit! #f)  num)
(define (dreg? x) (set-bbv-version-limit! #f)  (and (integer? x) (SFX>= x 0) (SFX< x 8)))
(define (dreg-num x) (set-bbv-version-limit! #f)  x)
(define (make-areg num) (set-bbv-version-limit! #f)  (SFX+ num 8))
(define (areg? x) (set-bbv-version-limit! #f)  (and (integer? x) (SFX>= x 8) (SFX< x 16)))
(define (areg-num x) (set-bbv-version-limit! #f)  (SFX- x 8))
(define (make-ind areg) (set-bbv-version-limit! #f)  (SFX+ areg 8))
(define (ind? x) (set-bbv-version-limit! #f)  (and (integer? x) (SFX>= x 16) (SFX< x 24)))
(define (ind-areg x) (set-bbv-version-limit! #f)  (SFX- x 8))
(define (make-pinc areg) (set-bbv-version-limit! #f)  (SFX+ areg 16))
(define (pinc? x) (set-bbv-version-limit! #f)  (and (integer? x) (SFX>= x 24) (SFX< x 32)))
(define (pinc-areg x) (set-bbv-version-limit! #f)  (SFX- x 16))
(define (make-pdec areg) (set-bbv-version-limit! #f)  (SFX+ areg 24))
(define (pdec? x) (set-bbv-version-limit! #f)  (and (integer? x) (SFX>= x 32) (SFX< x 40)))
(define (pdec-areg x) (set-bbv-version-limit! #f)  (SFX- x 24))
(define (make-disp areg offset) (set-bbv-version-limit! #f)  (SFX+ (SFX+ areg 32) (SFX* (SFXmodulo offset 65536) 8)))
(define (disp? x) (set-bbv-version-limit! #f)  (and (integer? x) (SFX>= x 40) (SFX< x 524328)))
(define (disp-areg x) (set-bbv-version-limit! #f)  (SFX+ (SFXremainder x 8) 8))
(define (disp-offset x) (set-bbv-version-limit! #f) 
  (SFX- (SFXmodulo (SFX+ (SFXquotient (SFX- x 40) 8) 32768) 65536) 32768))
(define (make-disp* areg offset) (set-bbv-version-limit! #f) 
  (if (SFX= offset 0) (make-ind areg) (make-disp areg offset)))
(define (disp*? x) (set-bbv-version-limit! #f)  (or (ind? x) (disp? x)))
(define (disp*-areg x) (set-bbv-version-limit! #f)  (if (ind? x) (ind-areg x) (disp-areg x)))
(define (disp*-offset x) (set-bbv-version-limit! #f)  (if (ind? x) 0 (disp-offset x)))
(define (make-inx areg ireg offset) (set-bbv-version-limit! #f) 
  (SFX+ (SFX+ areg 524320) (SFX+ (SFX* ireg 8) (SFX* (SFXmodulo offset 256) 128))))
(define (inx? x) (set-bbv-version-limit! #f)  (and (integer? x) (SFX>= x 524328) (SFX< x 557096)))
(define (inx-areg x) (set-bbv-version-limit! #f)  (SFX+ (SFXremainder (SFX- x 524328) 8) 8))
(define (inx-ireg x) (set-bbv-version-limit! #f)  (SFXquotient (SFXremainder (SFX- x 524328) 128) 8))
(define (inx-offset x) (set-bbv-version-limit! #f) 
  (SFX- (SFXmodulo (SFX+ (SFXquotient (SFX- x 524328) 128) 128) 256) 128))
(define (make-freg num) (set-bbv-version-limit! #f)  (SFX+ 557096 num))
(define (freg? x) (set-bbv-version-limit! #f)  (and (integer? x) (SFX>= x 557096) (SFX< x 557104)))
(define (freg-num x) (set-bbv-version-limit! #f)  (SFX- x 557096))
(define (make-pcr lbl offset) (set-bbv-version-limit! #f) 
  (SFX+ 557104 (SFX+ (SFXmodulo offset 65536) (SFX* lbl 65536))))
(define (pcr? x) (set-bbv-version-limit! #f)  (and (integer? x) (SFX>= x 557104)))
(define (pcr-lbl x) (set-bbv-version-limit! #f)  (SFXquotient (SFX- x 557104) 65536))
(define (pcr-offset x) (set-bbv-version-limit! #f)  (SFX- (SFXmodulo (SFX- x 524336) 65536) 32768))
(define (make-imm val) (set-bbv-version-limit! #f)  (if (SFX< val 0) (SFX* val 2) (SFX- -1 (SFX* val 2))))
(define (imm? x) (set-bbv-version-limit! #f)  (and (integer? x) (SFX< x 0)))
(define (imm-val x) (set-bbv-version-limit! #f)  (if (SFXeven? x) (SFXquotient x 2) (SFX- (SFXquotient x 2))))
(define (make-glob name) (set-bbv-version-limit! #f)  name)
(define (glob? x) (set-bbv-version-limit! #f)  (symbol? x))
(define (glob-name x) (set-bbv-version-limit! #f)  x)
(define (make-frame-base-rel slot) (set-bbv-version-limit! #f)  (make-disp sp-reg slot))
(define (frame-base-rel? x) (set-bbv-version-limit! #f) 
  (and (disp? x) (identical-opnd68? sp-reg (disp-areg x))))
(define (frame-base-rel-slot x) (set-bbv-version-limit! #f)  (disp-offset x))
(define (make-reg-list regs) (set-bbv-version-limit! #f)  regs)
(define (reg-list? x) (set-bbv-version-limit! #f)  (or (pair? x) (null? x)))
(define (reg-list-regs x) (set-bbv-version-limit! #f)  x)
(define first-dtemp 0)
(define gvm-reg1 1)
(define poll-timer-reg (make-dreg 5))
(define null-reg (make-dreg 6))
(define placeholder-reg (make-dreg 6))
(define false-reg (make-dreg 7))
(define pair-reg (make-dreg 7))
(define gvm-reg0 0)
(define first-atemp 1)
(define heap-reg (make-areg 3))
(define ltq-tail-reg (make-areg 4))
(define pstate-reg (make-areg 5))
(define table-reg (make-areg 6))
(define sp-reg (make-areg 7))
(define pdec-sp (make-pdec sp-reg))
(define pinc-sp (make-pinc sp-reg))
(define dtemp1 (make-dreg first-dtemp))
(define atemp1 (make-areg first-atemp))
(define atemp2 (make-areg (SFX+ first-atemp 1)))
(define ftemp1 (make-freg 0))
(define arg-count-reg dtemp1)
(define (trap-offset n) (set-bbv-version-limit! #f)  (SFX+ 32768 (SFX* (SFX- n 32) 8)))
(define (emit-move.l opnd1 opnd2) (set-bbv-version-limit! #f) 
  (let ((src (opnd->mode/reg opnd1)) (dst (opnd->reg/mode opnd2)))
    (asm-word (SFX+ 8192 (SFX+ dst src)))
    (opnd-ext-rd-long opnd1)
    (opnd-ext-wr-long opnd2)
    (if ofile-asm?
        (emit-asm "movl" ofile-tab (opnd-str opnd1) "," (opnd-str opnd2)))))
(define (emit-move.w opnd1 opnd2) (set-bbv-version-limit! #f) 
  (let ((src (opnd->mode/reg opnd1)) (dst (opnd->reg/mode opnd2)))
    (asm-word (SFX+ 12288 (SFX+ dst src)))
    (opnd-ext-rd-word opnd1)
    (opnd-ext-wr-word opnd2)
    (if ofile-asm?
        (emit-asm "movw" ofile-tab (opnd-str opnd1) "," (opnd-str opnd2)))))
(define (emit-move.b opnd1 opnd2) (set-bbv-version-limit! #f) 
  (let ((src (opnd->mode/reg opnd1)) (dst (opnd->reg/mode opnd2)))
    (asm-word (SFX+ 4096 (SFX+ dst src)))
    (opnd-ext-rd-word opnd1)
    (opnd-ext-wr-word opnd2)
    (if ofile-asm?
        (emit-asm "movb" ofile-tab (opnd-str opnd1) "," (opnd-str opnd2)))))
(define (emit-moveq n opnd) (set-bbv-version-limit! #f) 
  (asm-word (SFX+ 28672 (SFX+ (SFX* (dreg-num opnd) 512) (SFXmodulo n 256))))
  (if ofile-asm? (emit-asm "moveq" ofile-tab "#" n "," (opnd-str opnd))))
(define (emit-movem.l opnd1 opnd2) (set-bbv-version-limit! #f) 
  (define (reg-mask reg-list flip-bits?) (set-bbv-version-limit! #f) 
    (let loop ((i 15) (bit 32768) (mask 0))
      (if (SFX>= i 0)
          (loop (SFX- i 1)
                (SFXquotient bit 2)
                (if (Smemq i reg-list)
                    (SFX+ mask (if flip-bits? (SFXquotient 32768 bit) bit))
                    mask))
          mask)))
  (define (movem op reg-list opnd) (set-bbv-version-limit! #f) 
    (asm-word (SFX+ op (opnd->mode/reg opnd)))
    (asm-word (reg-mask reg-list (pdec? opnd))))
  (if (reg-list? opnd1)
      (begin (movem 18624 opnd1 opnd2) (opnd-ext-wr-long opnd2))
      (begin (movem 19648 opnd2 opnd1) (opnd-ext-rd-long opnd1)))
  (if ofile-asm?
      (emit-asm "moveml" ofile-tab (opnd-str opnd1) "," (opnd-str opnd2))))
(define (emit-exg opnd1 opnd2) (set-bbv-version-limit! #f) 
  (define (exg r1 r2) (set-bbv-version-limit! #f) 
    (let ((mode (if (dreg? r2) 49472 (if (dreg? r1) 49544 49480)))
          (num1 (if (dreg? r1) (dreg-num r1) (areg-num r1)))
          (num2 (if (dreg? r2) (dreg-num r2) (areg-num r2))))
      (asm-word (SFX+ mode (SFX+ (SFX* num1 512) num2)))))
  (if (dreg? opnd2) (exg opnd2 opnd1) (exg opnd1 opnd2))
  (if ofile-asm?
      (emit-asm "exg" ofile-tab (opnd-str opnd1) "," (opnd-str opnd2))))
(define (emit-eor.l opnd1 opnd2) (set-bbv-version-limit! #f) 
  (cond ((imm? opnd1)
         (asm-word (SFX+ 2688 (opnd->mode/reg opnd2)))
         (opnd-ext-rd-long opnd1)
         (opnd-ext-wr-long opnd2))
        (else
         (asm-word
          (SFX+ 45440 (SFX+ (SFX* (dreg-num opnd1) 512) (opnd->mode/reg opnd2))))
         (opnd-ext-wr-long opnd2)))
  (if ofile-asm?
      (emit-asm "eorl" ofile-tab (opnd-str opnd1) "," (opnd-str opnd2))))
(define (emit-and.l opnd1 opnd2) (set-bbv-version-limit! #f) 
  (cond ((imm? opnd1)
         (asm-word (SFX+ 640 (opnd->mode/reg opnd2)))
         (opnd-ext-rd-long opnd1)
         (opnd-ext-wr-long opnd2))
        (else
         (let ((mode (if (dreg? opnd2) 49280 49536))
               (reg (if (dreg? opnd2) (dreg-num opnd2) (dreg-num opnd1)))
               (other (if (dreg? opnd2) opnd1 opnd2)))
           (asm-word (SFX+ mode (SFX+ (SFX* reg 512) (opnd->mode/reg other))))
           (if (dreg? opnd2)
               (opnd-ext-rd-long other)
               (opnd-ext-wr-long other)))))
  (if ofile-asm?
      (emit-asm "andl" ofile-tab (opnd-str opnd1) "," (opnd-str opnd2))))
(define (emit-and.w opnd1 opnd2) (set-bbv-version-limit! #f) 
  (cond ((imm? opnd1)
         (asm-word (SFX+ 576 (opnd->mode/reg opnd2)))
         (opnd-ext-rd-word opnd1)
         (opnd-ext-wr-word opnd2))
        (else
         (let ((mode (if (dreg? opnd2) 49216 49472))
               (reg (if (dreg? opnd2) (dreg-num opnd2) (dreg-num opnd1)))
               (other (if (dreg? opnd2) opnd1 opnd2)))
           (asm-word (SFX+ mode (SFX+ (SFX* reg 512) (opnd->mode/reg other))))
           (if (dreg? opnd2)
               (opnd-ext-rd-word other)
               (opnd-ext-wr-word other)))))
  (if ofile-asm?
      (emit-asm "andw" ofile-tab (opnd-str opnd1) "," (opnd-str opnd2))))
(define (emit-or.l opnd1 opnd2) (set-bbv-version-limit! #f) 
  (cond ((imm? opnd1)
         (asm-word (SFX+ 128 (opnd->mode/reg opnd2)))
         (opnd-ext-rd-long opnd1)
         (opnd-ext-wr-long opnd2))
        (else
         (let ((mode (if (dreg? opnd2) 32896 33152))
               (reg (if (dreg? opnd2) (dreg-num opnd2) (dreg-num opnd1)))
               (other (if (dreg? opnd2) opnd1 opnd2)))
           (asm-word (SFX+ mode (SFX+ (SFX* reg 512) (opnd->mode/reg other))))
           (if (dreg? opnd2)
               (opnd-ext-rd-long other)
               (opnd-ext-wr-long other)))))
  (if ofile-asm?
      (emit-asm "orl" ofile-tab (opnd-str opnd1) "," (opnd-str opnd2))))
(define (emit-addq.l n opnd) (set-bbv-version-limit! #f) 
  (let ((m (if (SFX= n 8) 0 n)))
    (asm-word (SFX+ 20608 (SFX+ (SFX* m 512) (opnd->mode/reg opnd))))
    (opnd-ext-wr-long opnd)
    (if ofile-asm? (emit-asm "addql" ofile-tab "#" n "," (opnd-str opnd)))))
(define (emit-addq.w n opnd) (set-bbv-version-limit! #f) 
  (let ((m (if (SFX= n 8) 0 n)))
    (asm-word (SFX+ 20544 (SFX+ (SFX* m 512) (opnd->mode/reg opnd))))
    (opnd-ext-wr-word opnd)
    (if ofile-asm? (emit-asm "addqw" ofile-tab "#" n "," (opnd-str opnd)))))
(define (emit-add.l opnd1 opnd2) (set-bbv-version-limit! #f) 
  (cond ((areg? opnd2)
         (asm-word
          (SFX+ 53696 (SFX+ (SFX* (areg-num opnd2) 512) (opnd->mode/reg opnd1))))
         (opnd-ext-rd-long opnd1))
        ((imm? opnd1)
         (asm-word (SFX+ 1664 (opnd->mode/reg opnd2)))
         (opnd-ext-rd-long opnd1)
         (opnd-ext-wr-long opnd2))
        (else
         (let ((mode (if (dreg? opnd2) 53376 53632))
               (reg (if (dreg? opnd2) (dreg-num opnd2) (dreg-num opnd1)))
               (other (if (dreg? opnd2) opnd1 opnd2)))
           (asm-word (SFX+ mode (SFX+ (SFX* reg 512) (opnd->mode/reg other))))
           (if (dreg? opnd2)
               (opnd-ext-rd-long other)
               (opnd-ext-wr-long other)))))
  (if ofile-asm?
      (emit-asm "addl" ofile-tab (opnd-str opnd1) "," (opnd-str opnd2))))
(define (emit-add.w opnd1 opnd2) (set-bbv-version-limit! #f) 
  (cond ((areg? opnd2)
         (asm-word
          (SFX+ 53440 (SFX+ (SFX* (areg-num opnd2) 512) (opnd->mode/reg opnd1))))
         (opnd-ext-rd-word opnd1))
        ((imm? opnd1)
         (asm-word (SFX+ 1600 (opnd->mode/reg opnd2)))
         (opnd-ext-rd-word opnd1)
         (opnd-ext-wr-word opnd2))
        (else
         (let ((mode (if (dreg? opnd2) 53312 53568))
               (reg (if (dreg? opnd2) (dreg-num opnd2) (dreg-num opnd1)))
               (other (if (dreg? opnd2) opnd1 opnd2)))
           (asm-word (SFX+ mode (SFX+ (SFX* reg 512) (opnd->mode/reg other))))
           (if (dreg? opnd2)
               (opnd-ext-rd-word other)
               (opnd-ext-wr-word other)))))
  (if ofile-asm?
      (emit-asm "addw" ofile-tab (opnd-str opnd1) "," (opnd-str opnd2))))
(define (emit-addx.w opnd1 opnd2) (set-bbv-version-limit! #f) 
  (if (dreg? opnd1)
      (asm-word (SFX+ 53568 (SFX+ (SFX* (dreg-num opnd2) 512) (dreg-num opnd1))))
      (asm-word
       (SFX+ 53576
          (SFX+ (SFX* (areg-num (pdec-areg opnd2)) 512)
             (areg-num (pdec-areg opnd1))))))
  (if ofile-asm?
      (emit-asm "addxw" ofile-tab (opnd-str opnd1) "," (opnd-str opnd2))))
(define (emit-subq.l n opnd) (set-bbv-version-limit! #f) 
  (let ((m (if (SFX= n 8) 0 n)))
    (asm-word (SFX+ 20864 (SFX+ (SFX* m 512) (opnd->mode/reg opnd))))
    (opnd-ext-wr-long opnd)
    (if ofile-asm? (emit-asm "subql" ofile-tab "#" n "," (opnd-str opnd)))))
(define (emit-subq.w n opnd) (set-bbv-version-limit! #f) 
  (let ((m (if (SFX= n 8) 0 n)))
    (asm-word (SFX+ 20800 (SFX+ (SFX* m 512) (opnd->mode/reg opnd))))
    (opnd-ext-wr-word opnd)
    (if ofile-asm? (emit-asm "subqw" ofile-tab "#" n "," (opnd-str opnd)))))
(define (emit-sub.l opnd1 opnd2) (set-bbv-version-limit! #f) 
  (cond ((areg? opnd2)
         (asm-word
          (SFX+ 37312 (SFX+ (SFX* (areg-num opnd2) 512) (opnd->mode/reg opnd1))))
         (opnd-ext-rd-long opnd1))
        ((imm? opnd1)
         (asm-word (SFX+ 1152 (opnd->mode/reg opnd2)))
         (opnd-ext-rd-long opnd1)
         (opnd-ext-wr-long opnd2))
        (else
         (let ((mode (if (dreg? opnd2) 36992 37248))
               (reg (if (dreg? opnd2) (dreg-num opnd2) (dreg-num opnd1)))
               (other (if (dreg? opnd2) opnd1 opnd2)))
           (asm-word (SFX+ mode (SFX+ (SFX* reg 512) (opnd->mode/reg other))))
           (if (dreg? opnd2)
               (opnd-ext-rd-long other)
               (opnd-ext-wr-long other)))))
  (if ofile-asm?
      (emit-asm "subl" ofile-tab (opnd-str opnd1) "," (opnd-str opnd2))))
(define (emit-sub.w opnd1 opnd2) (set-bbv-version-limit! #f) 
  (cond ((areg? opnd2)
         (asm-word
          (SFX+ 37056 (SFX+ (SFX* (areg-num opnd2) 512) (opnd->mode/reg opnd1))))
         (opnd-ext-rd-word opnd1))
        ((imm? opnd1)
         (asm-word (SFX+ 1088 (opnd->mode/reg opnd2)))
         (opnd-ext-rd-word opnd1)
         (opnd-ext-wr-word opnd2))
        (else
         (let ((mode (if (dreg? opnd2) 36928 37184))
               (reg (if (dreg? opnd2) (dreg-num opnd2) (dreg-num opnd1)))
               (other (if (dreg? opnd2) opnd1 opnd2)))
           (asm-word (SFX+ mode (SFX+ (SFX* reg 512) (opnd->mode/reg other))))
           (if (dreg? opnd2)
               (opnd-ext-rd-word other)
               (opnd-ext-wr-word other)))))
  (if ofile-asm?
      (emit-asm "subw" ofile-tab (opnd-str opnd1) "," (opnd-str opnd2))))
(define (emit-asl.l opnd1 opnd2) (set-bbv-version-limit! #f) 
  (if (dreg? opnd1)
      (asm-word (SFX+ 57760 (SFX+ (SFX* (dreg-num opnd1) 512) (dreg-num opnd2))))
      (let ((n (imm-val opnd1)))
        (asm-word (SFX+ 57728 (SFX+ (SFX* (if (SFX= n 8) 0 n) 512) (dreg-num opnd2))))))
  (if ofile-asm?
      (emit-asm "asll" ofile-tab (opnd-str opnd1) "," (opnd-str opnd2))))
(define (emit-asl.w opnd1 opnd2) (set-bbv-version-limit! #f) 
  (if (dreg? opnd1)
      (asm-word (SFX+ 57696 (SFX+ (SFX* (dreg-num opnd1) 512) (dreg-num opnd2))))
      (let ((n (imm-val opnd1)))
        (asm-word (SFX+ 57664 (SFX+ (SFX* (if (SFX= n 8) 0 n) 512) (dreg-num opnd2))))))
  (if ofile-asm?
      (emit-asm "aslw" ofile-tab (opnd-str opnd1) "," (opnd-str opnd2))))
(define (emit-asr.l opnd1 opnd2) (set-bbv-version-limit! #f) 
  (if (dreg? opnd1)
      (asm-word (SFX+ 57504 (SFX+ (SFX* (dreg-num opnd1) 512) (dreg-num opnd2))))
      (let ((n (imm-val opnd1)))
        (asm-word (SFX+ 57472 (SFX+ (SFX* (if (SFX= n 8) 0 n) 512) (dreg-num opnd2))))))
  (if ofile-asm?
      (emit-asm "asrl" ofile-tab (opnd-str opnd1) "," (opnd-str opnd2))))
(define (emit-asr.w opnd1 opnd2) (set-bbv-version-limit! #f) 
  (if (dreg? opnd1)
      (asm-word (SFX+ 57440 (SFX+ (SFX* (dreg-num opnd1) 512) (dreg-num opnd2))))
      (let ((n (imm-val opnd1)))
        (asm-word (SFX+ 57408 (SFX+ (SFX* (if (SFX= n 8) 0 n) 512) (dreg-num opnd2))))))
  (if ofile-asm?
      (emit-asm "asrw" ofile-tab (opnd-str opnd1) "," (opnd-str opnd2))))
(define (emit-lsl.l opnd1 opnd2) (set-bbv-version-limit! #f) 
  (if (dreg? opnd1)
      (asm-word (SFX+ 57768 (SFX+ (SFX* (dreg-num opnd1) 512) (dreg-num opnd2))))
      (let ((n (imm-val opnd1)))
        (asm-word (SFX+ 57736 (SFX+ (SFX* (if (SFX= n 8) 0 n) 512) (dreg-num opnd2))))))
  (if ofile-asm?
      (emit-asm "lsll" ofile-tab (opnd-str opnd1) "," (opnd-str opnd2))))
(define (emit-lsr.l opnd1 opnd2) (set-bbv-version-limit! #f) 
  (if (dreg? opnd1)
      (asm-word (SFX+ 57512 (SFX+ (SFX* (dreg-num opnd1) 512) (dreg-num opnd2))))
      (let ((n (imm-val opnd1)))
        (asm-word (SFX+ 57480 (SFX+ (SFX* (if (SFX= n 8) 0 n) 512) (dreg-num opnd2))))))
  (if ofile-asm?
      (emit-asm "lsrl" ofile-tab (opnd-str opnd1) "," (opnd-str opnd2))))
(define (emit-lsr.w opnd1 opnd2) (set-bbv-version-limit! #f) 
  (if (dreg? opnd1)
      (asm-word (SFX+ 57448 (SFX+ (SFX* (dreg-num opnd1) 512) (dreg-num opnd2))))
      (let ((n (imm-val opnd1)))
        (asm-word (SFX+ 57416 (SFX+ (SFX* (if (SFX= n 8) 0 n) 512) (dreg-num opnd2))))))
  (if ofile-asm?
      (emit-asm "lsrw" ofile-tab (opnd-str opnd1) "," (opnd-str opnd2))))
(define (emit-clr.l opnd) (set-bbv-version-limit! #f) 
  (asm-word (SFX+ 17024 (opnd->mode/reg opnd)))
  (opnd-ext-wr-long opnd)
  (if ofile-asm? (emit-asm "clrl" ofile-tab (opnd-str opnd))))
(define (emit-neg.l opnd) (set-bbv-version-limit! #f) 
  (asm-word (SFX+ 17536 (opnd->mode/reg opnd)))
  (opnd-ext-wr-long opnd)
  (if ofile-asm? (emit-asm "negl" ofile-tab (opnd-str opnd))))
(define (emit-not.l opnd) (set-bbv-version-limit! #f) 
  (asm-word (SFX+ 18048 (opnd->mode/reg opnd)))
  (opnd-ext-wr-long opnd)
  (if ofile-asm? (emit-asm "notl" ofile-tab (opnd-str opnd))))
(define (emit-ext.l opnd) (set-bbv-version-limit! #f) 
  (asm-word (SFX+ 18624 (dreg-num opnd)))
  (if ofile-asm? (emit-asm "extl" ofile-tab (opnd-str opnd))))
(define (emit-ext.w opnd) (set-bbv-version-limit! #f) 
  (asm-word (SFX+ 18560 (dreg-num opnd)))
  (if ofile-asm? (emit-asm "extw" ofile-tab (opnd-str opnd))))
(define (emit-swap opnd) (set-bbv-version-limit! #f) 
  (asm-word (SFX+ 18496 (dreg-num opnd)))
  (if ofile-asm? (emit-asm "swap" ofile-tab (opnd-str opnd))))
(define (emit-cmp.l opnd1 opnd2) (set-bbv-version-limit! #f) 
  (cond ((areg? opnd2)
         (asm-word
          (SFX+ 45504 (SFX+ (SFX* (areg-num opnd2) 512) (opnd->mode/reg opnd1))))
         (opnd-ext-rd-long opnd1))
        ((imm? opnd1)
         (asm-word (SFX+ 3200 (opnd->mode/reg opnd2)))
         (opnd-ext-rd-long opnd1)
         (opnd-ext-rd-long opnd2))
        (else
         (asm-word
          (SFX+ 45184 (SFX+ (SFX* (dreg-num opnd2) 512) (opnd->mode/reg opnd1))))
         (opnd-ext-rd-long opnd1)))
  (if ofile-asm?
      (emit-asm "cmpl" ofile-tab (opnd-str opnd1) "," (opnd-str opnd2))))
(define (emit-cmp.w opnd1 opnd2) (set-bbv-version-limit! #f) 
  (cond ((areg? opnd2)
         (asm-word
          (SFX+ 45248 (SFX+ (SFX* (areg-num opnd2) 512) (opnd->mode/reg opnd1))))
         (opnd-ext-rd-word opnd1))
        ((imm? opnd1)
         (asm-word (SFX+ 3136 (opnd->mode/reg opnd2)))
         (opnd-ext-rd-word opnd1)
         (opnd-ext-rd-word opnd2))
        (else
         (asm-word
          (SFX+ 45120 (SFX+ (SFX* (dreg-num opnd2) 512) (opnd->mode/reg opnd1))))
         (opnd-ext-rd-word opnd1)))
  (if ofile-asm?
      (emit-asm "cmpw" ofile-tab (opnd-str opnd1) "," (opnd-str opnd2))))
(define (emit-cmp.b opnd1 opnd2) (set-bbv-version-limit! #f) 
  (cond ((imm? opnd1)
         (asm-word (SFX+ 3072 (opnd->mode/reg opnd2)))
         (opnd-ext-rd-word opnd1)
         (opnd-ext-rd-word opnd2))
        (else
         (asm-word
          (SFX+ 45056 (SFX+ (SFX* (dreg-num opnd2) 512) (opnd->mode/reg opnd1))))
         (opnd-ext-rd-word opnd1)))
  (if ofile-asm?
      (emit-asm "cmpb" ofile-tab (opnd-str opnd1) "," (opnd-str opnd2))))
(define (emit-tst.l opnd) (set-bbv-version-limit! #f) 
  (asm-word (SFX+ 19072 (opnd->mode/reg opnd)))
  (opnd-ext-rd-long opnd)
  (if ofile-asm? (emit-asm "tstl" ofile-tab (opnd-str opnd))))
(define (emit-tst.w opnd) (set-bbv-version-limit! #f) 
  (asm-word (SFX+ 19008 (opnd->mode/reg opnd)))
  (opnd-ext-rd-word opnd)
  (if ofile-asm? (emit-asm "tstw" ofile-tab (opnd-str opnd))))
(define (emit-lea opnd areg) (set-bbv-version-limit! #f) 
  (asm-word (SFX+ 16832 (SFX+ (SFX* (areg-num areg) 512) (opnd->mode/reg opnd))))
  (opnd-ext-rd-long opnd)
  (if ofile-asm?
      (emit-asm "lea" ofile-tab (opnd-str opnd) "," (opnd-str areg))))
(define (emit-unlk areg) (set-bbv-version-limit! #f) 
  (asm-word (SFX+ 20056 (areg-num areg)))
  (if ofile-asm? (emit-asm "unlk" ofile-tab (opnd-str areg))))
(define (emit-move-proc num opnd) (set-bbv-version-limit! #f) 
  (let ((dst (opnd->reg/mode opnd)))
    (asm-word (SFX+ 8192 (SFX+ dst 60)))
    (asm-proc-ref num 0)
    (opnd-ext-wr-long opnd)
    (if ofile-asm? (emit-asm "MOVE_PROC(" num "," (opnd-str opnd) ")"))))
(define (emit-move-prim val opnd) (set-bbv-version-limit! #f) 
  (let ((dst (opnd->reg/mode opnd)))
    (asm-word (SFX+ 8192 (SFX+ dst 60)))
    (asm-prim-ref val 0)
    (opnd-ext-wr-long opnd)
    (if ofile-asm?
        (emit-asm "MOVE_PRIM(" (proc-obj-name val) "," (opnd-str opnd) ")"))))
(define (emit-pea opnd) (set-bbv-version-limit! #f) 
  (asm-word (SFX+ 18496 (opnd->mode/reg opnd)))
  (opnd-ext-rd-long opnd)
  (if ofile-asm? (emit-asm "pea" ofile-tab (opnd-str opnd))))
(define (emit-pea* n) (set-bbv-version-limit! #f) 
  (asm-word 18552)
  (asm-word n)
  (if ofile-asm? (emit-asm "pea" ofile-tab n)))
(define (emit-btst opnd1 opnd2) (set-bbv-version-limit! #f) 
  (asm-word (SFX+ 256 (SFX+ (SFX* (dreg-num opnd1) 512) (opnd->mode/reg opnd2))))
  (opnd-ext-rd-word opnd2)
  (if ofile-asm?
      (emit-asm "btst" ofile-tab (opnd-str opnd1) "," (opnd-str opnd2))))
(define (emit-bra lbl) (set-bbv-version-limit! #f) 
  (asm-brel 24576 lbl)
  (if ofile-asm? (emit-asm "bra" ofile-tab "L" lbl)))
(define (emit-bcc lbl) (set-bbv-version-limit! #f) 
  (asm-brel 25600 lbl)
  (if ofile-asm? (emit-asm "bcc" ofile-tab "L" lbl)))
(define (emit-bcs lbl) (set-bbv-version-limit! #f) 
  (asm-brel 25856 lbl)
  (if ofile-asm? (emit-asm "bcs" ofile-tab "L" lbl)))
(define (emit-bhi lbl) (set-bbv-version-limit! #f) 
  (asm-brel 25088 lbl)
  (if ofile-asm? (emit-asm "bhi" ofile-tab "L" lbl)))
(define (emit-bls lbl) (set-bbv-version-limit! #f) 
  (asm-brel 25344 lbl)
  (if ofile-asm? (emit-asm "bls" ofile-tab "L" lbl)))
(define (emit-bmi lbl) (set-bbv-version-limit! #f) 
  (asm-brel 27392 lbl)
  (if ofile-asm? (emit-asm "bmi" ofile-tab "L" lbl)))
(define (emit-bpl lbl) (set-bbv-version-limit! #f) 
  (asm-brel 27136 lbl)
  (if ofile-asm? (emit-asm "bpl" ofile-tab "L" lbl)))
(define (emit-beq lbl) (set-bbv-version-limit! #f) 
  (asm-brel 26368 lbl)
  (if ofile-asm? (emit-asm "beq" ofile-tab "L" lbl)))
(define (emit-bne lbl) (set-bbv-version-limit! #f) 
  (asm-brel 26112 lbl)
  (if ofile-asm? (emit-asm "bne" ofile-tab "L" lbl)))
(define (emit-blt lbl) (set-bbv-version-limit! #f) 
  (asm-brel 27904 lbl)
  (if ofile-asm? (emit-asm "blt" ofile-tab "L" lbl)))
(define (emit-bgt lbl) (set-bbv-version-limit! #f) 
  (asm-brel 28160 lbl)
  (if ofile-asm? (emit-asm "bgt" ofile-tab "L" lbl)))
(define (emit-ble lbl) (set-bbv-version-limit! #f) 
  (asm-brel 28416 lbl)
  (if ofile-asm? (emit-asm "ble" ofile-tab "L" lbl)))
(define (emit-bge lbl) (set-bbv-version-limit! #f) 
  (asm-brel 27648 lbl)
  (if ofile-asm? (emit-asm "bge" ofile-tab "L" lbl)))
(define (emit-dbra dreg lbl) (set-bbv-version-limit! #f) 
  (asm-word (SFX+ 20936 dreg))
  (asm-wrel lbl 0)
  (if ofile-asm? (emit-asm "dbra" ofile-tab (opnd-str dreg) ",L" lbl)))
(define (emit-trap num) (set-bbv-version-limit! #f) 
  (asm-word (SFX+ 20032 num))
  (if ofile-asm? (emit-asm "trap" ofile-tab "#" num)))
(define (emit-trap1 num args) (set-bbv-version-limit! #f) 
  (asm-word (SFX+ 20136 (areg-num table-reg)))
  (asm-word (trap-offset num))
  (let loop ((args args))
    (if (not (null? args)) (begin (asm-word (Scar args)) (loop (Scdr args)))))
  (if ofile-asm?
      (let ()
        (define (words l) (set-bbv-version-limit! #f) 
          (if (null? l) (list ")") (cons "," (cons (Scar l) (words (Scdr l))))))
        (apply emit-asm (cons "TRAP1(" (cons num (words args)))))))
(define (emit-trap2 num args) (set-bbv-version-limit! #f) 
  (asm-word (SFX+ 20136 (areg-num table-reg)))
  (asm-word (trap-offset num))
  (asm-align 8 (SFXmodulo (SFX- 4 (SFX* (Slength args) 2)) 8))
  (let loop ((args args))
    (if (not (null? args)) (begin (asm-word (Scar args)) (loop (Scdr args)))))
  (if ofile-asm?
      (let ()
        (define (words l) (set-bbv-version-limit! #f) 
          (if (null? l) (list ")") (cons "," (cons (Scar l) (words (Scdr l))))))
        (apply emit-asm (cons "TRAP2(" (cons num (words args)))))))
(define (emit-trap3 num) (set-bbv-version-limit! #f) 
  (asm-word (SFX+ 20200 (areg-num table-reg)))
  (asm-word (trap-offset num))
  (if ofile-asm? (emit-asm "TRAP3(" num ")")))
(define (emit-rts) (set-bbv-version-limit! #f)  (asm-word 20085) (if ofile-asm? (emit-asm "rts")))
(define (emit-nop) (set-bbv-version-limit! #f)  (asm-word 20081) (if ofile-asm? (emit-asm "nop")))
(define (emit-jmp opnd) (set-bbv-version-limit! #f) 
  (asm-word (SFX+ 20160 (opnd->mode/reg opnd)))
  (opnd-ext-rd-long opnd)
  (if ofile-asm? (emit-asm "jmp" ofile-tab (opnd-str opnd))))
(define (emit-jmp-glob glob) (set-bbv-version-limit! #f) 
  (asm-word 8814)
  (asm-ref-glob-jump glob)
  (asm-word 20177)
  (if ofile-asm? (emit-asm "JMP_GLOB(" (glob-name glob) ")")))
(define (emit-jmp-proc num offset) (set-bbv-version-limit! #f) 
  (asm-word 20217)
  (asm-proc-ref num offset)
  (if ofile-asm? (emit-asm "JMP_PROC(" num "," offset ")")))
(define (emit-jmp-prim val offset) (set-bbv-version-limit! #f) 
  (asm-word 20217)
  (asm-prim-ref val offset)
  (if ofile-asm? (emit-asm "JMP_PRIM(" (proc-obj-name val) "," offset ")")))
(define (emit-jsr opnd) (set-bbv-version-limit! #f) 
  (asm-word (SFX+ 20096 (opnd->mode/reg opnd)))
  (opnd-ext-rd-long opnd)
  (if ofile-asm? (emit-asm "jsr" ofile-tab (opnd-str opnd))))
(define (emit-word n) (set-bbv-version-limit! #f) 
  (asm-word n)
  (if ofile-asm? (emit-asm ".word" ofile-tab n)))
(define (emit-label lbl) (set-bbv-version-limit! #f) 
  (asm-label lbl #f)
  (if ofile-asm? (emit-asm* "L" lbl ":")))
(define (emit-label-subproc lbl parent-lbl label-descr) (set-bbv-version-limit! #f) 
  (asm-align 8 0)
  (asm-wrel parent-lbl (SFX- 32768 type-procedure))
  (asm-label lbl label-descr)
  (if ofile-asm?
      (begin (emit-asm "SUBPROC(L" parent-lbl ")") (emit-asm* "L" lbl ":"))))
(define (emit-label-return lbl parent-lbl fs link label-descr) (set-bbv-version-limit! #f) 
  (asm-align 8 4)
  (asm-word (SFX* fs 4))
  (asm-word (SFX* (SFX- fs link) 4))
  (asm-wrel parent-lbl (SFX- 32768 type-procedure))
  (asm-label lbl label-descr)
  (if ofile-asm?
      (begin
        (emit-asm "RETURN(L" parent-lbl "," fs "," link ")")
        (emit-asm* "L" lbl ":"))))
(define (emit-label-task-return lbl parent-lbl fs link label-descr) (set-bbv-version-limit! #f) 
  (asm-align 8 4)
  (asm-word (SFX+ 32768 (SFX* fs 4)))
  (asm-word (SFX* (SFX- fs link) 4))
  (asm-wrel parent-lbl (SFX- 32768 type-procedure))
  (asm-label lbl label-descr)
  (if ofile-asm?
      (begin
        (emit-asm "TASK_RETURN(L" parent-lbl "," fs "," link ")")
        (emit-asm* "L" lbl ":"))))
(define (emit-lbl-ptr lbl) (set-bbv-version-limit! #f) 
  (asm-wrel lbl 0)
  (if ofile-asm? (emit-asm "LBL_PTR(L" lbl ")")))
(define (emit-set-glob glob) (set-bbv-version-limit! #f) 
  (asm-set-glob glob)
  (if ofile-asm? (emit-asm "SET_GLOB(" (glob-name glob) ")")))
(define (emit-const obj) (set-bbv-version-limit! #f) 
  (let ((n (pos-in-list obj (queue->list asm-const-queue))))
    (if n
        (make-pcr const-lbl (SFX* n 4))
        (let ((m (Slength (queue->list asm-const-queue))))
          (queue-put! asm-const-queue obj)
          (make-pcr const-lbl (SFX* m 4))))))
(define (emit-stat stat) (set-bbv-version-limit! #f) 
  (asm-word 21177)
  (asm-stat stat)
  (if ofile-asm? (emit-asm "STAT(" stat ")")))
(define (emit-asm . l) (set-bbv-version-limit! #f)  (asm-comment (cons ofile-tab l)))
(define (emit-asm* . l) (set-bbv-version-limit! #f)  (asm-comment l))
(define (emit-muls.l opnd1 opnd2) (set-bbv-version-limit! #f) 
  (asm-m68020-proc)
  (asm-word (SFX+ 19456 (opnd->mode/reg opnd1)))
  (asm-word (SFX+ 2048 (SFX* (dreg-num opnd2) 4096)))
  (opnd-ext-rd-long opnd1)
  (if ofile-asm?
      (emit-asm "mulsl" ofile-tab (opnd-str opnd1) "," (opnd-str opnd2))))
(define (emit-divsl.l opnd1 opnd2 opnd3) (set-bbv-version-limit! #f) 
  (asm-m68020-proc)
  (asm-word (SFX+ 19520 (opnd->mode/reg opnd1)))
  (asm-word (SFX+ 2048 (SFX+ (SFX* (dreg-num opnd3) 4096) (dreg-num opnd2))))
  (opnd-ext-rd-long opnd1)
  (if ofile-asm?
      (emit-asm
       "divsll"
       ofile-tab
       (opnd-str opnd1)
       ","
       (opnd-str opnd2)
       ":"
       (opnd-str opnd3))))
(define (emit-fint.dx opnd1 opnd2) (set-bbv-version-limit! #f)  (emit-fop.dx "int" 1 opnd1 opnd2))
(define (emit-fsinh.dx opnd1 opnd2) (set-bbv-version-limit! #f)  (emit-fop.dx "sinh" 2 opnd1 opnd2))
(define (emit-fintrz.dx opnd1 opnd2) (set-bbv-version-limit! #f)  (emit-fop.dx "intrz" 3 opnd1 opnd2))
(define (emit-fsqrt.dx opnd1 opnd2) (set-bbv-version-limit! #f)  (emit-fop.dx "sqrt" 4 opnd1 opnd2))
(define (emit-flognp1.dx opnd1 opnd2) (set-bbv-version-limit! #f)  (emit-fop.dx "lognp1" 6 opnd1 opnd2))
(define (emit-fetoxm1.dx opnd1 opnd2) (set-bbv-version-limit! #f)  (emit-fop.dx "etoxm1" 8 opnd1 opnd2))
(define (emit-ftanh.dx opnd1 opnd2) (set-bbv-version-limit! #f)  (emit-fop.dx "tanh" 9 opnd1 opnd2))
(define (emit-fatan.dx opnd1 opnd2) (set-bbv-version-limit! #f)  (emit-fop.dx "atan" 10 opnd1 opnd2))
(define (emit-fasin.dx opnd1 opnd2) (set-bbv-version-limit! #f)  (emit-fop.dx "asin" 12 opnd1 opnd2))
(define (emit-fatanh.dx opnd1 opnd2) (set-bbv-version-limit! #f)  (emit-fop.dx "atanh" 13 opnd1 opnd2))
(define (emit-fsin.dx opnd1 opnd2) (set-bbv-version-limit! #f)  (emit-fop.dx "sin" 14 opnd1 opnd2))
(define (emit-ftan.dx opnd1 opnd2) (set-bbv-version-limit! #f)  (emit-fop.dx "tan" 15 opnd1 opnd2))
(define (emit-fetox.dx opnd1 opnd2) (set-bbv-version-limit! #f)  (emit-fop.dx "etox" 16 opnd1 opnd2))
(define (emit-ftwotox.dx opnd1 opnd2) (set-bbv-version-limit! #f)  (emit-fop.dx "twotox" 17 opnd1 opnd2))
(define (emit-ftentox.dx opnd1 opnd2) (set-bbv-version-limit! #f)  (emit-fop.dx "tentox" 18 opnd1 opnd2))
(define (emit-flogn.dx opnd1 opnd2) (set-bbv-version-limit! #f)  (emit-fop.dx "logn" 20 opnd1 opnd2))
(define (emit-flog10.dx opnd1 opnd2) (set-bbv-version-limit! #f)  (emit-fop.dx "log10" 21 opnd1 opnd2))
(define (emit-flog2.dx opnd1 opnd2) (set-bbv-version-limit! #f)  (emit-fop.dx "log2" 22 opnd1 opnd2))
(define (emit-fabs.dx opnd1 opnd2) (set-bbv-version-limit! #f)  (emit-fop.dx "abs" 24 opnd1 opnd2))
(define (emit-fcosh.dx opnd1 opnd2) (set-bbv-version-limit! #f)  (emit-fop.dx "cosh" 25 opnd1 opnd2))
(define (emit-fneg.dx opnd1 opnd2) (set-bbv-version-limit! #f)  (emit-fop.dx "neg" 26 opnd1 opnd2))
(define (emit-facos.dx opnd1 opnd2) (set-bbv-version-limit! #f)  (emit-fop.dx "acos" 28 opnd1 opnd2))
(define (emit-fcos.dx opnd1 opnd2) (set-bbv-version-limit! #f)  (emit-fop.dx "cos" 29 opnd1 opnd2))
(define (emit-fgetexp.dx opnd1 opnd2) (set-bbv-version-limit! #f)  (emit-fop.dx "getexp" 30 opnd1 opnd2))
(define (emit-fgetman.dx opnd1 opnd2) (set-bbv-version-limit! #f)  (emit-fop.dx "getman" 31 opnd1 opnd2))
(define (emit-fdiv.dx opnd1 opnd2) (set-bbv-version-limit! #f)  (emit-fop.dx "div" 32 opnd1 opnd2))
(define (emit-fmod.dx opnd1 opnd2) (set-bbv-version-limit! #f)  (emit-fop.dx "mod" 33 opnd1 opnd2))
(define (emit-fadd.dx opnd1 opnd2) (set-bbv-version-limit! #f)  (emit-fop.dx "add" 34 opnd1 opnd2))
(define (emit-fmul.dx opnd1 opnd2) (set-bbv-version-limit! #f)  (emit-fop.dx "mul" 35 opnd1 opnd2))
(define (emit-fsgldiv.dx opnd1 opnd2) (set-bbv-version-limit! #f)  (emit-fop.dx "sgldiv" 36 opnd1 opnd2))
(define (emit-frem.dx opnd1 opnd2) (set-bbv-version-limit! #f)  (emit-fop.dx "rem" 37 opnd1 opnd2))
(define (emit-fscale.dx opnd1 opnd2) (set-bbv-version-limit! #f)  (emit-fop.dx "scale" 38 opnd1 opnd2))
(define (emit-fsglmul.dx opnd1 opnd2) (set-bbv-version-limit! #f)  (emit-fop.dx "sglmul" 39 opnd1 opnd2))
(define (emit-fsub.dx opnd1 opnd2) (set-bbv-version-limit! #f)  (emit-fop.dx "sub" 40 opnd1 opnd2))
(define (emit-fcmp.dx opnd1 opnd2) (set-bbv-version-limit! #f)  (emit-fop.dx "cmp" 56 opnd1 opnd2))
(define (emit-fop.dx name code opnd1 opnd2) (set-bbv-version-limit! #f) 
  (asm-m68881-proc)
  (asm-word (SFX+ 61952 (opnd->mode/reg opnd1)))
  (asm-word
   (SFX+ (if (freg? opnd1) (SFX* (freg-num opnd1) 1024) 21504)
      (SFX+ (SFX* (freg-num opnd2) 128)
            code)))
  (opnd-ext-rd-long opnd1)
  (if ofile-asm?
      (emit-asm
       "f"
       name
       (if (freg? opnd1) "x" "d")
       ofile-tab
       (opnd-str opnd1)
       ","
       (opnd-str opnd2))))
(define (emit-fmov.dx opnd1 opnd2) (set-bbv-version-limit! #f) 
  (emit-fmov
   (if (and (freg? opnd1) (freg? opnd2)) (SFX* (freg-num opnd1) 1024) 21504)
   opnd1
   opnd2)
  (if ofile-asm?
      (emit-asm
       (if (and (freg? opnd1) (freg? opnd2)) "fmovex" "fmoved")
       ofile-tab
       (opnd-str opnd1)
       ","
       (opnd-str opnd2))))
(define (emit-fmov.l opnd1 opnd2) (set-bbv-version-limit! #f) 
  (emit-fmov 16384 opnd1 opnd2)
  (if ofile-asm?
      (emit-asm "fmovel" ofile-tab (opnd-str opnd1) "," (opnd-str opnd2))))
(define (emit-fmov code opnd1 opnd2) (set-bbv-version-limit! #f) 
  (define (fmov code opnd1 opnd2) (set-bbv-version-limit! #f) 
    (asm-m68881-proc)
    (asm-word (SFX+ 61952 (opnd->mode/reg opnd1)))
    (asm-word (SFX+ (SFX* (freg-num opnd2) 128) code))
    (opnd-ext-rd-long opnd1))
  (if (freg? opnd2) (fmov code opnd1 opnd2) (fmov (SFX+ code 8192) opnd2 opnd1)))
(define (emit-fbeq lbl) (set-bbv-version-limit! #f) 
  (asm-m68881-proc)
  (asm-word 62081)
  (asm-wrel lbl 0)
  (if ofile-asm? (emit-asm "fbeq" ofile-tab "L" lbl)))
(define (emit-fbne lbl) (set-bbv-version-limit! #f) 
  (asm-m68881-proc)
  (asm-word 62094)
  (asm-wrel lbl 0)
  (if ofile-asm? (emit-asm "fbne" ofile-tab "L" lbl)))
(define (emit-fblt lbl) (set-bbv-version-limit! #f) 
  (asm-m68881-proc)
  (asm-word 62100)
  (asm-wrel lbl 0)
  (if ofile-asm? (emit-asm "fblt" ofile-tab "L" lbl)))
(define (emit-fbgt lbl) (set-bbv-version-limit! #f) 
  (asm-m68881-proc)
  (asm-word 62098)
  (asm-wrel lbl 0)
  (if ofile-asm? (emit-asm "fbgt" ofile-tab "L" lbl)))
(define (emit-fble lbl) (set-bbv-version-limit! #f) 
  (asm-m68881-proc)
  (asm-word 62101)
  (asm-wrel lbl 0)
  (if ofile-asm? (emit-asm "fble" ofile-tab "L" lbl)))
(define (emit-fbge lbl) (set-bbv-version-limit! #f) 
  (asm-m68881-proc)
  (asm-word 62099)
  (asm-wrel lbl 0)
  (if ofile-asm? (emit-asm "fbge" ofile-tab "L" lbl)))
(define (opnd->mode/reg opnd) (set-bbv-version-limit! #f) 
  (cond ((disp? opnd) (SFX+ 32 (disp-areg opnd)))
        ((inx? opnd) (SFX+ 40 (inx-areg opnd)))
        ((pcr? opnd) 58)
        ((imm? opnd) 60)
        ((glob? opnd) (SFX+ 32 table-reg))
        ((freg? opnd) 0)
        (else opnd)))
(define (opnd->reg/mode opnd) (set-bbv-version-limit! #f) 
  (let ((x (opnd->mode/reg opnd)))
    (SFX* (SFX+ (SFX* 8 (SFXremainder x 8)) (SFXquotient x 8)) 64)))
(define (opnd-ext-rd-long opnd) (set-bbv-version-limit! #f)  (opnd-extension opnd #f #f))
(define (opnd-ext-rd-word opnd) (set-bbv-version-limit! #f)  (opnd-extension opnd #f #t))
(define (opnd-ext-wr-long opnd) (set-bbv-version-limit! #f)  (opnd-extension opnd #t #f))
(define (opnd-ext-wr-word opnd) (set-bbv-version-limit! #f)  (opnd-extension opnd #t #t))
(define (opnd-extension opnd write? word?) (set-bbv-version-limit! #f) 
  (cond ((disp? opnd) (asm-word (disp-offset opnd)))
        ((inx? opnd)
         (asm-word
          (SFX+ (SFX+ (SFX* (inx-ireg opnd) 4096) 2048)
             (SFXmodulo (inx-offset opnd) 256))))
        ((pcr? opnd) (asm-wrel (pcr-lbl opnd) (pcr-offset opnd)))
        ((imm? opnd)
         (if word? (asm-word (imm-val opnd)) (asm-long (imm-val opnd))))
        ((glob? opnd) (if write? (asm-set-glob opnd) (asm-ref-glob opnd)))))
(define (opnd-str opnd) (set-bbv-version-limit! #f) 
  (cond ((dreg? opnd)
         (Svector-ref
          '#("d0" "d1" "d2" "d3" "d4" "d5" "d6" "d7")
          (dreg-num opnd)))
        ((areg? opnd)
         (Svector-ref
          '#("a0" "a1" "a2" "a3" "a4" "a5" "a6" "sp")
          (areg-num opnd)))
        ((ind? opnd)
         (Svector-ref
          '#("a0@" "a1@" "a2@" "a3@" "a4@" "a5@" "a6@" "sp@")
          (areg-num (ind-areg opnd))))
        ((pinc? opnd)
         (Svector-ref
          '#("a0@+" "a1@+" "a2@+" "a3@+" "a4@+" "a5@+" "a6@+" "sp@+")
          (areg-num (pinc-areg opnd))))
        ((pdec? opnd)
         (Svector-ref
          '#("a0@-" "a1@-" "a2@-" "a3@-" "a4@-" "a5@-" "a6@-" "sp@-")
          (areg-num (pdec-areg opnd))))
        ((disp? opnd)
         (Sstring-append
          (opnd-str (disp-areg opnd))
          "@("
          (SFXnumber->string (disp-offset opnd))
          ")"))
        ((inx? opnd)
         (Sstring-append
          (opnd-str (inx-areg opnd))
          "@("
          (SFXnumber->string (inx-offset opnd))
          ","
          (opnd-str (inx-ireg opnd))
          ":l)"))
        ((pcr? opnd)
         (let ((lbl (pcr-lbl opnd)) (offs (pcr-offset opnd)))
           (if (SFX= offs 0)
               (Sstring-append "L" (SFXnumber->string lbl))
               (Sstring-append
                "L"
                (SFXnumber->string lbl)
                "+"
                (SFXnumber->string offs)))))
        ((imm? opnd) (Sstring-append "#" (SFXnumber->string (imm-val opnd))))
        ((glob? opnd)
         (Sstring-append "GLOB(" (Ssymbol->string (glob-name opnd)) ")"))
        ((freg? opnd)
         (Svector-ref
          '#("fp0" "fp1" "fp2" "fp3" "fp4" "fp5" "fp6" "fp7")
          (freg-num opnd)))
        ((reg-list? opnd)
         (let loop ((l (reg-list-regs opnd)) (result "[") (sep ""))
           (if (pair? l)
               (loop (Scdr l) (Sstring-append result sep (opnd-str (Scar l))) "/")
               (Sstring-append result "]"))))
        (else (compiler-internal-error "opnd-str, unknown 'opnd'" opnd))))
(define (begin! info-port targ) (set-bbv-version-limit! #f) 
  (set! return-reg (make-reg 0))
  (target-end!-set! targ end!)
  (target-dump-set! targ dump)
  (target-nb-regs-set! targ nb-gvm-regs)
  (target-prim-info-set! targ prim-info)
  (target-label-info-set! targ label-info)
  (target-jump-info-set! targ jump-info)
  (target-proc-result-set! targ (make-reg 1))
  (target-task-return-set! targ return-reg)
  (set! *info-port* info-port)
  '())
(define (end!) (set-bbv-version-limit! #f)  '())
(define *info-port* '())
(define nb-gvm-regs 5)
(define nb-arg-regs 3)
(define pointer-size 4)
(define prim-proc-table
  (Smap2 (lambda (x) (set-bbv-version-limit! #f) 
         (cons (string->canonical-symbol (Scar x))
               (apply make-proc-obj (Scar x) #t #f (Scdr x))))
       prim-procs))
(define (prim-info name) (set-bbv-version-limit! #f) 
  (let ((x (Sassq name prim-proc-table))) (if x (Scdr x) #f)))
(define (get-prim-info name) (set-bbv-version-limit! #f) 
  (let ((proc (prim-info (string->canonical-symbol name))))
    (if proc
        proc
        (compiler-internal-error "get-prim-info, unknown primitive:" name))))
(define (label-info min-args nb-parms rest? closed?) (set-bbv-version-limit! #f) 
  (let ((nb-stacked (max 0 (SFX- nb-parms nb-arg-regs))))
    (define (location-of-parms i) (set-bbv-version-limit! #f) 
      (if (SFX> i nb-parms)
          '()
          (cons (cons i
                      (if (SFX> i nb-stacked)
                          (make-reg (SFX- i nb-stacked))
                          (make-stk i)))
                (location-of-parms (SFX+ i 1)))))
    (let ((x (cons (cons 'return 0) (location-of-parms 1))))
      (make-pcontext
       nb-stacked
       (if closed?
           (cons (cons 'closure-env (make-reg (SFX+ nb-arg-regs 1))) x)
           x)))))
(define (jump-info nb-args) (set-bbv-version-limit! #f) 
  (let ((nb-stacked (max 0 (SFX- nb-args nb-arg-regs))))
    (define (location-of-args i) (set-bbv-version-limit! #f) 
      (if (SFX> i nb-args)
          '()
          (cons (cons i
                      (if (SFX> i nb-stacked)
                          (make-reg (SFX- i nb-stacked))
                          (make-stk i)))
                (location-of-args (SFX+ i 1)))))
    (make-pcontext
     nb-stacked
     (cons (cons 'return (make-reg 0)) (location-of-args 1)))))
(define (closed-var-offset i) (set-bbv-version-limit! #f)  (SFX+ (SFX* i pointer-size) 2))
(define (dump proc filename c-intf options) (set-bbv-version-limit! #f) 
  (if *info-port*
      (begin (display "Dumping:" *info-port*) (newline *info-port*)))
  (set! ofile-asm? (Smemq 'asm options))
  (set! ofile-stats? (Smemq 'stats options))
  (set! debug-info? (Smemq 'debug options))
  (set! object-queue (queue-empty))
  (set! objects-dumped (queue-empty))
  (ofile.begin! filename add-object)
  (queue-put! object-queue proc)
  (queue-put! objects-dumped proc)
  (let loop ((index 0))
    (if (not (queue-empty? object-queue))
        (let ((obj (queue-get! object-queue)))
          (dump-object obj index)
          (loop (SFX+ index 1)))))
  (ofile.end!)
  (if *info-port* (newline *info-port*))
  (set! object-queue '())
  (set! objects-dumped '()))
(define debug-info? '())
(define object-queue '())
(define objects-dumped '())
(define (add-object obj) (set-bbv-version-limit! #f) 
  (if (and (proc-obj? obj) (not (proc-obj-code obj)))
      #f
      (let ((n (pos-in-list obj (queue->list objects-dumped))))
        (if n
            n
            (let ((m (Slength (queue->list objects-dumped))))
              (queue-put! objects-dumped obj)
              (queue-put! object-queue obj)
              m)))))
(define (dump-object obj index) (set-bbv-version-limit! #f) 
  (ofile-line "|------------------------------------------------------")
  (case (obj-type obj)
    ((pair) (dump-pair obj))
    ((flonum) (dump-flonum obj))
    ((subtyped)
     (case (obj-subtype obj)
       ((vector) (dump-vector obj))
       ((symbol) (dump-symbol obj))
;;       ((ratnum) (dump-ratnum obj))
;;       ((cpxnum) (dump-cpxnum obj))
       ((string) (dump-string obj))
       ((bignum) (dump-bignum obj))
       (else
        (compiler-internal-error
         "dump-object, can't dump object 'obj':"
         obj))))
    ((procedure) (dump-procedure obj))
    (else
     (compiler-internal-error "dump-object, can't dump object 'obj':" obj))))
(define (dump-pair pair) (set-bbv-version-limit! #f) 
  (ofile-long pair-prefix)
  (ofile-ref (Scdr pair))
  (ofile-ref (Scar pair)))
(define (dump-vector v) (set-bbv-version-limit! #f) 
  (ofile-long (SFX+ (SFX* (Svector-length v) 1024) (SFX* subtype-vector 8)))
  (let ((len (Svector-length v)))
    (let loop ((i 0))
      (if (SFX< i len) (begin (ofile-ref (Svector-ref v i)) (loop (SFX+ i 1)))))))
(define (dump-symbol sym) (set-bbv-version-limit! #f) 
  (compiler-internal-error "dump-symbol, can't dump SYMBOL type"))
;;(define (dump-ratnum x) (set-bbv-version-limit! #f) 
;;  (ofile-long (SFX+ (SFX* 2 1024) (SFX* subtype-ratnum 8)))
;;  (ofile-ref (numerator x))
;;  (ofile-ref (denominator x)))
;;(define (dump-cpxnum x) (set-bbv-version-limit! #f) 
;;  (ofile-long (SFX+ (SFX* 2 1024) (SFX* subtype-cpxnum 8)))
;;  (ofile-ref (real-part x))
;;  (ofile-ref (imag-part x)))
(define (dump-string s) (set-bbv-version-limit! #f) 
  (ofile-long (SFX+ (SFX* (SFX+ (Sstring-length s) 1) 256) (SFX* subtype-string 8)))
  (let ((len (Sstring-length s)))
    (define (ref i) (set-bbv-version-limit! #f)  (if (SFX>= i len) 0 (character-encoding (Sstring-ref s i))))
    (let loop ((i 0))
      (if (SFX<= i len)
          (begin
            (ofile-word (SFX+ (SFX* (ref i) 256) (ref (SFX+ i 1))))
            (loop (SFX+ i 2)))))))
(define (dump-flonum x) (set-bbv-version-limit! #f) 
  (let ((bits (flonum->bits x)))
    (ofile-long flonum-prefix)
    (ofile-long (GENquotient bits 4294967296))
    (ofile-long (GENmodulo bits 4294967296))))
(define (flonum->inexact-exponential-format x) (set-bbv-version-limit! #f) 
  (define (exp-form-pos x y i) (set-bbv-version-limit! #f) 
    (let ((i*2 (GEN+ i i)))
      (let ((z (if (and (not (GEN< flonum-e-bias i*2)) (not (GEN< x y)))
                   (exp-form-pos x (GEN* y y) i*2)
                   (cons x 0))))
        (let ((a (Scar z)) (b (Scdr z)))
          (let ((i+b (GEN+ i b)))
            (if (and (not (GEN< flonum-e-bias i+b)) (not (GEN< a y)))
                (begin (Sset-car! z (/ a y)) (Sset-cdr! z i+b)))
            z)))))
  (define (exp-form-neg x y i) (set-bbv-version-limit! #f) 
    (let ((i*2 (GEN+ i i)))
      (let ((z (if (and (GEN< i*2 flonum-e-bias-minus-1) (GEN< x y))
                   (exp-form-neg x (GEN* y y) i*2)
                   (cons x 0))))
        (let ((a (Scar z)) (b (Scdr z)))
          (let ((i+b (GEN+ i b)))
            (if (and (GEN< i+b flonum-e-bias-minus-1) (GEN< a y))
                (begin (Sset-car! z (/ a y)) (Sset-cdr! z i+b)))
            z)))))
  (define (exp-form x) (set-bbv-version-limit! #f) 
    (if (GEN< x inexact-+1)
        (let ((z (exp-form-neg x inexact-+1/2 1)))
          (Sset-car! z (GEN* inexact-+2 (Scar z)))
          (Sset-cdr! z (GEN- -1 (Scdr z)))
          z)
        (exp-form-pos x inexact-+2 1)))
  (if (negative? x)
      (let ((z (exp-form (GEN- inexact-0 x))))
        (Sset-car! z (GEN- inexact-0 (Scar z)))
        z)
      (exp-form x)))
(define (flonum->exact-exponential-format x) (set-bbv-version-limit! #f) 
  (let ((z (flonum->inexact-exponential-format x)))
    (let ((y (Scar z)))
      (cond ((not (GEN< y inexact-+2))
             (Sset-car! z flonum-+m-min)
             (Sset-cdr! z flonum-e-bias-plus-1))
            ((not (GEN< inexact--2 y))
             (Sset-car! z flonum--m-min)
             (Sset-cdr! z flonum-e-bias-plus-1))
            (else
             (Sset-car!
              z
              (truncate (inexact->exact (GEN* (Scar z) inexact-m-min))))))
      (Sset-cdr! z (GEN- (Scdr z) flonum-m-bits))
      z)))
(define (flonum->bits x) (set-bbv-version-limit! #f) 
  (define (bits a b) (set-bbv-version-limit! #f) 
    (if (GEN< a flonum-+m-min)
        a
        (GEN+ (GEN- a flonum-+m-min)
           (GEN* (GEN+ (GEN+ b flonum-m-bits) flonum-e-bias) flonum-+m-min))))
  (let ((z (flonum->exact-exponential-format x)))
    (let ((a (Scar z)) (b (Scdr z)))
      (if (negative? a) (GEN+ flonum-sign-bit (bits (GEN- 0 a) b)) (bits a b)))))
(define flonum-m-bits 52)
(define flonum-e-bits 11)
(define flonum-sign-bit 9223372036854775808)
(define flonum-+m-min 4503599627370496)
(define flonum--m-min -4503599627370496)
(define flonum-e-bias 1023)
(define flonum-e-bias-plus-1 1024)
(define flonum-e-bias-minus-1 1022)
(define inexact-m-min (exact->inexact flonum-+m-min))
(define inexact-+2 (exact->inexact 2))
(define inexact--2 (exact->inexact -2))
(define inexact-+1 (exact->inexact 1))
(define inexact-+1/2 (/ (exact->inexact 1) (exact->inexact 2)))
(define inexact-0 (exact->inexact 0))
(define (dump-bignum x) (set-bbv-version-limit! #f) 
  (define radix 16384)
  (define (integer->digits n) (set-bbv-version-limit! #f) 
    (if (GEN= n 0)
        '()
        (cons (GENremainder n radix) (integer->digits (GENquotient n radix)))))
  (let ((l (integer->digits (abs x))))
    (ofile-long (SFX+ (SFX* (SFX+ (Slength l) 1) 512) (SFX* subtype-bignum 8)))
    (if (GEN< x 0) (ofile-word 0) (ofile-word 1))
    (for-each ofile-word l)))
(define (dump-procedure proc) (set-bbv-version-limit! #f) 
  (let ((bbs (proc-obj-code proc)))
    (set! entry-lbl-num (bbs-entry-lbl-num bbs))
    (set! label-counter (bbs-lbl-counter bbs))
    (set! var-descr-queue (queue-empty))
    (set! first-class-label-queue (queue-empty))
    (set! deferred-code-queue (queue-empty))
    (if *info-port*
        (begin
          (display "  #[" *info-port*)
          (if (proc-obj-primitive? proc)
              (display "primitive " *info-port*)
              (display "procedure " *info-port*))
          (display (proc-obj-name proc) *info-port*)
          (display "]" *info-port*)))
    (if (proc-obj-primitive? proc)
        (ofile-prim-proc (proc-obj-name proc))
        (ofile-user-proc))
    (asm.begin!)
    (let loop ((prev-bb #f) (prev-gvm-instr #f) (l (bbs->code-list bbs)))
      (if (not (null? l))
          (let ((pres-bb (code-bb (Scar l)))
                (pres-gvm-instr (code-gvm-instr (Scar l)))
                (pres-slots-needed (code-slots-needed (Scar l)))
                (next-gvm-instr
                 (if (null? (Scdr l)) #f (code-gvm-instr (Scadr l)))))
            (if ofile-asm? (asm-comment (Scar l)))
            (gen-gvm-instr
             prev-gvm-instr
             pres-gvm-instr
             next-gvm-instr
             pres-slots-needed)
            (loop pres-bb pres-gvm-instr (Scdr l)))))
    (asm.end!
     (if debug-info?
         (vector (lst->vector (queue->list first-class-label-queue))
                 (lst->vector (queue->list var-descr-queue)))
         #f))
    (if *info-port* (newline *info-port*))
    (set! var-descr-queue '())
    (set! first-class-label-queue '())
    (set! deferred-code-queue '())
    (set! instr-source '())
    (set! entry-frame '())
    (set! exit-frame '())))
(define label-counter (lambda () 0))
(define entry-lbl-num '())
(define var-descr-queue '())
(define first-class-label-queue '())
(define deferred-code-queue '())
(define instr-source '())
(define entry-frame '())
(define exit-frame '())
(define (defer-code! thunk) (set-bbv-version-limit! #f)  (queue-put! deferred-code-queue thunk))
(define (gen-deferred-code!) (set-bbv-version-limit! #f) 
  (let loop ()
    (if (not (queue-empty? deferred-code-queue))
        (let ((thunk (queue-get! deferred-code-queue))) (thunk) (loop)))))
(define (add-var-descr! descr) (set-bbv-version-limit! #f) 
  (define (index x l) (set-bbv-version-limit! #f) 
    (let loop ((l l) (i 0))
      (cond ((not (pair? l)) #f)
            ((equal? (Scar l) x) i)
            (else (loop (Scdr l) (SFX+ i 1))))))
  (let ((n (index descr (queue->list var-descr-queue))))
    (if n
        n
        (let ((m (Slength (queue->list var-descr-queue))))
          (queue-put! var-descr-queue descr)
          m))))
(define (add-first-class-label! source slots frame) (set-bbv-version-limit! #f) 
  (let loop ((i 0) (l1 slots) (l2 '()))
    (if (pair? l1)
        (let ((var (Scar l1)))
          (let ((x (frame-live? var frame)))
            (if (and x (or (pair? x) (not (temp-var? x))))
                (let ((descr-index
                       (add-var-descr!
                        (if (pair? x)
                            (Smap2 (lambda (y) (set-bbv-version-limit! #f)  (add-var-descr! (var-name y))) x)
                            (var-name x)))))
                  (loop (SFX+ i 1)
                        (Scdr l1)
                        (cons (SFX+ (SFX* i 16384) descr-index) l2)))
                (loop (SFX+ i 1) (Scdr l1) l2))))
        (let ((label-descr (lst->vector (cons 0 (cons source l2)))))
          (queue-put! first-class-label-queue label-descr)
          label-descr))))
(define (gen-gvm-instr prev-gvm-instr gvm-instr next-gvm-instr sn) (set-bbv-version-limit! #f) 
  (set! instr-source (comment-get (gvm-instr-comment gvm-instr) 'source))
  (set! exit-frame (gvm-instr-frame gvm-instr))
  (set! entry-frame (and prev-gvm-instr (gvm-instr-frame prev-gvm-instr)))
  (case (gvm-instr-type gvm-instr)
    ((label)
     (set! entry-frame exit-frame)
     (set! current-fs (frame-size exit-frame))
     (case (label-type gvm-instr)
       ((simple) (gen-label-simple (label-lbl-num gvm-instr) sn))
       ((entry)
        (gen-label-entry
         (label-lbl-num gvm-instr)
         (label-entry-nb-parms gvm-instr)
         (label-entry-min gvm-instr)
         (label-entry-rest? gvm-instr)
         (label-entry-closed? gvm-instr)
         sn))
       ((return) (gen-label-return (label-lbl-num gvm-instr) sn))
       ((task-entry) (gen-label-task-entry (label-lbl-num gvm-instr) sn))
       ((task-return) (gen-label-task-return (label-lbl-num gvm-instr) sn))
       (else (compiler-internal-error "gen-gvm-instr, unknown label type"))))
    ((apply)
     (gen-apply
      (apply-prim gvm-instr)
      (apply-opnds gvm-instr)
      (apply-loc gvm-instr)
      sn))
    ((copy) (gen-copy (copy-opnd gvm-instr) (copy-loc gvm-instr) sn))
    ((close) (gen-close (close-parms gvm-instr) sn))
    ((ifjump)
     (gen-ifjump
      (ifjump-test gvm-instr)
      (ifjump-opnds gvm-instr)
      (ifjump-true gvm-instr)
      (ifjump-false gvm-instr)
      (ifjump-poll? gvm-instr)
      (if (and next-gvm-instr
               (Smemq (label-type next-gvm-instr) '(simple task-entry)))
          (label-lbl-num next-gvm-instr)
          #f)))
    ((jump)
     (gen-jump
      (jump-opnd gvm-instr)
      (jump-nb-args gvm-instr)
      (jump-poll? gvm-instr)
      (if (and next-gvm-instr
               (Smemq (label-type next-gvm-instr) '(simple task-entry)))
          (label-lbl-num next-gvm-instr)
          #f)))
    (else
     (compiler-internal-error
      "gen-gvm-instr, unknown 'gvm-instr':"
      gvm-instr))))
(define (reg-in-opnd68 opnd) (set-bbv-version-limit! #f) 
  (cond ((dreg? opnd) opnd)
        ((areg? opnd) opnd)
        ((ind? opnd) (ind-areg opnd))
        ((pinc? opnd) (pinc-areg opnd))
        ((pdec? opnd) (pdec-areg opnd))
        ((disp? opnd) (disp-areg opnd))
        ((inx? opnd) (inx-ireg opnd))
        (else #f)))
(define (temp-in-opnd68 opnd) (set-bbv-version-limit! #f) 
  (let ((reg (reg-in-opnd68 opnd)))
    (if reg
        (cond ((identical-opnd68? reg dtemp1) reg)
              ((identical-opnd68? reg atemp1) reg)
              ((identical-opnd68? reg atemp2) reg)
              (else #f))
        #f)))
(define (pick-atemp keep) (set-bbv-version-limit! #f) 
  (if (and keep (identical-opnd68? keep atemp1)) atemp2 atemp1))
(define return-reg '())
(define max-nb-args 1024)
(define heap-allocation-fudge (SFX* pointer-size (SFX+ (SFX* 2 max-nb-args) 1024)))
(define intr-flag 0)
(define ltq-tail 1)
(define ltq-head 2)
(define heap-lim 12)
(define closure-lim 17)
(define closure-ptr 18)
(define intr-flag-slot (make-disp* pstate-reg (SFX* pointer-size intr-flag)))
(define ltq-tail-slot (make-disp* pstate-reg (SFX* pointer-size ltq-tail)))
(define ltq-head-slot (make-disp* pstate-reg (SFX* pointer-size ltq-head)))
(define heap-lim-slot (make-disp* pstate-reg (SFX* pointer-size heap-lim)))
(define closure-lim-slot (make-disp* pstate-reg (SFX* pointer-size closure-lim)))
(define closure-ptr-slot (make-disp* pstate-reg (SFX* pointer-size closure-ptr)))
(define touch-trap 1)
(define non-proc-jump-trap 6)
(define rest-params-trap 7)
(define rest-params-closed-trap 8)
(define wrong-nb-arg1-trap 9)
(define wrong-nb-arg1-closed-trap 10)
(define wrong-nb-arg2-trap 11)
(define wrong-nb-arg2-closed-trap 12)
(define heap-alloc1-trap 13)
(define heap-alloc2-trap 14)
(define closure-alloc-trap 15)
(define intr-trap 24)
(define cache-line-length 16)
(define polling-intermittency '())
(set! polling-intermittency 10)
(define (stat-clear!) (set-bbv-version-limit! #f)  (set! *stats* (cons 0 '())))
(define (stat-dump!) (set-bbv-version-limit! #f)  (emit-stat (Scdr *stats*)))
(define (stat-add! bin count) (set-bbv-version-limit! #f) 
  (define (add! stats bin count) (set-bbv-version-limit! #f) 
    (Sset-car! stats (SFX+ (Scar stats) count))
    (if (not (null? bin))
        (let ((x (Sassoc (Scar bin) (Scdr stats))))
          (if x
              (add! (Scdr x) (Scdr bin) count)
              (begin
                (Sset-cdr! stats (cons (list (Scar bin) 0) (Scdr stats)))
                (add! (Scdadr stats) (Scdr bin) count))))))
  (add! *stats* bin count))
(define (fetch-stat-add! gvm-opnd) (set-bbv-version-limit! #f)  (opnd-stat-add! 'fetch gvm-opnd))
(define (store-stat-add! gvm-opnd) (set-bbv-version-limit! #f)  (opnd-stat-add! 'store gvm-opnd))
(define (jump-stat-add! gvm-opnd) (set-bbv-version-limit! #f)  (opnd-stat-add! 'jump gvm-opnd))
(define (opnd-stat-add! type opnd) (set-bbv-version-limit! #f) 
  (cond ((reg? opnd) (stat-add! (list 'gvm-opnd 'reg type (reg-num opnd)) 1))
        ((stk? opnd) (stat-add! (list 'gvm-opnd 'stk type) 1))
        ((glo? opnd) (stat-add! (list 'gvm-opnd 'glo type (glo-name opnd)) 1))
        ((clo? opnd)
         (stat-add! (list 'gvm-opnd 'clo type) 1)
         (fetch-stat-add! (clo-base opnd)))
        ((lbl? opnd) (stat-add! (list 'gvm-opnd 'lbl type) 1))
        ((obj? opnd)
         (let ((val (obj-val opnd)))
           (if (number? val)
               (stat-add! (list 'gvm-opnd 'obj type val) 1)
               (stat-add! (list 'gvm-opnd 'obj type (obj-type val)) 1))))
        (else
         (compiler-internal-error "opnd-stat-add!, unknown 'opnd':" opnd))))
(define (opnd-stat opnd) (set-bbv-version-limit! #f) 
  (cond ((reg? opnd) 'reg)
        ((stk? opnd) 'stk)
        ((glo? opnd) 'glo)
        ((clo? opnd) 'clo)
        ((lbl? opnd) 'lbl)
        ((obj? opnd) 'obj)
        (else (compiler-internal-error "opnd-stat, unknown 'opnd':" opnd))))
(define *stats* '())
(define (move-opnd68-to-loc68 opnd loc) (set-bbv-version-limit! #f) 
  (if (not (identical-opnd68? opnd loc))
      (if (imm? opnd)
          (move-n-to-loc68 (imm-val opnd) loc)
          (emit-move.l opnd loc))))
(define (move-obj-to-loc68 obj loc) (set-bbv-version-limit! #f) 
  (let ((n (obj-encoding obj)))
    (if n (move-n-to-loc68 n loc) (emit-move.l (emit-const obj) loc))))
(define (move-n-to-loc68 n loc) (set-bbv-version-limit! #f) 
  (cond ((SFX= n bits-null) (emit-move.l null-reg loc))
        ((SFX= n bits-false) (emit-move.l false-reg loc))
        ((and (dreg? loc) (SFX>= n -128) (SFX<= n 127)) (emit-moveq n loc))
        ((and (areg? loc) (SFX>= n -32768) (SFX<= n 32767))
         (emit-move.w (make-imm n) loc))
        ((and (identical-opnd68? loc pdec-sp) (SFX>= n -32768) (SFX<= n 32767))
         (emit-pea* n))
        ((SFX= n 0) (emit-clr.l loc))
        ((and (not (and (inx? loc) (SFX= (inx-ireg loc) dtemp1)))
              (SFX>= n -128)
              (SFX<= n 127))
         (emit-moveq n dtemp1)
         (emit-move.l dtemp1 loc))
        (else (emit-move.l (make-imm n) loc))))
(define (add-n-to-loc68 n loc) (set-bbv-version-limit! #f) 
  (if (not (SFX= n 0))
      (cond ((and (SFX>= n -8) (SFX<= n 8))
             (if (SFX> n 0) (emit-addq.l n loc) (emit-subq.l (SFX- n) loc)))
            ((and (areg? loc) (SFX>= n -32768) (SFX<= n 32767))
             (emit-lea (make-disp loc n) loc))
            ((and (not (identical-opnd68? loc dtemp1)) (SFX>= n -128) (SFX<= n 128))
             (emit-moveq (SFX- (abs n)) dtemp1)
             (if (SFX> n 0) (emit-sub.l dtemp1 loc) (emit-add.l dtemp1 loc)))
            (else (emit-add.l (make-imm n) loc)))))
(define (power-of-2 n) (set-bbv-version-limit! #f) 
  (let loop ((i 0) (k 1))
    (cond ((SFX= k n) i) ((SFX> k n) #f) (else (loop (SFX+ i 1) (SFX* k 2))))))
(define (mul-n-to-reg68 n reg) (set-bbv-version-limit! #f) 
  (if (SFX= n 0)
      (emit-moveq 0 reg)
      (let ((abs-n (abs n)))
        (if (SFX= abs-n 1)
            (if (SFX< n 0) (emit-neg.l reg))
            (let ((shift (power-of-2 abs-n)))
              (if shift
                  (let ((m (min shift 32)))
                    (if (or (SFX<= m 8) (identical-opnd68? reg dtemp1))
                        (let loop ((i m))
                          (if (SFX> i 0)
                              (begin
                                (emit-asl.l (make-imm (min i 8)) reg)
                                (loop (SFX- i 8)))))
                        (begin (emit-moveq m dtemp1) (emit-asl.l dtemp1 reg)))
                    (if (SFX< n 0) (emit-neg.l reg)))
                  (emit-muls.l (make-imm n) reg)))))))
(define (div-n-to-reg68 n reg) (set-bbv-version-limit! #f) 
  (let ((abs-n (abs n)))
    (if (SFX= abs-n 1)
        (if (SFX< n 0) (emit-neg.l reg))
        (let ((shift (power-of-2 abs-n)))
          (if shift
              (let ((m (min shift 32)) (lbl (new-lbl!)))
                (emit-move.l reg reg)
                (emit-bpl lbl)
                (add-n-to-loc68 (SFX* (SFX- abs-n 1) 8) reg)
                (emit-label lbl)
                (if (or (SFX<= m 8) (identical-opnd68? reg dtemp1))
                    (let loop ((i m))
                      (if (SFX> i 0)
                          (begin
                            (emit-asr.l (make-imm (min i 8)) reg)
                            (loop (SFX- i 8)))))
                    (begin (emit-moveq m dtemp1) (emit-asr.l dtemp1 reg)))
                (if (SFX< n 0) (emit-neg.l reg)))
              (emit-divsl.l (make-imm n) reg reg))))))
(define (cmp-n-to-opnd68 n opnd) (set-bbv-version-limit! #f) 
  (cond ((SFX= n bits-null) (emit-cmp.l opnd null-reg) #f)
        ((SFX= n bits-false) (emit-cmp.l opnd false-reg) #f)
        ((or (pcr? opnd) (imm? opnd))
         (if (SFX= n 0)
             (begin (emit-move.l opnd dtemp1) #t)
             (begin
               (move-opnd68-to-loc68 opnd atemp1)
               (if (and (SFX>= n -32768) (SFX<= n 32767))
                   (emit-cmp.w (make-imm n) atemp1)
                   (emit-cmp.l (make-imm n) atemp1))
               #t)))
        ((SFX= n 0) (emit-move.l opnd dtemp1) #t)
        ((and (SFX>= n -128) (SFX<= n 127) (not (identical-opnd68? opnd dtemp1)))
         (emit-moveq n dtemp1)
         (emit-cmp.l opnd dtemp1)
         #f)
        (else (emit-cmp.l (make-imm n) opnd) #t)))
(define current-fs '())
(define (adjust-current-fs n) (set-bbv-version-limit! #f)  (set! current-fs (SFX+ current-fs n)))
(define (new-lbl!) (set-bbv-version-limit! #f)  (label-counter))
(define (needed? loc sn) (set-bbv-version-limit! #f)  (and loc (if (stk? loc) (SFX<= (stk-num loc) sn) #t)))
(define (sn-opnd opnd sn) (set-bbv-version-limit! #f) 
  (cond ((stk? opnd) (max (stk-num opnd) sn))
        ((clo? opnd) (sn-opnd (clo-base opnd) sn))
        (else sn)))
(define (sn-opnds opnds sn) (set-bbv-version-limit! #f) 
  (if (null? opnds) sn (sn-opnd (Scar opnds) (sn-opnds (Scdr opnds) sn))))
(define (sn-opnd68 opnd sn) (set-bbv-version-limit! #f) 
  (cond ((and (disp*? opnd) (identical-opnd68? (disp*-areg opnd) sp-reg))
         (max (disp*-offset opnd) sn))
        ((identical-opnd68? opnd pdec-sp) (max (SFX+ current-fs 1) sn))
        ((identical-opnd68? opnd pinc-sp) (max current-fs sn))
        (else sn)))
(define (resize-frame n) (set-bbv-version-limit! #f) 
  (let ((x (SFX- n current-fs)))
    (adjust-current-fs x)
    (add-n-to-loc68 (SFX* (SFX- pointer-size) x) sp-reg)))
(define (shrink-frame n) (set-bbv-version-limit! #f) 
  (cond ((SFX< n current-fs) (resize-frame n))
        ((SFX> n current-fs)
         (compiler-internal-error "shrink-frame, can't increase frame size"))))
(define (make-top-of-frame n sn) (set-bbv-version-limit! #f) 
  (if (and (SFX< n current-fs) (SFX>= n sn)) (resize-frame n)))
(define (make-top-of-frame-if-stk-opnd68 opnd sn) (set-bbv-version-limit! #f) 
  (if (frame-base-rel? opnd)
      (make-top-of-frame (frame-base-rel-slot opnd) sn)))
(define (make-top-of-frame-if-stk-opnds68 opnd1 opnd2 sn) (set-bbv-version-limit! #f) 
  (if (frame-base-rel? opnd1)
      (let ((slot1 (frame-base-rel-slot opnd1)))
        (if (frame-base-rel? opnd2)
            (make-top-of-frame (max (frame-base-rel-slot opnd2) slot1) sn)
            (make-top-of-frame slot1 sn)))
      (if (frame-base-rel? opnd2)
          (make-top-of-frame (frame-base-rel-slot opnd2) sn))))
(define (opnd68->true-opnd68 opnd sn) (set-bbv-version-limit! #f) 
  (if (frame-base-rel? opnd)
      (let ((slot (frame-base-rel-slot opnd)))
        (cond ((SFX> slot current-fs) (adjust-current-fs 1) pdec-sp)
              ((and (SFX= slot current-fs) (SFX< sn current-fs))
               (adjust-current-fs -1)
               pinc-sp)
              (else (make-disp* sp-reg (SFX* pointer-size (SFX- current-fs slot))))))
      opnd))
(define (move-opnd68-to-any-areg opnd keep sn) (set-bbv-version-limit! #f) 
  (if (areg? opnd)
      opnd
      (let ((areg (pick-atemp keep)))
        (make-top-of-frame-if-stk-opnd68 opnd sn)
        (move-opnd68-to-loc68 (opnd68->true-opnd68 opnd sn) areg)
        areg)))
(define (clo->opnd68 opnd keep sn) (set-bbv-version-limit! #f) 
  (let ((base (clo-base opnd)) (offs (closed-var-offset (clo-index opnd))))
    (if (lbl? base) (make-pcr (lbl-num base) offs) (clo->loc68 opnd keep sn))))
(define (clo->loc68 opnd keep sn) (set-bbv-version-limit! #f) 
  (let ((base (clo-base opnd)) (offs (closed-var-offset (clo-index opnd))))
    (cond ((eq? base return-reg) (make-disp* (reg->reg68 base) offs))
          ((obj? base)
           (let ((areg (pick-atemp keep)))
             (move-obj-to-loc68 (obj-val base) areg)
             (make-disp* areg offs)))
          (else
           (let ((areg (pick-atemp keep)))
             (move-opnd-to-loc68 base areg sn)
             (make-disp* areg offs))))))
(define (reg->reg68 reg) (set-bbv-version-limit! #f)  (reg-num->reg68 (reg-num reg)))
(define (reg-num->reg68 num) (set-bbv-version-limit! #f) 
  (if (SFX= num 0) (make-areg gvm-reg0) (make-dreg (SFX+ (SFX- num 1) gvm-reg1))))
(define (opnd->opnd68 opnd keep sn) (set-bbv-version-limit! #f) 
  (cond ((lbl? opnd)
         (let ((areg (pick-atemp keep)))
           (emit-lea (make-pcr (lbl-num opnd) 0) areg)
           areg))
        ((obj? opnd)
         (let ((val (obj-val opnd)))
           (if (proc-obj? val)
               (let ((num (add-object val)) (areg (pick-atemp keep)))
                 (if num (emit-move-proc num areg) (emit-move-prim val areg))
                 areg)
               (let ((n (obj-encoding val)))
                 (if n (make-imm n) (emit-const val))))))
        ((clo? opnd) (clo->opnd68 opnd keep sn))
        (else (loc->loc68 opnd keep sn))))
(define (loc->loc68 loc keep sn) (set-bbv-version-limit! #f) 
  (cond ((reg? loc) (reg->reg68 loc))
        ((stk? loc) (make-frame-base-rel (stk-num loc)))
        ((glo? loc) (make-glob (glo-name loc)))
        ((clo? loc) (clo->loc68 loc keep sn))
        (else (compiler-internal-error "loc->loc68, unknown 'loc':" loc))))
(define (move-opnd68-to-loc opnd loc sn) (set-bbv-version-limit! #f) 
  (cond ((reg? loc)
         (make-top-of-frame-if-stk-opnd68 opnd sn)
         (move-opnd68-to-loc68 (opnd68->true-opnd68 opnd sn) (reg->reg68 loc)))
        ((stk? loc)
         (let* ((loc-slot (stk-num loc))
                (sn-after-opnd1 (if (SFX< loc-slot sn) sn (SFX- loc-slot 1))))
           (if (SFX> current-fs loc-slot)
               (make-top-of-frame
                (if (frame-base-rel? opnd)
                    (let ((opnd-slot (frame-base-rel-slot opnd)))
                      (if (SFX>= opnd-slot (SFX- loc-slot 1)) opnd-slot loc-slot))
                    loc-slot)
                sn-after-opnd1))
           (let* ((opnd1 (opnd68->true-opnd68 opnd sn-after-opnd1))
                  (opnd2 (opnd68->true-opnd68
                          (make-frame-base-rel loc-slot)
                          sn)))
             (move-opnd68-to-loc68 opnd1 opnd2))))
        ((glo? loc)
         (make-top-of-frame-if-stk-opnd68 opnd sn)
         (move-opnd68-to-loc68
          (opnd68->true-opnd68 opnd sn)
          (make-glob (glo-name loc))))
        ((clo? loc)
         (let ((clo (clo->loc68
                     loc
                     (temp-in-opnd68 opnd)
                     (sn-opnd68 opnd sn))))
           (make-top-of-frame-if-stk-opnd68 opnd sn)
           (move-opnd68-to-loc68 (opnd68->true-opnd68 opnd sn) clo)))
        (else
         (compiler-internal-error "move-opnd68-to-loc, unknown 'loc':" loc))))
(define (move-opnd-to-loc68 opnd loc68 sn) (set-bbv-version-limit! #f) 
  (if (and (lbl? opnd) (areg? loc68))
      (emit-lea (make-pcr (lbl-num opnd) 0) loc68)
      (let* ((sn-after-opnd68 (sn-opnd68 loc68 sn))
             (opnd68 (opnd->opnd68
                      opnd
                      (temp-in-opnd68 loc68)
                      sn-after-opnd68)))
        (make-top-of-frame-if-stk-opnds68 opnd68 loc68 sn)
        (let* ((opnd68* (opnd68->true-opnd68 opnd68 sn-after-opnd68))
               (loc68* (opnd68->true-opnd68 loc68 sn)))
          (move-opnd68-to-loc68 opnd68* loc68*)))))
(define (copy-opnd-to-loc opnd loc sn) (set-bbv-version-limit! #f) 
  (if (and (lbl? opnd) (eq? loc return-reg))
      (emit-lea (make-pcr (lbl-num opnd) 0) (reg->reg68 loc))
      (move-opnd68-to-loc (opnd->opnd68 opnd #f (sn-opnd loc sn)) loc sn)))
(define (touch-reg68-to-reg68 src dst) (set-bbv-version-limit! #f) 
  (define (trap-to-touch-handler dreg lbl) (set-bbv-version-limit! #f) 
    (if ofile-stats?
        (emit-stat
         '((touch 0
                  (determined-placeholder -1)
                  (undetermined-placeholder 1)))))
    (gen-trap
     instr-source
     entry-frame
     #t
     dreg
     (SFX+ touch-trap (dreg-num dreg))
     lbl))
  (define (touch-dreg-to-reg src dst) (set-bbv-version-limit! #f) 
    (let ((lbl1 (new-lbl!)))
      (emit-btst src placeholder-reg)
      (emit-bne lbl1)
      (if ofile-stats?
          (emit-stat
           '((touch 0 (non-placeholder -1) (determined-placeholder 1)))))
      (trap-to-touch-handler src lbl1)
      (move-opnd68-to-loc68 src dst)))
  (define (touch-areg-to-dreg src dst) (set-bbv-version-limit! #f) 
    (let ((lbl1 (new-lbl!)))
      (emit-move.l src dst)
      (emit-btst dst placeholder-reg)
      (emit-bne lbl1)
      (if ofile-stats?
          (emit-stat
           '((touch 0 (non-placeholder -1) (determined-placeholder 1)))))
      (trap-to-touch-handler dst lbl1)))
  (if ofile-stats? (emit-stat '((touch 1 (non-placeholder 1)))))
  (cond ((dreg? src) (touch-dreg-to-reg src dst))
        ((dreg? dst) (touch-areg-to-dreg src dst))
        (else (emit-move.l src dtemp1) (touch-dreg-to-reg dtemp1 dst))))
(define (touch-opnd-to-any-reg68 opnd sn) (set-bbv-version-limit! #f) 
  (if (reg? opnd)
      (let ((reg (reg->reg68 opnd))) (touch-reg68-to-reg68 reg reg) reg)
      (let ((opnd68 (opnd->opnd68 opnd #f sn)))
        (make-top-of-frame-if-stk-opnd68 opnd68 sn)
        (move-opnd68-to-loc68 (opnd68->true-opnd68 opnd68 sn) dtemp1)
        (touch-reg68-to-reg68 dtemp1 dtemp1)
        dtemp1)))
(define (touch-opnd-to-loc opnd loc sn) (set-bbv-version-limit! #f) 
  (if (reg? opnd)
      (let ((reg68 (reg->reg68 opnd)))
        (if (reg? loc)
            (touch-reg68-to-reg68 reg68 (reg->reg68 loc))
            (begin
              (touch-reg68-to-reg68 reg68 reg68)
              (move-opnd68-to-loc reg68 loc sn))))
      (if (reg? loc)
          (let ((reg68 (reg->reg68 loc)))
            (move-opnd-to-loc68 opnd reg68 sn)
            (touch-reg68-to-reg68 reg68 reg68))
          (let ((reg68 (touch-opnd-to-any-reg68 opnd sn)))
            (move-opnd68-to-loc reg68 loc sn)))))
(define (gen-trap source frame save-live? not-save-reg num lbl) (set-bbv-version-limit! #f) 
  (define (adjust-slots l n) (set-bbv-version-limit! #f) 
    (cond ((SFX= n 0) (Sappend l '()))
          ((SFX< n 0) (adjust-slots (Scdr l) (SFX+ n 1)))
          (else (adjust-slots (cons empty-var l) (SFX- n 1)))))
  (define (set-slot! slots i x) (set-bbv-version-limit! #f) 
    (let loop ((l slots) (n (SFX- (Slength slots) i)))
      (if (SFX> n 0) (loop (Scdr l) (SFX- n 1)) (Sset-car! l x))))
  (let ((ret-slot (frame-first-empty-slot frame)))
    (let loop1 ((save1 '()) (save2 #f) (regs (frame-regs frame)) (i 0))
      (if (pair? regs)
          (let ((var (Scar regs)))
            (if (eq? var ret-var)
                (let ((x (cons (reg->reg68 (make-reg i)) var)))
                  (if (SFX> ret-slot current-fs)
                      (loop1 (cons x save1) save2 (Scdr regs) (SFX+ i 1))
                      (loop1 save1 x (Scdr regs) (SFX+ i 1))))
                (if (and save-live?
                         (frame-live? var frame)
                         (not (eqv? not-save-reg (reg->reg68 (make-reg i)))))
                    (loop1 (cons (cons (reg->reg68 (make-reg i)) var) save1)
                           save2
                           (Scdr regs)
                           (SFX+ i 1))
                    (loop1 save1 save2 (Scdr regs) (SFX+ i 1)))))
          (let ((order (sort-list save1 (lambda (x y) (set-bbv-version-limit! #f)  (SFX< (Scar x) (Scar y))))))
            (let ((slots (Sappend (Smap2 (lambda (x) (set-bbv-version-limit! #f)  (Scdr x)) order)
                                 (adjust-slots
                                  (frame-slots frame)
                                  (SFX- current-fs (frame-size frame)))))
                  (reg-list (Smap2 (lambda (x) (set-bbv-version-limit! #f)  (Scar x)) order))
                  (nb-regs (Slength order)))
              (define (trap) (set-bbv-version-limit! #f) 
                (emit-trap2 num '())
                (gen-label-return*
                 (new-lbl!)
                 (add-first-class-label! source slots frame)
                 slots
                 0))
              (if save2
                  (begin
                    (emit-move.l
                     (Scar save2)
                     (make-disp*
                      sp-reg
                      (SFX* pointer-size (SFX- current-fs ret-slot))))
                    (set-slot! slots ret-slot (Scdr save2))))
              (if (SFX> (Slength order) 2)
                  (begin
                    (emit-movem.l reg-list pdec-sp)
                    (trap)
                    (emit-movem.l pinc-sp reg-list))
                  (let loop2 ((l (Sreverse reg-list)))
                    (if (pair? l)
                        (let ((reg (Scar l)))
                          (emit-move.l reg pdec-sp)
                          (loop2 (Scdr l))
                          (emit-move.l pinc-sp reg))
                        (trap))))
              (if save2
                  (emit-move.l
                   (make-disp* sp-reg (SFX* pointer-size (SFX- current-fs ret-slot)))
                   (Scar save2)))
              (emit-label lbl)))))))
(define (gen-label-simple lbl sn) (set-bbv-version-limit! #f) 
  (if ofile-stats?
      (begin (stat-clear!) (stat-add! '(gvm-instr label simple) 1)))
  (set! pointers-allocated 0)
  (emit-label lbl))
(define (gen-label-entry lbl nb-parms min rest? closed? sn) (set-bbv-version-limit! #f) 
  (if ofile-stats?
      (begin
        (stat-clear!)
        (stat-add!
         (list 'gvm-instr
               'label
               'entry
               nb-parms
               min
               (if rest? 'rest 'not-rest)
               (if closed? 'closed 'not-closed))
         1)))
  (set! pointers-allocated 0)
  (let ((label-descr (add-first-class-label! instr-source '() exit-frame)))
    (if (SFX= lbl entry-lbl-num)
        (emit-label lbl)
        (emit-label-subproc lbl entry-lbl-num label-descr)))
  (let* ((nb-parms* (if rest? (SFX- nb-parms 1) nb-parms))
         (dispatch-lbls (Smake-vector1 (SFX+ (SFX- nb-parms min) 1)))
         (optional-lbls (Smake-vector1 (SFX+ (SFX- nb-parms min) 1))))
    (let loop ((i min))
      (if (SFX<= i nb-parms)
          (let ((lbl (new-lbl!)))
            (Svector-set! optional-lbls (SFX- nb-parms i) lbl)
            (Svector-set!
             dispatch-lbls
             (SFX- nb-parms i)
             (if (or (SFX>= i nb-parms) (SFX<= nb-parms nb-arg-regs))
                 lbl
                 (new-lbl!)))
            (loop (SFX+ i 1)))))
    (if closed?
        (let ((closure-reg (reg-num->reg68 (SFX+ nb-arg-regs 1))))
          (emit-move.l pinc-sp closure-reg)
          (emit-subq.l 6 closure-reg)
          (if (or (and (SFX<= min 1) (SFX<= 1 nb-parms*))
                  (and (SFX<= min 2) (SFX<= 2 nb-parms*)))
              (emit-move.w dtemp1 dtemp1))))
    (if (and (SFX<= min 2) (SFX<= 2 nb-parms*))
        (emit-beq (Svector-ref dispatch-lbls (SFX- nb-parms 2))))
    (if (and (SFX<= min 1) (SFX<= 1 nb-parms*))
        (emit-bmi (Svector-ref dispatch-lbls (SFX- nb-parms 1))))
    (let loop ((i min))
      (if (SFX<= i nb-parms*)
          (begin
            (if (not (or (SFX= i 1) (SFX= i 2)))
                (begin
                  (emit-cmp.w (make-imm (encode-arg-count i)) arg-count-reg)
                  (emit-beq (Svector-ref dispatch-lbls (SFX- nb-parms i)))))
            (loop (SFX+ i 1)))))
    (cond (rest?
           (emit-trap1
            (if closed? rest-params-closed-trap rest-params-trap)
            (list min nb-parms*))
           (if (not closed?) (emit-lbl-ptr lbl))
           (set! pointers-allocated 1)
           (gen-guarantee-fudge)
           (emit-bra (Svector-ref optional-lbls 0)))
          ((SFX= min nb-parms*)
           (emit-trap1
            (if closed? wrong-nb-arg1-closed-trap wrong-nb-arg1-trap)
            (list nb-parms*))
           (if (not closed?) (emit-lbl-ptr lbl)))
          (else
           (emit-trap1
            (if closed? wrong-nb-arg2-closed-trap wrong-nb-arg2-trap)
            (list min nb-parms*))
           (if (not closed?) (emit-lbl-ptr lbl))))
    (if (SFX> nb-parms nb-arg-regs)
        (let loop1 ((i (SFX- nb-parms 1)))
          (if (SFX>= i min)
              (let ((nb-stacked (if (SFX<= i nb-arg-regs) 0 (SFX- i nb-arg-regs))))
                (emit-label (Svector-ref dispatch-lbls (SFX- nb-parms i)))
                (let loop2 ((j 1))
                  (if (and (SFX<= j nb-arg-regs)
                           (SFX<= j i)
                           (SFX<= j (SFX- (SFX- nb-parms nb-arg-regs) nb-stacked)))
                      (begin
                        (emit-move.l (reg-num->reg68 j) pdec-sp)
                        (loop2 (SFX+ j 1)))
                      (let loop3 ((k j))
                        (if (and (SFX<= k nb-arg-regs) (SFX<= k i))
                            (begin
                              (emit-move.l
                               (reg-num->reg68 k)
                               (reg-num->reg68 (SFX+ (SFX- k j) 1)))
                              (loop3 (SFX+ k 1)))))))
                (if (SFX> i min)
                    (emit-bra (Svector-ref optional-lbls (SFX- nb-parms i))))
                (loop1 (SFX- i 1))))))
    (let loop ((i min))
      (if (SFX<= i nb-parms)
          (let ((val (if (SFX= i nb-parms*) bits-null bits-unass)))
            (emit-label (Svector-ref optional-lbls (SFX- nb-parms i)))
            (cond ((SFX> (SFX- nb-parms i) nb-arg-regs)
                   (move-n-to-loc68 val pdec-sp))
                  ((SFX< i nb-parms)
                   (move-n-to-loc68
                    val
                    (reg-num->reg68 (parm->reg-num (SFX+ i 1) nb-parms)))))
            (loop (SFX+ i 1)))))))
(define (encode-arg-count n) (set-bbv-version-limit! #f)  (cond ((SFX= n 1) -1) ((SFX= n 2) 0) (else (SFX+ n 1))))
(define (parm->reg-num i nb-parms) (set-bbv-version-limit! #f) 
  (if (SFX<= nb-parms nb-arg-regs) i (SFX+ i (SFX- nb-arg-regs nb-parms))))
(define (no-arg-check-entry-offset proc nb-args) (set-bbv-version-limit! #f) 
  (let ((x (proc-obj-call-pat proc)))
    (if (and (pair? x) (null? (Scdr x)))
        (let ((arg-count (Scar x)))
          (if (SFX= arg-count nb-args)
              (if (or (SFX= arg-count 1) (SFX= arg-count 2)) 10 14)
              0))
        0)))
(define (gen-label-return lbl sn) (set-bbv-version-limit! #f) 
  (if ofile-stats?
      (begin (stat-clear!) (stat-add! '(gvm-instr label return) 1)))
  (set! pointers-allocated 0)
  (let ((slots (frame-slots exit-frame)))
    (gen-label-return*
     lbl
     (add-first-class-label! instr-source slots exit-frame)
     slots
     0)))
(define (gen-label-return* lbl label-descr slots extra) (set-bbv-version-limit! #f) 
  (let ((i (pos-in-list ret-var slots)))
    (if i
        (let* ((fs (Slength slots)) (link (SFX- fs i)))
          (emit-label-return lbl entry-lbl-num (SFX+ fs extra) link label-descr))
        (compiler-internal-error
         "gen-label-return*, no return address in frame"))))
(define (gen-label-task-entry lbl sn) (set-bbv-version-limit! #f) 
  (if ofile-stats?
      (begin (stat-clear!) (stat-add! '(gvm-instr label task-entry) 1)))
  (set! pointers-allocated 0)
  (emit-label lbl)
  (if (SFX= current-fs 0)
      (begin
        (emit-move.l (reg->reg68 return-reg) pdec-sp)
        (emit-move.l sp-reg (make-pinc ltq-tail-reg)))
      (begin
        (emit-move.l sp-reg atemp1)
        (emit-move.l (make-pinc atemp1) pdec-sp)
        (let loop ((i (SFX- current-fs 1)))
          (if (SFX> i 0)
              (begin
                (emit-move.l (make-pinc atemp1) (make-disp atemp1 -8))
                (loop (SFX- i 1)))))
        (emit-move.l (reg->reg68 return-reg) (make-pdec atemp1))
        (emit-move.l atemp1 (make-pinc ltq-tail-reg))))
  (emit-move.l ltq-tail-reg ltq-tail-slot))
(define (gen-label-task-return lbl sn) (set-bbv-version-limit! #f) 
  (if ofile-stats?
      (begin (stat-clear!) (stat-add! '(gvm-instr label task-return) 1)))
  (set! pointers-allocated 0)
  (let ((slots (frame-slots exit-frame)))
    (set! current-fs (SFX+ current-fs 1))
    (let ((dummy-lbl (new-lbl!)) (skip-lbl (new-lbl!)))
      (gen-label-return*
       dummy-lbl
       (add-first-class-label! instr-source slots exit-frame)
       slots
       1)
      (emit-bra skip-lbl)
      (gen-label-task-return*
       lbl
       (add-first-class-label! instr-source slots exit-frame)
       slots
       1)
      (emit-subq.l pointer-size ltq-tail-reg)
      (emit-label skip-lbl))))
(define (gen-label-task-return* lbl label-descr slots extra) (set-bbv-version-limit! #f) 
  (let ((i (pos-in-list ret-var slots)))
    (if i
        (let* ((fs (Slength slots)) (link (SFX- fs i)))
          (emit-label-task-return
           lbl
           entry-lbl-num
           (SFX+ fs extra)
           link
           label-descr))
        (compiler-internal-error
         "gen-label-task-return*, no return address in frame"))))
(define (gen-apply prim opnds loc sn) (set-bbv-version-limit! #f) 
  (if ofile-stats?
      (begin
        (stat-add!
         (list 'gvm-instr
               'apply
               (string->canonical-symbol (proc-obj-name prim))
               (Smap2 opnd-stat opnds)
               (if loc (opnd-stat loc) #f))
         1)
        (for-each fetch-stat-add! opnds)
        (if loc (store-stat-add! loc))))
  (let ((x (proc-obj-inlinable prim)))
    (if (not x)
        (compiler-internal-error "gen-APPLY, unknown 'prim':" prim)
        (if (or (needed? loc sn) (Scar x)) ((Scdr x) opnds loc sn)))))
(define (define-apply name side-effects? proc) (set-bbv-version-limit! #f) 
  (let ((prim (get-prim-info name)))
    (proc-obj-inlinable-set! prim (cons side-effects? proc))))
(define (gen-copy opnd loc sn) (set-bbv-version-limit! #f) 
  (if ofile-stats?
      (begin
        (stat-add! (list 'gvm-instr 'copy (opnd-stat opnd) (opnd-stat loc)) 1)
        (fetch-stat-add! opnd)
        (store-stat-add! loc)))
  (if (needed? loc sn) (copy-opnd-to-loc opnd loc sn)))
(define (gen-close parms sn) (set-bbv-version-limit! #f) 
  (define (size->bytes size) (set-bbv-version-limit! #f) 
    (SFX* (SFXquotient
        (SFX+ (SFX* (SFX+ size 2) pointer-size) (SFX- cache-line-length 1))
        cache-line-length)
       cache-line-length))
  (define (parms->bytes parms) (set-bbv-version-limit! #f) 
    (if (null? parms)
        0
        (SFX+ (size->bytes (Slength (closure-parms-opnds (Scar parms))))
           (parms->bytes (Scdr parms)))))
  (if ofile-stats?
      (begin
        (for-each
         (lambda (x) (set-bbv-version-limit! #f) 
           (stat-add!
            (list 'gvm-instr
                  'close
                  (opnd-stat (closure-parms-loc x))
                  (Smap2 opnd-stat (closure-parms-opnds x)))
            1)
           (store-stat-add! (closure-parms-loc x))
           (fetch-stat-add! (make-lbl (closure-parms-lbl x)))
           (for-each fetch-stat-add! (closure-parms-opnds x)))
         parms)))
  (let ((total-space-needed (parms->bytes parms)) (lbl1 (new-lbl!)))
    (emit-move.l closure-ptr-slot atemp2)
    (move-n-to-loc68 total-space-needed dtemp1)
    (emit-sub.l dtemp1 atemp2)
    (emit-cmp.l closure-lim-slot atemp2)
    (emit-bcc lbl1)
    (gen-trap instr-source entry-frame #f #f closure-alloc-trap lbl1)
    (emit-move.l atemp2 closure-ptr-slot)
    (let* ((opnds* (LIBconcatenate (Smap2 closure-parms-opnds parms)))
           (sn* (sn-opnds opnds* sn)))
      (let loop1 ((parms parms))
        (let ((loc (closure-parms-loc (Scar parms)))
              (size (Slength (closure-parms-opnds (Scar parms))))
              (rest (Scdr parms)))
          (if (SFX= size 1)
              (emit-addq.l type-procedure atemp2)
              (emit-move.w
               (make-imm (SFX+ 32768 (SFX* (SFX+ size 1) 4)))
               (make-pinc atemp2)))
          (move-opnd68-to-loc
           atemp2
           loc
           (sn-opnds (Smap2 closure-parms-loc rest) sn*))
          (if (null? rest)
              (add-n-to-loc68
               (SFX+ (SFX- (size->bytes size) total-space-needed) 2)
               atemp2)
              (begin
                (add-n-to-loc68 (SFX- (size->bytes size) type-procedure) atemp2)
                (loop1 rest)))))
      (let loop2 ((parms parms))
        (let* ((opnds (closure-parms-opnds (Scar parms)))
               (lbl (closure-parms-lbl (Scar parms)))
               (size (Slength opnds))
               (rest (Scdr parms)))
          (emit-lea (make-pcr lbl 0) atemp1)
          (emit-move.l atemp1 (make-pinc atemp2))
          (let loop3 ((opnds opnds))
            (if (not (null? opnds))
                (let ((sn** (sn-opnds
                             (LIBconcatenate (Smap2 closure-parms-opnds rest))
                             sn)))
                  (move-opnd-to-loc68
                   (Scar opnds)
                   (make-pinc atemp2)
                   (sn-opnds (Scdr opnds) sn**))
                  (loop3 (Scdr opnds)))))
          (if (not (null? rest))
              (begin
                (add-n-to-loc68
                 (SFX- (size->bytes size) (SFX* (SFX+ size 1) pointer-size))
                 atemp2)
                (loop2 rest))))))))
(define (gen-ifjump test opnds true-lbl false-lbl poll? next-lbl) (set-bbv-version-limit! #f) 
  (if ofile-stats?
      (begin
        (stat-add!
         (list 'gvm-instr
               'ifjump
               (string->canonical-symbol (proc-obj-name test))
               (Smap2 opnd-stat opnds)
               (if poll? 'poll 'not-poll))
         1)
        (for-each fetch-stat-add! opnds)
        (stat-dump!)))
  (let ((proc (proc-obj-test test)))
    (if proc
        (gen-ifjump* proc opnds true-lbl false-lbl poll? next-lbl)
        (compiler-internal-error "gen-IFJUMP, unknown 'test':" test))))
(define (gen-ifjump* proc opnds true-lbl false-lbl poll? next-lbl) (set-bbv-version-limit! #f) 
  (let ((fs (frame-size exit-frame)))
    (define (double-branch) (set-bbv-version-limit! #f) 
      (proc #t opnds false-lbl fs)
      (if ofile-stats?
          (emit-stat
           '((gvm-instr.ifjump.fall-through 1)
             (gvm-instr.ifjump.double-branch 1))))
      (emit-bra true-lbl)
      (gen-deferred-code!))
    (gen-guarantee-fudge)
    (if poll? (gen-poll))
    (if next-lbl
        (cond ((SFX= true-lbl next-lbl)
               (proc #t opnds false-lbl fs)
               (if ofile-stats?
                   (emit-stat '((gvm-instr.ifjump.fall-through 1)))))
              ((SFX= false-lbl next-lbl)
               (proc #f opnds true-lbl fs)
               (if ofile-stats?
                   (emit-stat '((gvm-instr.ifjump.fall-through 1)))))
              (else (double-branch)))
        (double-branch))))
(define (define-ifjump name proc) (set-bbv-version-limit! #f) 
  (define-apply
   name
   #f
   (lambda (opnds loc sn) (set-bbv-version-limit! #f) 
     (let ((true-lbl (new-lbl!))
           (cont-lbl (new-lbl!))
           (reg68 (if (and (reg? loc) (not (eq? loc return-reg)))
                      (reg->reg68 loc)
                      dtemp1)))
       (proc #f opnds true-lbl current-fs)
       (move-n-to-loc68 bits-false reg68)
       (emit-bra cont-lbl)
       (emit-label true-lbl)
       (move-n-to-loc68 bits-true reg68)
       (emit-label cont-lbl)
       (move-opnd68-to-loc reg68 loc sn))))
  (proc-obj-test-set! (get-prim-info name) proc))
(define (gen-jump opnd nb-args poll? next-lbl) (set-bbv-version-limit! #f) 
  (let ((fs (frame-size exit-frame)))
    (if ofile-stats?
        (begin
          (stat-add!
           (list 'gvm-instr
                 'jump
                 (opnd-stat opnd)
                 nb-args
                 (if poll? 'poll 'not-poll))
           1)
          (jump-stat-add! opnd)
          (if (and (lbl? opnd) next-lbl (SFX= next-lbl (lbl-num opnd)))
              (stat-add! '(gvm-instr.jump.fall-through) 1))
          (stat-dump!)))
    (gen-guarantee-fudge)
    (cond ((glo? opnd)
           (if poll? (gen-poll))
           (setup-jump fs nb-args)
           (emit-jmp-glob (make-glob (glo-name opnd)))
           (gen-deferred-code!))
          ((and (stk? opnd) (SFX= (stk-num opnd) (SFX+ fs 1)) (not nb-args))
           (if poll? (gen-poll))
           (setup-jump (SFX+ fs 1) nb-args)
           (emit-rts)
           (gen-deferred-code!))
          ((lbl? opnd)
           (if (and poll?
                    (SFX= fs current-fs)
                    (not nb-args)
                    (not (and next-lbl (SFX= next-lbl (lbl-num opnd)))))
               (gen-poll-branch (lbl-num opnd))
               (begin
                 (if poll? (gen-poll))
                 (setup-jump fs nb-args)
                 (if (not (and next-lbl (SFX= next-lbl (lbl-num opnd))))
                     (emit-bra (lbl-num opnd))))))
          ((obj? opnd)
           (if poll? (gen-poll))
           (let ((val (obj-val opnd)))
             (if (proc-obj? val)
                 (let ((num (add-object val))
                       (offset (no-arg-check-entry-offset val nb-args)))
                   (setup-jump fs (if (SFX<= offset 0) nb-args #f))
                   (if num
                       (emit-jmp-proc num offset)
                       (emit-jmp-prim val offset))
                   (gen-deferred-code!))
                 (gen-jump* (opnd->opnd68 opnd #f fs) fs nb-args))))
          (else
           (if poll? (gen-poll))
           (gen-jump* (opnd->opnd68 opnd #f fs) fs nb-args)))))
(define (gen-jump* opnd fs nb-args) (set-bbv-version-limit! #f) 
  (if nb-args
      (let ((lbl (new-lbl!)))
        (make-top-of-frame-if-stk-opnd68 opnd fs)
        (move-opnd68-to-loc68 (opnd68->true-opnd68 opnd fs) atemp1)
        (shrink-frame fs)
        (emit-move.l atemp1 dtemp1)
        (emit-addq.w (SFXmodulo (SFX- type-pair type-procedure) 8) dtemp1)
        (emit-btst dtemp1 pair-reg)
        (emit-beq lbl)
        (move-n-to-loc68 (encode-arg-count nb-args) arg-count-reg)
        (emit-trap3 non-proc-jump-trap)
        (emit-label lbl)
        (move-n-to-loc68 (encode-arg-count nb-args) arg-count-reg)
        (emit-jmp (make-ind atemp1)))
      (let ((areg (move-opnd68-to-any-areg opnd #f fs)))
        (setup-jump fs nb-args)
        (emit-jmp (make-ind areg))))
  (gen-deferred-code!))
(define (setup-jump fs nb-args) (set-bbv-version-limit! #f) 
  (shrink-frame fs)
  (if nb-args (move-n-to-loc68 (encode-arg-count nb-args) arg-count-reg)))
(define (gen-poll) (set-bbv-version-limit! #f) 
  (let ((lbl (new-lbl!)))
    (emit-dbra poll-timer-reg lbl)
    (emit-moveq (SFX- polling-intermittency 1) poll-timer-reg)
    (emit-cmp.l intr-flag-slot sp-reg)
    (emit-bcc lbl)
    (gen-trap instr-source entry-frame #f #f intr-trap lbl)))
(define (gen-poll-branch lbl) (set-bbv-version-limit! #f) 
  (emit-dbra poll-timer-reg lbl)
  (emit-moveq (SFX- polling-intermittency 1) poll-timer-reg)
  (emit-cmp.l intr-flag-slot sp-reg)
  (emit-bcc lbl)
  (gen-trap instr-source entry-frame #f #f intr-trap (new-lbl!))
  (emit-bra lbl))
(define (make-gen-slot-ref slot type) (set-bbv-version-limit! #f) 
  (lambda (opnds loc sn) (set-bbv-version-limit! #f) 
    (let ((sn-loc (sn-opnd loc sn)) (opnd (Scar opnds)))
      (move-opnd-to-loc68 opnd atemp1 sn-loc)
      (move-opnd68-to-loc
       (make-disp* atemp1 (SFX- (SFX* slot pointer-size) type))
       loc
       sn))))
(define (make-gen-slot-set! slot type) (set-bbv-version-limit! #f) 
  (lambda (opnds loc sn) (set-bbv-version-limit! #f) 
    (let ((sn-loc (if loc (sn-opnd loc sn) sn)))
      (let* ((first-opnd (Scar opnds))
             (second-opnd (Scadr opnds))
             (sn-second-opnd (sn-opnd second-opnd sn-loc)))
        (move-opnd-to-loc68 first-opnd atemp1 sn-second-opnd)
        (move-opnd-to-loc68
         second-opnd
         (make-disp* atemp1 (SFX- (SFX* slot pointer-size) type))
         sn-loc)
        (if loc
            (if (not (eq? first-opnd loc))
                (move-opnd68-to-loc atemp1 loc sn)))))))
(define (gen-cons opnds loc sn) (set-bbv-version-limit! #f) 
  (let ((sn-loc (sn-opnd loc sn)))
    (let ((first-opnd (Scar opnds)) (second-opnd (Scadr opnds)))
      (gen-guarantee-space 2)
      (if (contains-opnd? loc second-opnd)
          (let ((sn-second-opnd (sn-opnd second-opnd sn-loc)))
            (move-opnd-to-loc68 first-opnd (make-pdec heap-reg) sn-second-opnd)
            (move-opnd68-to-loc68 heap-reg atemp2)
            (move-opnd-to-loc68 second-opnd (make-pdec heap-reg) sn-loc)
            (move-opnd68-to-loc atemp2 loc sn))
          (let* ((sn-second-opnd (sn-opnd second-opnd sn))
                 (sn-loc (sn-opnd loc sn-second-opnd)))
            (move-opnd-to-loc68 first-opnd (make-pdec heap-reg) sn-loc)
            (move-opnd68-to-loc heap-reg loc sn-second-opnd)
            (move-opnd-to-loc68 second-opnd (make-pdec heap-reg) sn))))))
(define (make-gen-apply-c...r pattern) (set-bbv-version-limit! #f) 
  (lambda (opnds loc sn) (set-bbv-version-limit! #f) 
    (let ((sn-loc (sn-opnd loc sn)) (opnd (Scar opnds)))
      (move-opnd-to-loc68 opnd atemp1 sn-loc)
      (let loop ((pattern pattern))
        (if (SFX<= pattern 3)
            (if (SFX= pattern 3)
                (move-opnd68-to-loc (make-pdec atemp1) loc sn)
                (move-opnd68-to-loc (make-ind atemp1) loc sn))
            (begin
              (if (SFXodd? pattern)
                  (emit-move.l (make-pdec atemp1) atemp1)
                  (emit-move.l (make-ind atemp1) atemp1))
              (loop (SFXquotient pattern 2))))))))
(define (gen-set-car! opnds loc sn) (set-bbv-version-limit! #f) 
  (let ((sn-loc (if loc (sn-opnd loc sn) sn)))
    (let* ((first-opnd (Scar opnds))
           (second-opnd (Scadr opnds))
           (sn-second-opnd (sn-opnd second-opnd sn-loc)))
      (move-opnd-to-loc68 first-opnd atemp1 sn-second-opnd)
      (move-opnd-to-loc68 second-opnd (make-ind atemp1) sn-loc)
      (if (and loc (not (eq? first-opnd loc)))
          (move-opnd68-to-loc atemp1 loc sn)))))
(define (gen-set-cdr! opnds loc sn) (set-bbv-version-limit! #f) 
  (let ((sn-loc (if loc (sn-opnd loc sn) sn)))
    (let* ((first-opnd (Scar opnds))
           (second-opnd (Scadr opnds))
           (sn-second-opnd (sn-opnd second-opnd sn-loc)))
      (move-opnd-to-loc68 first-opnd atemp1 sn-second-opnd)
      (if (and loc (not (eq? first-opnd loc)))
          (move-opnd-to-loc68
           second-opnd
           (make-disp atemp1 (SFX- pointer-size))
           sn-loc)
          (move-opnd-to-loc68 second-opnd (make-pdec atemp1) sn-loc))
      (if (and loc (not (eq? first-opnd loc)))
          (move-opnd68-to-loc atemp1 loc sn)))))
(define (commut-oper gen opnds loc sn self? accum-self accum-other) (set-bbv-version-limit! #f) 
  (if (null? opnds)
      (gen (Sreverse accum-self) (Sreverse accum-other) loc sn self?)
      (let ((opnd (Scar opnds)) (rest (Scdr opnds)))
        (cond ((and (not self?) (eq? opnd loc))
               (commut-oper gen rest loc sn #t accum-self accum-other))
              ((contains-opnd? loc opnd)
               (commut-oper
                gen
                rest
                loc
                sn
                self?
                (cons opnd accum-self)
                accum-other))
              (else
               (commut-oper
                gen
                rest
                loc
                sn
                self?
                accum-self
                (cons opnd accum-other)))))))
(define (gen-add-in-place opnds loc68 sn) (set-bbv-version-limit! #f) 
  (if (not (null? opnds))
      (let* ((first-opnd (Scar opnds))
             (other-opnds (Scdr opnds))
             (sn-other-opnds (sn-opnds other-opnds sn))
             (sn-first-opnd (sn-opnd first-opnd sn-other-opnds))
             (opnd68 (opnd->opnd68
                      first-opnd
                      (temp-in-opnd68 loc68)
                      (sn-opnd68 loc68 sn))))
        (make-top-of-frame-if-stk-opnds68 opnd68 loc68 sn-other-opnds)
        (if (imm? opnd68)
            (add-n-to-loc68
             (imm-val opnd68)
             (opnd68->true-opnd68 loc68 sn-other-opnds))
            (let ((opnd68* (opnd68->true-opnd68 opnd68 sn-other-opnds)))
              (if (or (dreg? opnd68) (reg68? loc68))
                  (emit-add.l
                   opnd68*
                   (opnd68->true-opnd68 loc68 sn-other-opnds))
                  (begin
                    (move-opnd68-to-loc68 opnd68* dtemp1)
                    (emit-add.l
                     dtemp1
                     (opnd68->true-opnd68 loc68 sn-other-opnds))))))
        (gen-add-in-place other-opnds loc68 sn))))
(define (gen-add self-opnds other-opnds loc sn self?) (set-bbv-version-limit! #f) 
  (let* ((opnds (Sappend self-opnds other-opnds))
         (first-opnd (Scar opnds))
         (other-opnds (Scdr opnds))
         (sn-other-opnds (sn-opnds other-opnds sn))
         (sn-first-opnd (sn-opnd first-opnd sn-other-opnds)))
    (if (SFX<= (Slength self-opnds) 1)
        (let ((loc68 (loc->loc68 loc #f sn-first-opnd)))
          (if self?
              (gen-add-in-place opnds loc68 sn)
              (begin
                (move-opnd-to-loc68 first-opnd loc68 sn-other-opnds)
                (gen-add-in-place other-opnds loc68 sn))))
        (begin
          (move-opnd-to-loc68 first-opnd dtemp1 (sn-opnd loc sn-other-opnds))
          (gen-add-in-place other-opnds dtemp1 (sn-opnd loc sn))
          (if self?
              (let ((loc68 (loc->loc68 loc dtemp1 sn)))
                (make-top-of-frame-if-stk-opnd68 loc68 sn)
                (emit-add.l dtemp1 (opnd68->true-opnd68 loc68 sn)))
              (move-opnd68-to-loc dtemp1 loc sn))))))
(define (gen-sub-in-place opnds loc68 sn) (set-bbv-version-limit! #f) 
  (if (not (null? opnds))
      (let* ((first-opnd (Scar opnds))
             (other-opnds (Scdr opnds))
             (sn-other-opnds (sn-opnds other-opnds sn))
             (sn-first-opnd (sn-opnd first-opnd sn-other-opnds))
             (opnd68 (opnd->opnd68
                      first-opnd
                      (temp-in-opnd68 loc68)
                      (sn-opnd68 loc68 sn))))
        (make-top-of-frame-if-stk-opnds68 opnd68 loc68 sn-other-opnds)
        (if (imm? opnd68)
            (add-n-to-loc68
             (SFX- (imm-val opnd68))
             (opnd68->true-opnd68 loc68 sn-other-opnds))
            (let ((opnd68* (opnd68->true-opnd68 opnd68 sn-other-opnds)))
              (if (or (dreg? opnd68) (reg68? loc68))
                  (emit-sub.l
                   opnd68*
                   (opnd68->true-opnd68 loc68 sn-other-opnds))
                  (begin
                    (move-opnd68-to-loc68 opnd68* dtemp1)
                    (emit-sub.l
                     dtemp1
                     (opnd68->true-opnd68 loc68 sn-other-opnds))))))
        (gen-sub-in-place other-opnds loc68 sn))))
(define (gen-sub first-opnd other-opnds loc sn self-opnds?) (set-bbv-version-limit! #f) 
  (if (null? other-opnds)
      (if (and (or (reg? loc) (stk? loc)) (not (eq? loc return-reg)))
          (begin
            (copy-opnd-to-loc first-opnd loc (sn-opnd loc sn))
            (let ((loc68 (loc->loc68 loc #f sn)))
              (make-top-of-frame-if-stk-opnd68 loc68 sn)
              (emit-neg.l (opnd68->true-opnd68 loc68 sn))))
          (begin
            (move-opnd-to-loc68 first-opnd dtemp1 (sn-opnd loc sn))
            (emit-neg.l dtemp1)
            (move-opnd68-to-loc dtemp1 loc sn)))
      (let* ((sn-other-opnds (sn-opnds other-opnds sn))
             (sn-first-opnd (sn-opnd first-opnd sn-other-opnds)))
        (if (and (not self-opnds?) (or (reg? loc) (stk? loc)))
            (let ((loc68 (loc->loc68 loc #f sn-first-opnd)))
              (if (not (eq? first-opnd loc))
                  (move-opnd-to-loc68 first-opnd loc68 sn-other-opnds))
              (gen-sub-in-place other-opnds loc68 sn))
            (begin
              (move-opnd-to-loc68
               first-opnd
               dtemp1
               (sn-opnd loc sn-other-opnds))
              (gen-sub-in-place other-opnds dtemp1 (sn-opnd loc sn))
              (move-opnd68-to-loc dtemp1 loc sn))))))
(define (gen-mul-in-place opnds reg68 sn) (set-bbv-version-limit! #f) 
  (if (not (null? opnds))
      (let* ((first-opnd (Scar opnds))
             (other-opnds (Scdr opnds))
             (sn-other-opnds (sn-opnds other-opnds sn))
             (opnd68 (opnd->opnd68 first-opnd (temp-in-opnd68 reg68) sn)))
        (make-top-of-frame-if-stk-opnd68 opnd68 sn-other-opnds)
        (if (imm? opnd68)
            (mul-n-to-reg68 (SFXquotient (imm-val opnd68) 8) reg68)
            (begin
              (emit-asr.l (make-imm 3) reg68)
              (emit-muls.l (opnd68->true-opnd68 opnd68 sn-other-opnds) reg68)))
        (gen-mul-in-place other-opnds reg68 sn))))
(define (gen-mul self-opnds other-opnds loc sn self?) (set-bbv-version-limit! #f) 
  (let* ((opnds (Sappend self-opnds other-opnds))
         (first-opnd (Scar opnds))
         (other-opnds (Scdr opnds))
         (sn-other-opnds (sn-opnds other-opnds sn))
         (sn-first-opnd (sn-opnd first-opnd sn-other-opnds)))
    (if (null? self-opnds)
        (let ((loc68 (loc->loc68 loc #f sn-first-opnd)))
          (if self?
              (gen-mul-in-place opnds loc68 sn)
              (begin
                (move-opnd-to-loc68 first-opnd loc68 sn-other-opnds)
                (gen-mul-in-place other-opnds loc68 sn))))
        (begin
          (move-opnd-to-loc68 first-opnd dtemp1 (sn-opnd loc sn-other-opnds))
          (gen-mul-in-place other-opnds dtemp1 (sn-opnd loc sn))
          (if self?
              (let ((loc68 (loc->loc68 loc dtemp1 sn)))
                (make-top-of-frame-if-stk-opnd68 loc68 sn)
                (emit-asr.l (make-imm 3) dtemp1)
                (emit-muls.l dtemp1 (opnd68->true-opnd68 loc68 sn)))
              (move-opnd68-to-loc dtemp1 loc sn))))))
(define (gen-div-in-place opnds reg68 sn) (set-bbv-version-limit! #f) 
  (if (not (null? opnds))
      (let* ((first-opnd (Scar opnds))
             (other-opnds (Scdr opnds))
             (sn-other-opnds (sn-opnds other-opnds sn))
             (sn-first-opnd (sn-opnd first-opnd sn-other-opnds))
             (opnd68 (opnd->opnd68 first-opnd (temp-in-opnd68 reg68) sn)))
        (make-top-of-frame-if-stk-opnd68 opnd68 sn-other-opnds)
        (if (imm? opnd68)
            (let ((n (SFXquotient (imm-val opnd68) 8)))
              (div-n-to-reg68 n reg68)
              (if (SFX> (abs n) 1) (emit-and.w (make-imm -8) reg68)))
            (let ((opnd68* (opnd68->true-opnd68 opnd68 sn-other-opnds)))
              (emit-divsl.l opnd68* reg68 reg68)
              (emit-asl.l (make-imm 3) reg68)))
        (gen-div-in-place other-opnds reg68 sn))))
(define (gen-div first-opnd other-opnds loc sn self-opnds?) (set-bbv-version-limit! #f) 
  (if (null? other-opnds)
      (begin
        (move-opnd-to-loc68 first-opnd pdec-sp (sn-opnd loc sn))
        (emit-moveq 8 dtemp1)
        (emit-divsl.l pinc-sp dtemp1 dtemp1)
        (emit-asl.l (make-imm 3) dtemp1)
        (emit-and.w (make-imm -8) dtemp1)
        (move-opnd68-to-loc dtemp1 loc sn))
      (let* ((sn-other-opnds (sn-opnds other-opnds sn))
             (sn-first-opnd (sn-opnd first-opnd sn-other-opnds)))
        (if (and (reg? loc) (not self-opnds?) (not (eq? loc return-reg)))
            (let ((reg68 (reg->reg68 loc)))
              (if (not (eq? first-opnd loc))
                  (move-opnd-to-loc68 first-opnd reg68 sn-other-opnds))
              (gen-div-in-place other-opnds reg68 sn))
            (begin
              (move-opnd-to-loc68
               first-opnd
               dtemp1
               (sn-opnd loc sn-other-opnds))
              (gen-div-in-place other-opnds dtemp1 (sn-opnd loc sn))
              (move-opnd68-to-loc dtemp1 loc sn))))))
(define (gen-rem first-opnd second-opnd loc sn) (set-bbv-version-limit! #f) 
  (let* ((sn-loc (sn-opnd loc sn))
         (sn-second-opnd (sn-opnd second-opnd sn-loc)))
    (move-opnd-to-loc68 first-opnd dtemp1 sn-second-opnd)
    (let ((opnd68 (opnd->opnd68 second-opnd #f sn-loc))
          (reg68 (if (and (reg? loc) (not (eq? loc return-reg)))
                     (reg->reg68 loc)
                     false-reg)))
      (make-top-of-frame-if-stk-opnd68 opnd68 sn-loc)
      (let ((opnd68* (if (areg? opnd68)
                         (begin (emit-move.l opnd68 reg68) reg68)
                         (opnd68->true-opnd68 opnd68 sn-loc))))
        (emit-divsl.l opnd68* reg68 dtemp1))
      (move-opnd68-to-loc reg68 loc sn)
      (if (not (and (reg? loc) (not (eq? loc return-reg))))
          (emit-move.l (make-imm bits-false) false-reg)))))
(define (gen-mod first-opnd second-opnd loc sn) (set-bbv-version-limit! #f) 
  (let* ((sn-loc (sn-opnd loc sn))
         (sn-first-opnd (sn-opnd first-opnd sn-loc))
         (sn-second-opnd (sn-opnd second-opnd sn-first-opnd))
         (opnd68 (opnd->opnd68 second-opnd #f sn-second-opnd)))
    (define (general-case) (set-bbv-version-limit! #f) 
      (let ((lbl1 (new-lbl!))
            (lbl2 (new-lbl!))
            (lbl3 (new-lbl!))
            (opnd68** (opnd68->true-opnd68 opnd68 sn-second-opnd))
            (opnd68* (opnd68->true-opnd68
                      (opnd->opnd68 first-opnd #f sn-second-opnd)
                      sn-second-opnd)))
        (move-opnd68-to-loc68 opnd68* dtemp1)
        (move-opnd68-to-loc68 opnd68** false-reg)
        (emit-divsl.l false-reg false-reg dtemp1)
        (emit-move.l false-reg false-reg)
        (emit-beq lbl3)
        (move-opnd68-to-loc68 opnd68* dtemp1)
        (emit-bmi lbl1)
        (move-opnd68-to-loc68 opnd68** dtemp1)
        (emit-bpl lbl3)
        (emit-bra lbl2)
        (emit-label lbl1)
        (move-opnd68-to-loc68 opnd68** dtemp1)
        (emit-bmi lbl3)
        (emit-label lbl2)
        (emit-add.l dtemp1 false-reg)
        (emit-label lbl3)
        (move-opnd68-to-loc false-reg loc sn)
        (emit-move.l (make-imm bits-false) false-reg)))
    (make-top-of-frame-if-stk-opnd68 opnd68 sn-first-opnd)
    (if (imm? opnd68)
        (let ((n (SFXquotient (imm-val opnd68) 8)))
          (if (SFX> n 0)
              (let ((shift (power-of-2 n)))
                (if shift
                    (let ((reg68 (if (and (reg? loc)
                                          (not (eq? loc return-reg)))
                                     (reg->reg68 loc)
                                     dtemp1)))
                      (move-opnd-to-loc68 first-opnd reg68 sn-loc)
                      (emit-and.l (make-imm (SFX* (SFX- n 1) 8)) reg68)
                      (move-opnd68-to-loc reg68 loc sn))
                    (general-case)))
              (general-case)))
        (general-case))))
(define (gen-op emit-op dst-ok?) (set-bbv-version-limit! #f) 
  (define (gen-op-in-place opnds loc68 sn) (set-bbv-version-limit! #f) 
    (if (not (null? opnds))
        (let* ((first-opnd (Scar opnds))
               (other-opnds (Scdr opnds))
               (sn-other-opnds (sn-opnds other-opnds sn))
               (sn-first-opnd (sn-opnd first-opnd sn-other-opnds))
               (opnd68 (opnd->opnd68
                        first-opnd
                        (temp-in-opnd68 loc68)
                        (sn-opnd68 loc68 sn))))
          (make-top-of-frame-if-stk-opnds68 opnd68 loc68 sn-other-opnds)
          (if (imm? opnd68)
              (emit-op opnd68 (opnd68->true-opnd68 loc68 sn-other-opnds))
              (let ((opnd68* (opnd68->true-opnd68 opnd68 sn-other-opnds)))
                (if (or (dreg? opnd68) (dst-ok? loc68))
                    (emit-op opnd68*
                             (opnd68->true-opnd68 loc68 sn-other-opnds))
                    (begin
                      (move-opnd68-to-loc68 opnd68* dtemp1)
                      (emit-op dtemp1
                               (opnd68->true-opnd68 loc68 sn-other-opnds))))))
          (gen-op-in-place other-opnds loc68 sn))))
  (lambda (self-opnds other-opnds loc sn self?) (set-bbv-version-limit! #f) 
    (let* ((opnds (Sappend self-opnds other-opnds))
           (first-opnd (Scar opnds))
           (other-opnds (Scdr opnds))
           (sn-other-opnds (sn-opnds other-opnds sn))
           (sn-first-opnd (sn-opnd first-opnd sn-other-opnds)))
      (if (SFX<= (Slength self-opnds) 1)
          (let ((loc68 (loc->loc68 loc #f sn-first-opnd)))
            (if self?
                (gen-op-in-place opnds loc68 sn)
                (begin
                  (move-opnd-to-loc68 first-opnd loc68 sn-other-opnds)
                  (gen-op-in-place other-opnds loc68 sn))))
          (begin
            (move-opnd-to-loc68 first-opnd dtemp1 (sn-opnd loc sn-other-opnds))
            (gen-op-in-place other-opnds dtemp1 (sn-opnd loc sn))
            (if self?
                (let ((loc68 (loc->loc68 loc dtemp1 sn)))
                  (make-top-of-frame-if-stk-opnd68 loc68 sn)
                  (emit-op dtemp1 (opnd68->true-opnd68 loc68 sn)))
                (move-opnd68-to-loc dtemp1 loc sn)))))))
(define gen-logior (gen-op emit-or.l dreg?))
(define gen-logxor (gen-op emit-eor.l (lambda (x) (set-bbv-version-limit! #f)  #f)))
(define gen-logand (gen-op emit-and.l dreg?))
(define (gen-shift right-shift) (set-bbv-version-limit! #f) 
  (lambda (opnds loc sn) (set-bbv-version-limit! #f) 
    (let ((sn-loc (sn-opnd loc sn)))
      (let* ((opnd1 (Scar opnds))
             (opnd2 (Scadr opnds))
             (sn-opnd1 (sn-opnd opnd1 sn-loc))
             (o2 (opnd->opnd68 opnd2 #f sn-opnd1)))
        (make-top-of-frame-if-stk-opnd68 o2 sn-opnd1)
        (if (imm? o2)
            (let* ((reg68 (if (and (reg? loc) (not (eq? loc return-reg)))
                              (reg->reg68 loc)
                              dtemp1))
                   (n (SFXquotient (imm-val o2) 8))
                   (emit-shft (if (SFX> n 0) emit-lsl.l right-shift)))
              (move-opnd-to-loc68 opnd1 reg68 sn-loc)
              (let loop ((i (min (abs n) 29)))
                (if (SFX> i 0)
                    (begin
                      (emit-shft (make-imm (min i 8)) reg68)
                      (loop (SFX- i 8)))))
              (if (SFX< n 0) (emit-and.w (make-imm -8) reg68))
              (move-opnd68-to-loc reg68 loc sn))
            (let* ((reg68 (if (and (reg? loc) (not (eq? loc return-reg)))
                              (reg->reg68 loc)
                              dtemp1))
                   (reg68* (if (and (reg? loc) (not (eq? loc return-reg)))
                               dtemp1
                               false-reg))
                   (lbl1 (new-lbl!))
                   (lbl2 (new-lbl!)))
              (emit-move.l (opnd68->true-opnd68 o2 sn-opnd1) reg68*)
              (move-opnd-to-loc68 opnd1 reg68 sn-loc)
              (emit-asr.l (make-imm 3) reg68*)
              (emit-bmi lbl1)
              (emit-lsl.l reg68* reg68)
              (emit-bra lbl2)
              (emit-label lbl1)
              (emit-neg.l reg68*)
              (right-shift reg68* reg68)
              (emit-and.w (make-imm -8) reg68)
              (emit-label lbl2)
              (move-opnd68-to-loc reg68 loc sn)
              (if (not (and (reg? loc) (not (eq? loc return-reg))))
                  (emit-move.l (make-imm bits-false) false-reg))))))))
(define (flo-oper oper1 oper2 opnds loc sn) (set-bbv-version-limit! #f) 
  (gen-guarantee-space 2)
  (move-opnd-to-loc68
   (Scar opnds)
   atemp1
   (sn-opnds (Scdr opnds) (sn-opnd loc sn)))
  (oper1 (make-disp* atemp1 (SFX- type-flonum)) ftemp1)
  (let loop ((opnds (Scdr opnds)))
    (if (not (null? opnds))
        (let* ((opnd (Scar opnds))
               (other-opnds (Scdr opnds))
               (sn-other-opnds (sn-opnds other-opnds sn)))
          (move-opnd-to-loc68 opnd atemp1 sn-other-opnds)
          (oper2 (make-disp* atemp1 (SFX- type-flonum)) ftemp1)
          (loop (Scdr opnds)))))
  (add-n-to-loc68 (SFX* -2 pointer-size) heap-reg)
  (emit-fmov.dx ftemp1 (make-ind heap-reg))
  (let ((reg68 (if (reg? loc) (reg->reg68 loc) atemp1)))
    (emit-move.l heap-reg reg68)
    (emit-addq.l type-flonum reg68))
  (if (not (reg? loc)) (move-opnd68-to-loc atemp1 loc sn)))
(define (gen-make-placeholder opnds loc sn) (set-bbv-version-limit! #f) 
  (let ((sn-loc (sn-opnd loc sn)))
    (let ((opnd (Scar opnds)))
      (gen-guarantee-space 4)
      (emit-clr.l (make-pdec heap-reg))
      (move-opnd-to-loc68 opnd (make-pdec heap-reg) sn-loc)
      (emit-move.l null-reg (make-pdec heap-reg))
      (move-opnd68-to-loc68 heap-reg atemp2)
      (emit-addq.l (SFXmodulo (SFX- type-placeholder type-pair) 8) atemp2)
      (emit-move.l atemp2 (make-pdec heap-reg))
      (move-opnd68-to-loc atemp2 loc sn))))
(define (gen-subprocedure-id opnds loc sn) (set-bbv-version-limit! #f) 
  (let ((sn-loc (sn-opnd loc sn))
        (opnd (Scar opnds))
        (reg68 (if (and (reg? loc) (not (eq? loc return-reg)))
                   (reg->reg68 loc)
                   dtemp1)))
    (move-opnd-to-loc68 opnd atemp1 sn-loc)
    (move-n-to-loc68 32768 reg68)
    (emit-sub.w (make-disp* atemp1 -2) reg68)
    (move-opnd68-to-loc reg68 loc sn)))
(define (gen-subprocedure-parent opnds loc sn) (set-bbv-version-limit! #f) 
  (let ((sn-loc (sn-opnd loc sn)) (opnd (Scar opnds)))
    (move-opnd-to-loc68 opnd atemp1 sn-loc)
    (emit-add.w (make-disp* atemp1 -2) atemp1)
    (add-n-to-loc68 -32768 atemp1)
    (move-opnd68-to-loc atemp1 loc sn)))
(define (gen-return-fs opnds loc sn) (set-bbv-version-limit! #f) 
  (let ((sn-loc (sn-opnd loc sn))
        (opnd (Scar opnds))
        (reg68 (if (and (reg? loc) (not (eq? loc return-reg)))
                   (reg->reg68 loc)
                   dtemp1))
        (lbl (new-lbl!)))
    (move-opnd-to-loc68 opnd atemp1 sn-loc)
    (emit-moveq 0 reg68)
    (emit-move.w (make-disp* atemp1 -6) reg68)
    (emit-beq lbl)
    (emit-and.w (make-imm 32767) reg68)
    (emit-subq.l 8 reg68)
    (emit-label lbl)
    (emit-addq.l 8 reg68)
    (emit-asl.l (make-imm 1) reg68)
    (move-opnd68-to-loc reg68 loc sn)))
(define (gen-return-link opnds loc sn) (set-bbv-version-limit! #f) 
  (let ((sn-loc (sn-opnd loc sn))
        (opnd (Scar opnds))
        (reg68 (if (and (reg? loc) (not (eq? loc return-reg)))
                   (reg->reg68 loc)
                   dtemp1))
        (lbl (new-lbl!)))
    (move-opnd-to-loc68 opnd atemp1 sn-loc)
    (emit-moveq 0 reg68)
    (emit-move.w (make-disp* atemp1 -6) reg68)
    (emit-beq lbl)
    (emit-and.w (make-imm 32767) reg68)
    (emit-subq.l 8 reg68)
    (emit-label lbl)
    (emit-addq.l 8 reg68)
    (emit-sub.w (make-disp* atemp1 -4) reg68)
    (emit-asl.l (make-imm 1) reg68)
    (move-opnd68-to-loc reg68 loc sn)))
(define (gen-procedure-info opnds loc sn) (set-bbv-version-limit! #f) 
  (let ((sn-loc (sn-opnd loc sn)) (opnd (Scar opnds)))
    (move-opnd-to-loc68 opnd atemp1 sn-loc)
    (emit-add.w (make-disp* atemp1 -2) atemp1)
    (move-opnd68-to-loc (make-disp* atemp1 (SFX- 32768 6)) loc sn)))
(define (gen-guarantee-space n) (set-bbv-version-limit! #f) 
  (set! pointers-allocated (SFX+ pointers-allocated n))
  (if (SFX> pointers-allocated heap-allocation-fudge)
      (begin (gen-guarantee-fudge) (set! pointers-allocated n))))
(define (gen-guarantee-fudge) (set-bbv-version-limit! #f) 
  (if (SFX> pointers-allocated 0)
      (let ((lbl (new-lbl!)))
        (emit-cmp.l heap-lim-slot heap-reg)
        (emit-bcc lbl)
        (gen-trap instr-source entry-frame #f #f heap-alloc1-trap lbl)
        (set! pointers-allocated 0))))
(define pointers-allocated '())
(define (gen-type opnds loc sn) (set-bbv-version-limit! #f) 
  (let* ((sn-loc (sn-opnd loc sn))
         (opnd (Scar opnds))
         (reg68 (if (and (reg? loc) (not (eq? loc return-reg)))
                    (reg->reg68 loc)
                    dtemp1)))
    (move-opnd-to-loc68 opnd reg68 sn-loc)
    (emit-and.l (make-imm 7) reg68)
    (emit-asl.l (make-imm 3) reg68)
    (move-opnd68-to-loc reg68 loc sn)))
(define (gen-type-cast opnds loc sn) (set-bbv-version-limit! #f) 
  (let ((sn-loc (if loc (sn-opnd loc sn) sn)))
    (let ((first-opnd (Scar opnds)) (second-opnd (Scadr opnds)))
      (let* ((sn-loc (if (and loc (not (eq? first-opnd loc))) sn-loc sn))
             (o1 (opnd->opnd68 first-opnd #f (sn-opnd second-opnd sn-loc)))
             (o2 (opnd->opnd68 second-opnd (temp-in-opnd68 o1) sn-loc))
             (reg68 (if (and (reg? loc) (not (eq? loc return-reg)))
                        (reg->reg68 loc)
                        dtemp1)))
        (make-top-of-frame-if-stk-opnds68 o1 o2 sn-loc)
        (move-opnd68-to-loc68
         (opnd68->true-opnd68 o1 (sn-opnd68 o2 sn-loc))
         reg68)
        (emit-and.w (make-imm -8) reg68)
        (if (imm? o2)
            (let ((n (SFXquotient (imm-val o2) 8)))
              (if (SFX> n 0) (emit-addq.w n reg68)))
            (begin
              (move-opnd68-to-loc68 (opnd68->true-opnd68 o2 sn-loc) atemp1)
              (emit-exg atemp1 reg68)
              (emit-asr.l (make-imm 3) reg68)
              (emit-add.l atemp1 reg68)))
        (move-opnd68-to-loc reg68 loc sn)))))
(define (gen-subtype opnds loc sn) (set-bbv-version-limit! #f) 
  (let ((sn-loc (sn-opnd loc sn))
        (opnd (Scar opnds))
        (reg68 (if (and (reg? loc) (not (eq? loc return-reg)))
                   (reg->reg68 loc)
                   dtemp1)))
    (move-opnd-to-loc68 opnd atemp1 sn-loc)
    (emit-moveq 0 reg68)
    (emit-move.b (make-ind atemp1) reg68)
    (move-opnd68-to-loc reg68 loc sn)))
(define (gen-subtype-set! opnds loc sn) (set-bbv-version-limit! #f) 
  (let ((sn-loc (if loc (sn-opnd loc sn) sn)))
    (let ((first-opnd (Scar opnds)) (second-opnd (Scadr opnds)))
      (let* ((sn-loc (if (and loc (not (eq? first-opnd loc))) sn-loc sn))
             (o1 (opnd->opnd68 first-opnd #f (sn-opnd second-opnd sn-loc)))
             (o2 (opnd->opnd68 second-opnd (temp-in-opnd68 o1) sn-loc)))
        (make-top-of-frame-if-stk-opnds68 o1 o2 sn-loc)
        (move-opnd68-to-loc68
         (opnd68->true-opnd68 o1 (sn-opnd68 o2 sn-loc))
         atemp1)
        (if (imm? o2)
            (emit-move.b (make-imm (imm-val o2)) (make-ind atemp1))
            (begin
              (move-opnd68-to-loc68 (opnd68->true-opnd68 o2 sn-loc) dtemp1)
              (emit-move.b dtemp1 (make-ind atemp1))))
        (if (and loc (not (eq? first-opnd loc)))
            (move-opnd68-to-loc atemp1 loc sn))))))
(define (vector-select kind vector string vector8 vector16) (set-bbv-version-limit! #f) 
  (case kind
    ((string) string)
    ((vector8) vector8)
    ((vector16) vector16)
    (else vector)))
(define (obj-vector? kind) (set-bbv-version-limit! #f)  (vector-select kind #t #f #f #f))
(define (make-gen-vector kind) (set-bbv-version-limit! #f) 
  (lambda (opnds loc sn) (set-bbv-version-limit! #f) 
    (let ((sn-loc (if loc (sn-opnd loc sn) sn)))
      (let* ((n (Slength opnds))
             (bytes (SFX+ pointer-size
                       (SFX* (vector-select kind 4 1 1 2)
                          (SFX+ n (if (eq? kind 'string) 1 0)))))
             (adjust (SFXmodulo (SFX- bytes) 8)))
        (gen-guarantee-space
         (SFXquotient (SFX* (SFXquotient (SFX+ bytes (SFX- 8 1)) 8) 8) pointer-size))
        (if (not (SFX= adjust 0)) (emit-subq.l adjust heap-reg))
        (if (eq? kind 'string) (emit-move.b (make-imm 0) (make-pdec heap-reg)))
        (let loop ((opnds (Sreverse opnds)))
          (if (pair? opnds)
              (let* ((o (Scar opnds)) (sn-o (sn-opnds (Scdr opnds) sn-loc)))
                (if (eq? kind 'vector)
                    (move-opnd-to-loc68 o (make-pdec heap-reg) sn-o)
                    (begin
                      (move-opnd-to-loc68 o dtemp1 sn-o)
                      (emit-asr.l (make-imm 3) dtemp1)
                      (if (eq? kind 'vector16)
                          (emit-move.w dtemp1 (make-pdec heap-reg))
                          (emit-move.b dtemp1 (make-pdec heap-reg)))))
                (loop (Scdr opnds)))))
        (emit-move.l
         (make-imm
          (SFX+ (SFX* 256 (SFX- bytes pointer-size))
             (SFX* 8 (if (eq? kind 'vector) subtype-vector subtype-string))))
         (make-pdec heap-reg))
        (if loc
            (begin
              (emit-lea (make-disp* heap-reg type-subtyped) atemp2)
              (move-opnd68-to-loc atemp2 loc sn)))))))
(define (make-gen-vector-length kind) (set-bbv-version-limit! #f) 
  (lambda (opnds loc sn) (set-bbv-version-limit! #f) 
    (let ((sn-loc (sn-opnd loc sn))
          (opnd (Scar opnds))
          (reg68 (if (and (reg? loc) (not (eq? loc return-reg)))
                     (reg->reg68 loc)
                     dtemp1)))
      (move-opnd-to-loc68 opnd atemp1 sn-loc)
      (move-opnd68-to-loc68 (make-disp* atemp1 (SFX- type-subtyped)) reg68)
      (emit-lsr.l (make-imm (vector-select kind 7 5 5 6)) reg68)
      (if (not (eq? kind 'vector))
          (begin
            (emit-and.w (make-imm -8) reg68)
            (if (eq? kind 'string) (emit-subq.l 8 reg68))))
      (move-opnd68-to-loc reg68 loc sn))))
(define (make-gen-vector-ref kind) (set-bbv-version-limit! #f) 
  (lambda (opnds loc sn) (set-bbv-version-limit! #f) 
    (let ((sn-loc (sn-opnd loc sn)))
      (let ((first-opnd (Scar opnds))
            (second-opnd (Scadr opnds))
            (reg68 (if (and (reg? loc) (not (eq? loc return-reg)))
                       (reg->reg68 loc)
                       dtemp1)))
        (let* ((o2 (opnd->opnd68 second-opnd #f (sn-opnd first-opnd sn-loc)))
               (o1 (opnd->opnd68 first-opnd (temp-in-opnd68 o2) sn-loc)))
          (make-top-of-frame-if-stk-opnds68 o1 o2 sn-loc)
          (let* ((offset (if (eq? kind 'closure)
                             (SFX- pointer-size type-procedure)
                             (SFX- pointer-size type-subtyped)))
                 (loc68 (if (imm? o2)
                            (begin
                              (move-opnd68-to-loc68
                               (opnd68->true-opnd68 o1 sn-loc)
                               atemp1)
                              (make-disp*
                               atemp1
                               (SFX+ (SFXquotient
                                   (imm-val o2)
                                   (vector-select kind 2 8 8 4))
                                  offset)))
                            (begin
                              (move-opnd68-to-loc68
                               (opnd68->true-opnd68 o2 (sn-opnd68 o1 sn-loc))
                               dtemp1)
                              (emit-asr.l
                               (make-imm (vector-select kind 1 3 3 2))
                               dtemp1)
                              (move-opnd68-to-loc68
                               (opnd68->true-opnd68 o1 sn-loc)
                               atemp1)
                              (if (and (identical-opnd68? reg68 dtemp1)
                                       (not (obj-vector? kind)))
                                  (begin
                                    (emit-move.l dtemp1 atemp2)
                                    (make-inx atemp1 atemp2 offset))
                                  (make-inx atemp1 dtemp1 offset))))))
            (if (not (obj-vector? kind)) (emit-moveq 0 reg68))
            (case kind
              ((string vector8) (emit-move.b loc68 reg68))
              ((vector16) (emit-move.w loc68 reg68))
              (else (emit-move.l loc68 reg68)))
            (if (not (obj-vector? kind))
                (begin
                  (emit-asl.l (make-imm 3) reg68)
                  (if (eq? kind 'string) (emit-addq.w type-special reg68))))
            (move-opnd68-to-loc reg68 loc sn)))))))
(define (make-gen-vector-set! kind) (set-bbv-version-limit! #f) 
  (lambda (opnds loc sn) (set-bbv-version-limit! #f) 
    (let ((sn-loc (if loc (sn-opnd loc sn) sn)))
      (let ((first-opnd (Scar opnds))
            (second-opnd (Scadr opnds))
            (third-opnd (Scaddr opnds)))
        (let* ((sn-loc (if (and loc (not (eq? first-opnd loc)))
                           (sn-opnd first-opnd sn-loc)
                           sn))
               (sn-third-opnd (sn-opnd third-opnd sn-loc))
               (o2 (opnd->opnd68
                    second-opnd
                    #f
                    (sn-opnd first-opnd sn-third-opnd)))
               (o1 (opnd->opnd68
                    first-opnd
                    (temp-in-opnd68 o2)
                    sn-third-opnd)))
          (make-top-of-frame-if-stk-opnds68 o1 o2 sn-third-opnd)
          (let* ((offset (if (eq? kind 'closure)
                             (SFX- pointer-size type-procedure)
                             (SFX- pointer-size type-subtyped)))
                 (loc68 (if (imm? o2)
                            (begin
                              (move-opnd68-to-loc68
                               (opnd68->true-opnd68 o1 sn-third-opnd)
                               atemp1)
                              (make-disp*
                               atemp1
                               (SFX+ (SFXquotient
                                   (imm-val o2)
                                   (vector-select kind 2 8 8 4))
                                  offset)))
                            (begin
                              (move-opnd68-to-loc68
                               (opnd68->true-opnd68 o2 (sn-opnd68 o1 sn-loc))
                               dtemp1)
                              (emit-asr.l
                               (make-imm (vector-select kind 1 3 3 2))
                               dtemp1)
                              (move-opnd68-to-loc68
                               (opnd68->true-opnd68 o1 sn-loc)
                               atemp1)
                              (if (obj-vector? kind)
                                  (make-inx atemp1 dtemp1 offset)
                                  (begin
                                    (emit-move.l dtemp1 atemp2)
                                    (make-inx atemp1 atemp2 offset)))))))
            (if (obj-vector? kind)
                (move-opnd-to-loc68 third-opnd loc68 sn-loc)
                (begin
                  (move-opnd-to-loc68 third-opnd dtemp1 sn-loc)
                  (emit-asr.l (make-imm 3) dtemp1)
                  (if (eq? kind 'vector16)
                      (emit-move.w dtemp1 loc68)
                      (emit-move.b dtemp1 loc68))))
            (if (and loc (not (eq? first-opnd loc)))
                (copy-opnd-to-loc first-opnd loc sn))))))))
(define (make-gen-vector-shrink! kind) (set-bbv-version-limit! #f) 
  (lambda (opnds loc sn) (set-bbv-version-limit! #f) 
    (let ((sn-loc (if loc (sn-opnd loc sn) sn)))
      (let ((first-opnd (Scar opnds)) (second-opnd (Scadr opnds)))
        (let* ((sn-loc (if (and loc (not (eq? first-opnd loc)))
                           (sn-opnd first-opnd sn-loc)
                           sn))
               (o2 (opnd->opnd68 second-opnd #f (sn-opnd first-opnd sn-loc)))
               (o1 (opnd->opnd68 first-opnd (temp-in-opnd68 o2) sn-loc)))
          (make-top-of-frame-if-stk-opnds68 o1 o2 sn-loc)
          (move-opnd68-to-loc68
           (opnd68->true-opnd68 o2 (sn-opnd68 o1 sn-loc))
           dtemp1)
          (emit-move.l (opnd68->true-opnd68 o1 sn-loc) atemp1)
          (if (eq? kind 'string)
              (begin
                (emit-asr.l (make-imm 3) dtemp1)
                (emit-move.b
                 (make-imm 0)
                 (make-inx atemp1 dtemp1 (SFX- pointer-size type-subtyped)))
                (emit-addq.l 1 dtemp1)
                (emit-asl.l (make-imm 8) dtemp1))
              (emit-asl.l (make-imm (vector-select kind 7 5 5 6)) dtemp1))
          (emit-move.b (make-ind atemp1) dtemp1)
          (emit-move.l dtemp1 (make-disp* atemp1 (SFX- type-subtyped)))
          (if (and loc (not (eq? first-opnd loc)))
              (move-opnd68-to-loc atemp1 loc sn)))))))
(define (gen-eq-test bits not? opnds lbl fs) (set-bbv-version-limit! #f) 
  (gen-compare* (opnd->opnd68 (Scar opnds) #f fs) (make-imm bits) fs)
  (if not? (emit-bne lbl) (emit-beq lbl)))
(define (gen-compare opnd1 opnd2 fs) (set-bbv-version-limit! #f) 
  (let* ((o1 (opnd->opnd68 opnd1 #f (sn-opnd opnd2 fs)))
         (o2 (opnd->opnd68 opnd2 (temp-in-opnd68 o1) fs)))
    (gen-compare* o1 o2 fs)))
(define (gen-compare* o1 o2 fs) (set-bbv-version-limit! #f) 
  (make-top-of-frame-if-stk-opnds68 o1 o2 fs)
  (let ((order-1-2
         (cond ((imm? o1)
                (cmp-n-to-opnd68 (imm-val o1) (opnd68->true-opnd68 o2 fs)))
               ((imm? o2)
                (not (cmp-n-to-opnd68
                      (imm-val o2)
                      (opnd68->true-opnd68 o1 fs))))
               ((reg68? o1) (emit-cmp.l (opnd68->true-opnd68 o2 fs) o1) #f)
               ((reg68? o2) (emit-cmp.l (opnd68->true-opnd68 o1 fs) o2) #t)
               (else
                (emit-move.l (opnd68->true-opnd68 o1 (sn-opnd68 o2 fs)) dtemp1)
                (emit-cmp.l (opnd68->true-opnd68 o2 fs) dtemp1)
                #f))))
    (shrink-frame fs)
    order-1-2))
(define (gen-compares branch< branch>= branch> branch<= not? opnds lbl fs) (set-bbv-version-limit! #f) 
  (gen-compares*
   gen-compare
   branch<
   branch>=
   branch>
   branch<=
   not?
   opnds
   lbl
   fs))
(define (gen-compares*
         gen-comp
         branch<
         branch>=
         branch>
         branch<=
         not?
         opnds
         lbl
         fs)
  (define (gen-compare-sequence opnd1 opnd2 rest) (set-bbv-version-limit! #f) 
    (if (null? rest)
        (if (gen-comp opnd1 opnd2 fs)
            (if not? (branch<= lbl) (branch> lbl))
            (if not? (branch>= lbl) (branch< lbl)))
        (let ((order-1-2
               (gen-comp opnd1 opnd2 (sn-opnd opnd2 (sn-opnds rest fs)))))
          (if (SFX= current-fs fs)
              (if not?
                  (begin
                    (if order-1-2 (branch<= lbl) (branch>= lbl))
                    (gen-compare-sequence opnd2 (Scar rest) (Scdr rest)))
                  (let ((exit-lbl (new-lbl!)))
                    (if order-1-2 (branch<= exit-lbl) (branch>= exit-lbl))
                    (gen-compare-sequence opnd2 (Scar rest) (Scdr rest))
                    (emit-label exit-lbl)))
              (if not?
                  (let ((next-lbl (new-lbl!)))
                    (if order-1-2 (branch> next-lbl) (branch< next-lbl))
                    (shrink-frame fs)
                    (emit-bra lbl)
                    (emit-label next-lbl)
                    (gen-compare-sequence opnd2 (Scar rest) (Scdr rest)))
                  (let* ((next-lbl (new-lbl!)) (exit-lbl (new-lbl!)))
                    (if order-1-2 (branch> next-lbl) (branch< next-lbl))
                    (shrink-frame fs)
                    (emit-bra exit-lbl)
                    (emit-label next-lbl)
                    (gen-compare-sequence opnd2 (Scar rest) (Scdr rest))
                    (emit-label exit-lbl)))))))
  (if (or (null? opnds) (null? (Scdr opnds)))
      (begin (shrink-frame fs) (if (not not?) (emit-bra lbl)))
      (gen-compare-sequence (Scar opnds) (Scadr opnds) (Scddr opnds))))
(define (gen-compare-flo opnd1 opnd2 fs) (set-bbv-version-limit! #f) 
  (let* ((o1 (opnd->opnd68 opnd1 #f (sn-opnd opnd2 fs)))
         (o2 (opnd->opnd68 opnd2 (temp-in-opnd68 o1) fs)))
    (make-top-of-frame-if-stk-opnds68 o1 o2 fs)
    (emit-move.l (opnd68->true-opnd68 o1 (sn-opnd68 o2 fs)) atemp1)
    (emit-move.l (opnd68->true-opnd68 o2 fs) atemp2)
    (emit-fmov.dx (make-disp* atemp2 (SFX- type-flonum)) ftemp1)
    (emit-fcmp.dx (make-disp* atemp1 (SFX- type-flonum)) ftemp1)
    #t))
(define (gen-compares-flo branch< branch>= branch> branch<= not? opnds lbl fs) (set-bbv-version-limit! #f) 
  (gen-compares*
   gen-compare-flo
   branch<
   branch>=
   branch>
   branch<=
   not?
   opnds
   lbl
   fs))
(define (gen-type-test tag not? opnds lbl fs) (set-bbv-version-limit! #f) 
  (let ((opnd (Scar opnds)))
    (let ((o (opnd->opnd68 opnd #f fs)))
      (define (mask-test set-reg correction) (set-bbv-version-limit! #f) 
        (emit-btst
         (if (SFX= correction 0)
             (if (dreg? o)
                 o
                 (begin
                   (emit-move.l (opnd68->true-opnd68 o fs) dtemp1)
                   dtemp1))
             (begin
               (if (not (eq? o dtemp1))
                   (emit-move.l (opnd68->true-opnd68 o fs) dtemp1))
               (emit-addq.w correction dtemp1)
               dtemp1))
         set-reg))
      (make-top-of-frame-if-stk-opnd68 o fs)
      (cond ((SFX= tag 0)
             (if (eq? o dtemp1)
                 (emit-and.w (make-imm 7) dtemp1)
                 (begin
                   (emit-move.l (opnd68->true-opnd68 o fs) dtemp1)
                   (emit-and.w (make-imm 7) dtemp1))))
            ((SFX= tag type-placeholder) (mask-test placeholder-reg 0))
            (else (mask-test pair-reg (SFXmodulo (SFX- type-pair tag) 8))))
      (shrink-frame fs)
      (if not? (emit-bne lbl) (emit-beq lbl)))))
(define (gen-subtype-test type not? opnds lbl fs) (set-bbv-version-limit! #f) 
  (let ((opnd (Scar opnds)))
    (let ((o (opnd->opnd68 opnd #f fs)) (cont-lbl (new-lbl!)))
      (make-top-of-frame-if-stk-opnd68 o fs)
      (if (not (eq? o dtemp1)) (emit-move.l (opnd68->true-opnd68 o fs) dtemp1))
      (emit-move.l dtemp1 atemp1)
      (emit-addq.w (SFXmodulo (SFX- type-pair type-subtyped) 8) dtemp1)
      (emit-btst dtemp1 pair-reg)
      (shrink-frame fs)
      (if not? (emit-bne lbl) (emit-bne cont-lbl))
      (emit-cmp.b (make-imm (SFX* type 8)) (make-ind atemp1))
      (if not? (emit-bne lbl) (emit-beq lbl))
      (emit-label cont-lbl))))
(define (gen-even-test not? opnds lbl fs) (set-bbv-version-limit! #f) 
  (move-opnd-to-loc68 (Scar opnds) dtemp1 fs)
  (emit-and.w (make-imm 8) dtemp1)
  (shrink-frame fs)
  (if not? (emit-bne lbl) (emit-beq lbl)))
(define (def-spec name specializer-maker) (set-bbv-version-limit! #f) 
  (let ((proc-name (string->canonical-symbol name)))
    (let ((proc (prim-info proc-name)))
      (if proc
          (proc-obj-specialize-set! proc (specializer-maker proc proc-name))
          (compiler-internal-error "def-spec, unknown primitive:" name)))))
(define (safe name) (set-bbv-version-limit! #f) 
  (lambda (proc proc-name) (set-bbv-version-limit! #f) 
    (let ((spec (get-prim-info name))) (lambda (decls) (set-bbv-version-limit! #f)  spec))))
(define (unsafe name) (set-bbv-version-limit! #f) 
  (lambda (proc proc-name) (set-bbv-version-limit! #f) 
    (let ((spec (get-prim-info name)))
      (lambda (decls) (set-bbv-version-limit! #f)  (if (not (safe? decls)) spec proc)))))
(define (safe-arith fix-name flo-name) (set-bbv-version-limit! #f)  (arith #t fix-name flo-name))
(define (unsafe-arith fix-name flo-name) (set-bbv-version-limit! #f)  (arith #f fix-name flo-name))
(define (arith fix-safe? fix-name flo-name) (set-bbv-version-limit! #f) 
  (lambda (proc proc-name) (set-bbv-version-limit! #f) 
    (let ((fix-spec (if fix-name (get-prim-info fix-name) proc))
          (flo-spec (if flo-name (get-prim-info flo-name) proc)))
      (lambda (decls) (set-bbv-version-limit! #f) 
        (let ((arith (arith-implementation proc-name decls)))
          (cond ((eq? arith fixnum-sym)
                 (if (or fix-safe? (not (safe? decls))) fix-spec proc))
                ((eq? arith flonum-sym) (if (not (safe? decls)) flo-spec proc))
                (else proc)))))))
(define-apply "##TYPE" #f (lambda (opnds loc sn) (set-bbv-version-limit! #f)  (gen-type opnds loc sn)))
(define-apply
 "##TYPE-CAST"
 #f
 (lambda (opnds loc sn) (set-bbv-version-limit! #f)  (gen-type-cast opnds loc sn)))
(define-apply
 "##SUBTYPE"
 #f
 (lambda (opnds loc sn) (set-bbv-version-limit! #f)  (gen-subtype opnds loc sn)))
(define-apply
 "##SUBTYPE-SET!"
 #t
 (lambda (opnds loc sn) (set-bbv-version-limit! #f)  (gen-subtype-set! opnds loc sn)))
(define-ifjump
 "##NOT"
 (lambda (not? opnds lbl fs) (set-bbv-version-limit! #f)  (gen-eq-test bits-false not? opnds lbl fs)))
(define-ifjump
 "##NULL?"
 (lambda (not? opnds lbl fs) (set-bbv-version-limit! #f)  (gen-eq-test bits-null not? opnds lbl fs)))
(define-ifjump
 "##UNASSIGNED?"
 (lambda (not? opnds lbl fs) (set-bbv-version-limit! #f)  (gen-eq-test bits-unass not? opnds lbl fs)))
(define-ifjump
 "##UNBOUND?"
 (lambda (not? opnds lbl fs) (set-bbv-version-limit! #f)  (gen-eq-test bits-unbound not? opnds lbl fs)))
(define-ifjump
 "##EQ?"
 (lambda (not? opnds lbl fs) (set-bbv-version-limit! #f) 
   (gen-compares emit-beq emit-bne emit-beq emit-bne not? opnds lbl fs)))
(define-ifjump
 "##FIXNUM?"
 (lambda (not? opnds lbl fs) (set-bbv-version-limit! #f)  (gen-type-test type-fixnum not? opnds lbl fs)))
(define-ifjump
 "##FLONUM?"
 (lambda (not? opnds lbl fs) (set-bbv-version-limit! #f)  (gen-type-test type-flonum not? opnds lbl fs)))
(define-ifjump
 "##SPECIAL?"
 (lambda (not? opnds lbl fs) (set-bbv-version-limit! #f)  (gen-type-test type-special not? opnds lbl fs)))
(define-ifjump
 "##PAIR?"
 (lambda (not? opnds lbl fs) (set-bbv-version-limit! #f)  (gen-type-test type-pair not? opnds lbl fs)))
(define-ifjump
 "##SUBTYPED?"
 (lambda (not? opnds lbl fs) (set-bbv-version-limit! #f)  (gen-type-test type-subtyped not? opnds lbl fs)))
(define-ifjump
 "##PROCEDURE?"
 (lambda (not? opnds lbl fs) (set-bbv-version-limit! #f)  (gen-type-test type-procedure not? opnds lbl fs)))
(define-ifjump
 "##PLACEHOLDER?"
 (lambda (not? opnds lbl fs) (set-bbv-version-limit! #f) 
   (gen-type-test type-placeholder not? opnds lbl fs)))
(define-ifjump
 "##VECTOR?"
 (lambda (not? opnds lbl fs) (set-bbv-version-limit! #f) 
   (gen-subtype-test subtype-vector not? opnds lbl fs)))
(define-ifjump
 "##SYMBOL?"
 (lambda (not? opnds lbl fs) (set-bbv-version-limit! #f) 
   (gen-subtype-test subtype-symbol not? opnds lbl fs)))
(define-ifjump
 "##RATNUM?"
 (lambda (not? opnds lbl fs) (set-bbv-version-limit! #f) 
   (gen-subtype-test subtype-ratnum not? opnds lbl fs)))
(define-ifjump
 "##CPXNUM?"
 (lambda (not? opnds lbl fs) (set-bbv-version-limit! #f) 
   (gen-subtype-test subtype-cpxnum not? opnds lbl fs)))
(define-ifjump
 "##STRING?"
 (lambda (not? opnds lbl fs) (set-bbv-version-limit! #f) 
   (gen-subtype-test subtype-string not? opnds lbl fs)))
(define-ifjump
 "##BIGNUM?"
 (lambda (not? opnds lbl fs) (set-bbv-version-limit! #f) 
   (gen-subtype-test subtype-bignum not? opnds lbl fs)))
(define-ifjump
 "##CHAR?"
 (lambda (not? opnds lbl fs) (set-bbv-version-limit! #f) 
   (let ((opnd (Scar opnds)))
     (let ((o (opnd->opnd68 opnd #f fs)) (cont-lbl (new-lbl!)))
       (make-top-of-frame-if-stk-opnd68 o fs)
       (emit-move.l (opnd68->true-opnd68 o fs) dtemp1)
       (if not? (emit-bmi lbl) (emit-bmi cont-lbl))
       (emit-addq.w (SFXmodulo (SFX- type-pair type-special) 8) dtemp1)
       (emit-btst dtemp1 pair-reg)
       (shrink-frame fs)
       (if not? (emit-bne lbl) (emit-beq lbl))
       (emit-label cont-lbl)))))
(define-ifjump
 "##CLOSURE?"
 (lambda (not? opnds lbl fs) (set-bbv-version-limit! #f) 
   (move-opnd-to-loc68 (Scar opnds) atemp1 fs)
   (shrink-frame fs)
   (emit-cmp.w (make-imm 20153) (make-ind atemp1))
   (if not? (emit-bne lbl) (emit-beq lbl))))
(define-ifjump
 "##SUBPROCEDURE?"
 (lambda (not? opnds lbl fs) (set-bbv-version-limit! #f) 
   (move-opnd-to-loc68 (Scar opnds) atemp1 fs)
   (shrink-frame fs)
   (emit-move.w (make-pdec atemp1) dtemp1)
   (if not? (emit-bmi lbl) (emit-bpl lbl))))
(define-ifjump
 "##RETURN-DYNAMIC-ENV-BIND?"
 (lambda (not? opnds lbl fs) (set-bbv-version-limit! #f) 
   (move-opnd-to-loc68 (Scar opnds) atemp1 fs)
   (shrink-frame fs)
   (emit-move.w (make-disp* atemp1 -6) dtemp1)
   (if not? (emit-bne lbl) (emit-beq lbl))))
(define-apply
 "##FIXNUM.+"
 #f
 (lambda (opnds loc sn) (set-bbv-version-limit! #f) 
   (let ((sn-loc (sn-opnd loc sn)))
     (cond ((null? opnds) (copy-opnd-to-loc (make-obj '0) loc sn))
           ((null? (Scdr opnds)) (copy-opnd-to-loc (Scar opnds) loc sn))
           ((or (reg? loc) (stk? loc))
            (commut-oper gen-add opnds loc sn #f '() '()))
           (else (gen-add opnds '() loc sn #f))))))
(define-apply
 "##FIXNUM.-"
 #f
 (lambda (opnds loc sn) (set-bbv-version-limit! #f) 
   (let ((sn-loc (sn-opnd loc sn)))
     (gen-sub (Scar opnds)
              (Scdr opnds)
              loc
              sn
              (any-contains-opnd? loc (Scdr opnds))))))
(define-apply
 "##FIXNUM.*"
 #f
 (lambda (opnds loc sn) (set-bbv-version-limit! #f) 
   (let ((sn-loc (sn-opnd loc sn)))
     (cond ((null? opnds) (copy-opnd-to-loc (make-obj '1) loc sn))
           ((null? (Scdr opnds)) (copy-opnd-to-loc (Scar opnds) loc sn))
           ((and (reg? loc) (not (eq? loc return-reg)))
            (commut-oper gen-mul opnds loc sn #f '() '()))
           (else (gen-mul opnds '() loc sn #f))))))
(define-apply
 "##FIXNUM.QUOTIENT"
 #f
 (lambda (opnds loc sn) (set-bbv-version-limit! #f) 
   (let ((sn-loc (sn-opnd loc sn)))
     (gen-div (Scar opnds)
              (Scdr opnds)
              loc
              sn
              (any-contains-opnd? loc (Scdr opnds))))))
(define-apply
 "##FIXNUM.REMAINDER"
 #f
 (lambda (opnds loc sn) (set-bbv-version-limit! #f) 
   (let ((sn-loc (sn-opnd loc sn)))
     (gen-rem (Scar opnds) (Scadr opnds) loc sn))))
(define-apply
 "##FIXNUM.MODULO"
 #f
 (lambda (opnds loc sn) (set-bbv-version-limit! #f) 
   (let ((sn-loc (sn-opnd loc sn)))
     (gen-mod (Scar opnds) (Scadr opnds) loc sn))))
(define-apply
 "##FIXNUM.LOGIOR"
 #f
 (lambda (opnds loc sn) (set-bbv-version-limit! #f) 
   (let ((sn-loc (sn-opnd loc sn)))
     (cond ((null? opnds) (copy-opnd-to-loc (make-obj '0) loc sn))
           ((null? (Scdr opnds)) (copy-opnd-to-loc (Scar opnds) loc sn))
           ((or (reg? loc) (stk? loc))
            (commut-oper gen-logior opnds loc sn #f '() '()))
           (else (gen-logior opnds '() loc sn #f))))))
(define-apply
 "##FIXNUM.LOGXOR"
 #f
 (lambda (opnds loc sn) (set-bbv-version-limit! #f) 
   (let ((sn-loc (sn-opnd loc sn)))
     (cond ((null? opnds) (copy-opnd-to-loc (make-obj '0) loc sn))
           ((null? (Scdr opnds)) (copy-opnd-to-loc (Scar opnds) loc sn))
           ((or (reg? loc) (stk? loc))
            (commut-oper gen-logxor opnds loc sn #f '() '()))
           (else (gen-logxor opnds '() loc sn #f))))))
(define-apply
 "##FIXNUM.LOGAND"
 #f
 (lambda (opnds loc sn) (set-bbv-version-limit! #f) 
   (let ((sn-loc (sn-opnd loc sn)))
     (cond ((null? opnds) (copy-opnd-to-loc (make-obj '-1) loc sn))
           ((null? (Scdr opnds)) (copy-opnd-to-loc (Scar opnds) loc sn))
           ((or (reg? loc) (stk? loc))
            (commut-oper gen-logand opnds loc sn #f '() '()))
           (else (gen-logand opnds '() loc sn #f))))))
(define-apply
 "##FIXNUM.LOGNOT"
 #f
 (lambda (opnds loc sn) (set-bbv-version-limit! #f) 
   (let ((sn-loc (sn-opnd loc sn)) (opnd (Scar opnds)))
     (if (and (or (reg? loc) (stk? loc)) (not (eq? loc return-reg)))
         (begin
           (copy-opnd-to-loc opnd loc sn-loc)
           (let ((loc68 (loc->loc68 loc #f sn)))
             (make-top-of-frame-if-stk-opnd68 loc68 sn)
             (emit-not.l (opnd68->true-opnd68 loc68 sn))
             (emit-and.w (make-imm -8) (opnd68->true-opnd68 loc68 sn))))
         (begin
           (move-opnd-to-loc68 opnd dtemp1 (sn-opnd loc sn))
           (emit-not.l dtemp1)
           (emit-and.w (make-imm -8) dtemp1)
           (move-opnd68-to-loc dtemp1 loc sn))))))
(define-apply "##FIXNUM.ASH" #f (gen-shift emit-asr.l))
(define-apply "##FIXNUM.LSH" #f (gen-shift emit-lsr.l))
(define-ifjump
 "##FIXNUM.ZERO?"
 (lambda (not? opnds lbl fs) (set-bbv-version-limit! #f)  (gen-eq-test 0 not? opnds lbl fs)))
(define-ifjump
 "##FIXNUM.POSITIVE?"
 (lambda (not? opnds lbl fs) (set-bbv-version-limit! #f) 
   (gen-compares
    emit-bgt
    emit-ble
    emit-blt
    emit-bge
    not?
    (list (Scar opnds) (make-obj '0))
    lbl
    fs)))
(define-ifjump
 "##FIXNUM.NEGATIVE?"
 (lambda (not? opnds lbl fs) (set-bbv-version-limit! #f) 
   (gen-compares
    emit-blt
    emit-bge
    emit-bgt
    emit-ble
    not?
    (list (Scar opnds) (make-obj '0))
    lbl
    fs)))
(define-ifjump
 "##FIXNUM.ODD?"
 (lambda (not? opnds lbl fs) (set-bbv-version-limit! #f)  (gen-even-test (not not?) opnds lbl fs)))
(define-ifjump
 "##FIXNUM.EVEN?"
 (lambda (not? opnds lbl fs) (set-bbv-version-limit! #f)  (gen-even-test not? opnds lbl fs)))
(define-ifjump
 "##FIXNUM.="
 (lambda (not? opnds lbl fs) (set-bbv-version-limit! #f) 
   (gen-compares emit-beq emit-bne emit-beq emit-bne not? opnds lbl fs)))
(define-ifjump
 "##FIXNUM.<"
 (lambda (not? opnds lbl fs) (set-bbv-version-limit! #f) 
   (gen-compares emit-blt emit-bge emit-bgt emit-ble not? opnds lbl fs)))
(define-ifjump
 "##FIXNUM.>"
 (lambda (not? opnds lbl fs) (set-bbv-version-limit! #f) 
   (gen-compares emit-bgt emit-ble emit-blt emit-bge not? opnds lbl fs)))
(define-ifjump
 "##FIXNUM.<="
 (lambda (not? opnds lbl fs) (set-bbv-version-limit! #f) 
   (gen-compares emit-ble emit-bgt emit-bge emit-blt not? opnds lbl fs)))
(define-ifjump
 "##FIXNUM.>="
 (lambda (not? opnds lbl fs) (set-bbv-version-limit! #f) 
   (gen-compares emit-bge emit-blt emit-ble emit-bgt not? opnds lbl fs)))
(define-apply
 "##FLONUM.->FIXNUM"
 #f
 (lambda (opnds loc sn) (set-bbv-version-limit! #f) 
   (let ((sn-loc (sn-opnd loc sn)))
     (move-opnd-to-loc68 (Scar opnds) atemp1 sn-loc)
     (let ((reg68 (if (and (reg? loc) (not (eq? loc return-reg)))
                      (reg->reg68 loc)
                      dtemp1)))
       (emit-fmov.dx (make-disp* atemp1 (SFX- type-flonum)) ftemp1)
       (emit-fmov.l ftemp1 reg68)
       (emit-asl.l (make-imm 3) reg68)
       (if (not (and (reg? loc) (not (eq? loc return-reg))))
           (move-opnd68-to-loc reg68 loc sn))))))
(define-apply
 "##FLONUM.<-FIXNUM"
 #f
 (lambda (opnds loc sn) (set-bbv-version-limit! #f) 
   (gen-guarantee-space 2)
   (move-opnd-to-loc68
    (Scar opnds)
    dtemp1
    (sn-opnds (Scdr opnds) (sn-opnd loc sn)))
   (emit-asr.l (make-imm 3) dtemp1)
   (emit-fmov.l dtemp1 ftemp1)
   (add-n-to-loc68 (SFX* -2 pointer-size) heap-reg)
   (emit-fmov.dx ftemp1 (make-ind heap-reg))
   (let ((reg68 (if (reg? loc) (reg->reg68 loc) atemp1)))
     (emit-move.l heap-reg reg68)
     (emit-addq.l type-flonum reg68))
   (if (not (reg? loc)) (move-opnd68-to-loc atemp1 loc sn))))
(define-apply
 "##FLONUM.+"
 #f
 (lambda (opnds loc sn) (set-bbv-version-limit! #f) 
   (let ((sn-loc (sn-opnd loc sn)))
     (cond ((null? opnds) (copy-opnd-to-loc (make-obj inexact-0) loc sn))
           ((null? (Scdr opnds)) (copy-opnd-to-loc (Scar opnds) loc sn))
           (else (flo-oper emit-fmov.dx emit-fadd.dx opnds loc sn))))))
(define-apply
 "##FLONUM.*"
 #f
 (lambda (opnds loc sn) (set-bbv-version-limit! #f) 
   (let ((sn-loc (sn-opnd loc sn)))
     (cond ((null? opnds) (copy-opnd-to-loc (make-obj inexact-+1) loc sn))
           ((null? (Scdr opnds)) (copy-opnd-to-loc (Scar opnds) loc sn))
           (else (flo-oper emit-fmov.dx emit-fmul.dx opnds loc sn))))))
(define-apply
 "##FLONUM.-"
 #f
 (lambda (opnds loc sn) (set-bbv-version-limit! #f) 
   (let ((sn-loc (sn-opnd loc sn)))
     (if (null? (Scdr opnds))
         (flo-oper emit-fneg.dx #f opnds loc sn)
         (flo-oper emit-fmov.dx emit-fsub.dx opnds loc sn)))))
(define-apply
 "##FLONUM./"
 #f
 (lambda (opnds loc sn) (set-bbv-version-limit! #f) 
   (let ((sn-loc (sn-opnd loc sn)))
     (if (null? (Scdr opnds))
         (flo-oper
          emit-fmov.dx
          emit-fdiv.dx
          (cons (make-obj inexact-+1) opnds)
          loc
          sn)
         (flo-oper emit-fmov.dx emit-fdiv.dx opnds loc sn)))))
(define-apply
 "##FLONUM.ABS"
 #f
 (lambda (opnds loc sn) (set-bbv-version-limit! #f) 
   (let ((sn-loc (sn-opnd loc sn))) (flo-oper emit-fabs.dx #f opnds loc sn))))
(define-apply
 "##FLONUM.TRUNCATE"
 #f
 (lambda (opnds loc sn) (set-bbv-version-limit! #f) 
   (let ((sn-loc (sn-opnd loc sn)))
     (flo-oper emit-fintrz.dx #f opnds loc sn))))
(define-apply
 "##FLONUM.ROUND"
 #f
 (lambda (opnds loc sn) (set-bbv-version-limit! #f) 
   (let ((sn-loc (sn-opnd loc sn))) (flo-oper emit-fint.dx #f opnds loc sn))))
(define-apply
 "##FLONUM.EXP"
 #f
 (lambda (opnds loc sn) (set-bbv-version-limit! #f) 
   (let ((sn-loc (sn-opnd loc sn))) (flo-oper emit-fetox.dx #f opnds loc sn))))
(define-apply
 "##FLONUM.LOG"
 #f
 (lambda (opnds loc sn) (set-bbv-version-limit! #f) 
   (let ((sn-loc (sn-opnd loc sn))) (flo-oper emit-flogn.dx #f opnds loc sn))))
(define-apply
 "##FLONUM.SIN"
 #f
 (lambda (opnds loc sn) (set-bbv-version-limit! #f) 
   (let ((sn-loc (sn-opnd loc sn))) (flo-oper emit-fsin.dx #f opnds loc sn))))
(define-apply
 "##FLONUM.COS"
 #f
 (lambda (opnds loc sn) (set-bbv-version-limit! #f) 
   (let ((sn-loc (sn-opnd loc sn))) (flo-oper emit-fcos.dx #f opnds loc sn))))
(define-apply
 "##FLONUM.TAN"
 #f
 (lambda (opnds loc sn) (set-bbv-version-limit! #f) 
   (let ((sn-loc (sn-opnd loc sn))) (flo-oper emit-ftan.dx #f opnds loc sn))))
(define-apply
 "##FLONUM.ASIN"
 #f
 (lambda (opnds loc sn) (set-bbv-version-limit! #f) 
   (let ((sn-loc (sn-opnd loc sn))) (flo-oper emit-fasin.dx #f opnds loc sn))))
(define-apply
 "##FLONUM.ACOS"
 #f
 (lambda (opnds loc sn) (set-bbv-version-limit! #f) 
   (let ((sn-loc (sn-opnd loc sn))) (flo-oper emit-facos.dx #f opnds loc sn))))
(define-apply
 "##FLONUM.ATAN"
 #f
 (lambda (opnds loc sn) (set-bbv-version-limit! #f) 
   (let ((sn-loc (sn-opnd loc sn))) (flo-oper emit-fatan.dx #f opnds loc sn))))
(define-apply
 "##FLONUM.SQRT"
 #f
 (lambda (opnds loc sn) (set-bbv-version-limit! #f) 
   (let ((sn-loc (sn-opnd loc sn))) (flo-oper emit-fsqrt.dx #f opnds loc sn))))
(define-ifjump
 "##FLONUM.ZERO?"
 (lambda (not? opnds lbl fs) (set-bbv-version-limit! #f) 
   (gen-compares-flo
    emit-fbeq
    emit-fbne
    emit-fbeq
    emit-fbne
    not?
    (list (Scar opnds) (make-obj inexact-0))
    lbl
    fs)))
(define-ifjump
 "##FLONUM.NEGATIVE?"
 (lambda (not? opnds lbl fs) (set-bbv-version-limit! #f) 
   (gen-compares-flo
    emit-fblt
    emit-fbge
    emit-fbgt
    emit-fble
    not?
    (list (Scar opnds) (make-obj inexact-0))
    lbl
    fs)))
(define-ifjump
 "##FLONUM.POSITIVE?"
 (lambda (not? opnds lbl fs) (set-bbv-version-limit! #f) 
   (gen-compares-flo
    emit-fbgt
    emit-fble
    emit-fblt
    emit-fbge
    not?
    (list (Scar opnds) (make-obj inexact-0))
    lbl
    fs)))
(define-ifjump
 "##FLONUM.="
 (lambda (not? opnds lbl fs) (set-bbv-version-limit! #f) 
   (gen-compares-flo
    emit-fbeq
    emit-fbne
    emit-fbeq
    emit-fbne
    not?
    opnds
    lbl
    fs)))
(define-ifjump
 "##FLONUM.<"
 (lambda (not? opnds lbl fs) (set-bbv-version-limit! #f) 
   (gen-compares-flo
    emit-fblt
    emit-fbge
    emit-fbgt
    emit-fble
    not?
    opnds
    lbl
    fs)))
(define-ifjump
 "##FLONUM.>"
 (lambda (not? opnds lbl fs) (set-bbv-version-limit! #f) 
   (gen-compares-flo
    emit-fbgt
    emit-fble
    emit-fblt
    emit-fbge
    not?
    opnds
    lbl
    fs)))
(define-ifjump
 "##FLONUM.<="
 (lambda (not? opnds lbl fs) (set-bbv-version-limit! #f) 
   (gen-compares-flo
    emit-fble
    emit-fbgt
    emit-fbge
    emit-fblt
    not?
    opnds
    lbl
    fs)))
(define-ifjump
 "##FLONUM.>="
 (lambda (not? opnds lbl fs) (set-bbv-version-limit! #f) 
   (gen-compares-flo
    emit-fbge
    emit-fblt
    emit-fble
    emit-fbgt
    not?
    opnds
    lbl
    fs)))
(define-ifjump
 "##CHAR=?"
 (lambda (not? opnds lbl fs) (set-bbv-version-limit! #f) 
   (gen-compares emit-beq emit-bne emit-beq emit-bne not? opnds lbl fs)))
(define-ifjump
 "##CHAR<?"
 (lambda (not? opnds lbl fs) (set-bbv-version-limit! #f) 
   (gen-compares emit-blt emit-bge emit-bgt emit-ble not? opnds lbl fs)))
(define-ifjump
 "##CHAR>?"
 (lambda (not? opnds lbl fs) (set-bbv-version-limit! #f) 
   (gen-compares emit-bgt emit-ble emit-blt emit-bge not? opnds lbl fs)))
(define-ifjump
 "##CHAR<=?"
 (lambda (not? opnds lbl fs) (set-bbv-version-limit! #f) 
   (gen-compares emit-ble emit-bgt emit-bge emit-blt not? opnds lbl fs)))
(define-ifjump
 "##CHAR>=?"
 (lambda (not? opnds lbl fs) (set-bbv-version-limit! #f) 
   (gen-compares emit-bge emit-blt emit-ble emit-bgt not? opnds lbl fs)))
(define-apply "##CONS" #f (lambda (opnds loc sn) (set-bbv-version-limit! #f)  (gen-cons opnds loc sn)))
(define-apply
 "##SET-CAR!"
 #t
 (lambda (opnds loc sn) (set-bbv-version-limit! #f)  (gen-set-car! opnds loc sn)))
(define-apply
 "##SET-CDR!"
 #t
 (lambda (opnds loc sn) (set-bbv-version-limit! #f)  (gen-set-cdr! opnds loc sn)))
(define-apply "##CAR" #f (make-gen-apply-c...r 2))
(define-apply "##CDR" #f (make-gen-apply-c...r 3))
(define-apply "##CAAR" #f (make-gen-apply-c...r 4))
(define-apply "##CADR" #f (make-gen-apply-c...r 5))
(define-apply "##CDAR" #f (make-gen-apply-c...r 6))
(define-apply "##CDDR" #f (make-gen-apply-c...r 7))
(define-apply "##CAAAR" #f (make-gen-apply-c...r 8))
(define-apply "##CAADR" #f (make-gen-apply-c...r 9))
(define-apply "##CADAR" #f (make-gen-apply-c...r 10))
(define-apply "##CADDR" #f (make-gen-apply-c...r 11))
(define-apply "##CDAAR" #f (make-gen-apply-c...r 12))
(define-apply "##CDADR" #f (make-gen-apply-c...r 13))
(define-apply "##CDDAR" #f (make-gen-apply-c...r 14))
(define-apply "##CDDDR" #f (make-gen-apply-c...r 15))
(define-apply "##CAAAAR" #f (make-gen-apply-c...r 16))
(define-apply "##CAAADR" #f (make-gen-apply-c...r 17))
(define-apply "##CAADAR" #f (make-gen-apply-c...r 18))
(define-apply "##CAADDR" #f (make-gen-apply-c...r 19))
(define-apply "##CADAAR" #f (make-gen-apply-c...r 20))
(define-apply "##CADADR" #f (make-gen-apply-c...r 21))
(define-apply "##CADDAR" #f (make-gen-apply-c...r 22))
(define-apply "##CADDDR" #f (make-gen-apply-c...r 23))
(define-apply "##CDAAAR" #f (make-gen-apply-c...r 24))
(define-apply "##CDAADR" #f (make-gen-apply-c...r 25))
(define-apply "##CDADAR" #f (make-gen-apply-c...r 26))
(define-apply "##CDADDR" #f (make-gen-apply-c...r 27))
(define-apply "##CDDAAR" #f (make-gen-apply-c...r 28))
(define-apply "##CDDADR" #f (make-gen-apply-c...r 29))
(define-apply "##CDDDAR" #f (make-gen-apply-c...r 30))
(define-apply "##CDDDDR" #f (make-gen-apply-c...r 31))
(define-apply
 "##MAKE-CELL"
 #f
 (lambda (opnds loc sn) (set-bbv-version-limit! #f)  (gen-cons (list (Scar opnds) (make-obj '())) loc sn)))
(define-apply "##CELL-REF" #f (make-gen-apply-c...r 2))
(define-apply
 "##CELL-SET!"
 #t
 (lambda (opnds loc sn) (set-bbv-version-limit! #f)  (gen-set-car! opnds loc sn)))
(define-apply "##VECTOR" #f (make-gen-vector 'vector))
(define-apply "##VECTOR-LENGTH" #f (make-gen-vector-length 'vector))
(define-apply "##VECTOR-REF" #f (make-gen-vector-ref 'vector))
(define-apply "##VECTOR-SET!" #t (make-gen-vector-set! 'vector))
(define-apply "##VECTOR-SHRINK!" #t (make-gen-vector-shrink! 'vector))
(define-apply "##STRING" #f (make-gen-vector 'string))
(define-apply "##STRING-LENGTH" #f (make-gen-vector-length 'string))
(define-apply "##STRING-REF" #f (make-gen-vector-ref 'string))
(define-apply "##STRING-SET!" #t (make-gen-vector-set! 'string))
(define-apply "##STRING-SHRINK!" #t (make-gen-vector-shrink! 'string))
(define-apply "##VECTOR8" #f (make-gen-vector 'vector8))
(define-apply "##VECTOR8-LENGTH" #f (make-gen-vector-length 'vector8))
(define-apply "##VECTOR8-REF" #f (make-gen-vector-ref 'vector8))
(define-apply "##VECTOR8-SET!" #t (make-gen-vector-set! 'vector8))
(define-apply "##VECTOR8-SHRINK!" #t (make-gen-vector-shrink! 'vector8))
(define-apply "##VECTOR16" #f (make-gen-vector 'vector16))
(define-apply "##VECTOR16-LENGTH" #f (make-gen-vector-length 'vector16))
(define-apply "##VECTOR16-REF" #f (make-gen-vector-ref 'vector16))
(define-apply "##VECTOR16-SET!" #t (make-gen-vector-set! 'vector16))
(define-apply "##VECTOR16-SHRINK!" #t (make-gen-vector-shrink! 'vector16))
(define-apply "##CLOSURE-CODE" #f (make-gen-slot-ref 1 type-procedure))
(define-apply "##CLOSURE-REF" #f (make-gen-vector-ref 'closure))
(define-apply "##CLOSURE-SET!" #t (make-gen-vector-set! 'closure))
(define-apply
 "##SUBPROCEDURE-ID"
 #f
 (lambda (opnds loc sn) (set-bbv-version-limit! #f)  (gen-subprocedure-id opnds loc sn)))
(define-apply
 "##SUBPROCEDURE-PARENT"
 #f
 (lambda (opnds loc sn) (set-bbv-version-limit! #f)  (gen-subprocedure-parent opnds loc sn)))
(define-apply
 "##RETURN-FS"
 #f
 (lambda (opnds loc sn) (set-bbv-version-limit! #f)  (gen-return-fs opnds loc sn)))
(define-apply
 "##RETURN-LINK"
 #f
 (lambda (opnds loc sn) (set-bbv-version-limit! #f)  (gen-return-link opnds loc sn)))
(define-apply
 "##PROCEDURE-INFO"
 #f
 (lambda (opnds loc sn) (set-bbv-version-limit! #f)  (gen-procedure-info opnds loc sn)))
(define-apply
 "##PSTATE"
 #f
 (lambda (opnds loc sn) (set-bbv-version-limit! #f)  (move-opnd68-to-loc pstate-reg loc sn)))
(define-apply
 "##MAKE-PLACEHOLDER"
 #f
 (lambda (opnds loc sn) (set-bbv-version-limit! #f)  (gen-make-placeholder opnds loc sn)))
(define-apply
 "##TOUCH"
 #t
 (lambda (opnds loc sn) (set-bbv-version-limit! #f) 
   (let ((opnd (Scar opnds)))
     (if loc
         (touch-opnd-to-loc opnd loc sn)
         (touch-opnd-to-any-reg68 opnd sn)))))
(def-spec "NOT" (safe "##NOT"))
(def-spec "NULL?" (safe "##NULL?"))
(def-spec "EQ?" (safe "##EQ?"))
(def-spec "PAIR?" (safe "##PAIR?"))
(def-spec "PROCEDURE?" (safe "##PROCEDURE?"))
(def-spec "VECTOR?" (safe "##VECTOR?"))
(def-spec "SYMBOL?" (safe "##SYMBOL?"))
(def-spec "STRING?" (safe "##STRING?"))
(def-spec "CHAR?" (safe "##CHAR?"))
(def-spec "ZERO?" (safe-arith "##FIXNUM.ZERO?" "##FLONUM.ZERO?"))
(def-spec "POSITIVE?" (safe-arith "##FIXNUM.POSITIVE?" "##FLONUM.POSITIVE?"))
(def-spec "NEGATIVE?" (safe-arith "##FIXNUM.NEGATIVE?" "##FLONUM.NEGATIVE?"))
(def-spec "ODD?" (safe-arith "##FIXNUM.ODD?" #f))
(def-spec "EVEN?" (safe-arith "##FIXNUM.EVEN?" #f))
(def-spec "+" (unsafe-arith "##FIXNUM.+" "##FLONUM.+"))
(def-spec "*" (unsafe-arith "##FIXNUM.*" "##FLONUM.*"))
(def-spec "-" (unsafe-arith "##FIXNUM.-" "##FLONUM.-"))
(def-spec "/" (unsafe-arith #f "##FLONUM./"))
(def-spec "QUOTIENT" (unsafe-arith "##FIXNUM.QUOTIENT" #f))
(def-spec "REMAINDER" (unsafe-arith "##FIXNUM.REMAINDER" #f))
(def-spec "MODULO" (unsafe-arith "##FIXNUM.MODULO" #f))
(def-spec "=" (safe-arith "##FIXNUM.=" "##FLONUM.="))
(def-spec "<" (safe-arith "##FIXNUM.<" "##FLONUM.<"))
(def-spec ">" (safe-arith "##FIXNUM.>" "##FLONUM.>"))
(def-spec "<=" (safe-arith "##FIXNUM.<=" "##FLONUM.<="))
(def-spec ">=" (safe-arith "##FIXNUM.>=" "##FLONUM.>="))
(def-spec "ABS" (unsafe-arith #f "##FLONUM.ABS"))
(def-spec "TRUNCATE" (unsafe-arith #f "##FLONUM.TRUNCATE"))
(def-spec "EXP" (unsafe-arith #f "##FLONUM.EXP"))
(def-spec "LOG" (unsafe-arith #f "##FLONUM.LOG"))
(def-spec "SIN" (unsafe-arith #f "##FLONUM.SIN"))
(def-spec "COS" (unsafe-arith #f "##FLONUM.COS"))
(def-spec "TAN" (unsafe-arith #f "##FLONUM.TAN"))
(def-spec "ASIN" (unsafe-arith #f "##FLONUM.ASIN"))
(def-spec "ACOS" (unsafe-arith #f "##FLONUM.ACOS"))
(def-spec "ATAN" (unsafe-arith #f "##FLONUM.ATAN"))
(def-spec "SQRT" (unsafe-arith #f "##FLONUM.SQRT"))
(def-spec "CHAR=?" (safe "##CHAR=?"))
(def-spec "CHAR<?" (safe "##CHAR<?"))
(def-spec "CHAR>?" (safe "##CHAR>?"))
(def-spec "CHAR<=?" (safe "##CHAR<=?"))
(def-spec "CHAR>=?" (safe "##CHAR>=?"))
(def-spec "CONS" (safe "##CONS"))
(def-spec "SET-CAR!" (unsafe "##SET-CAR!"))
(def-spec "SET-CDR!" (unsafe "##SET-CDR!"))
(def-spec "CAR" (unsafe "##CAR"))
(def-spec "CDR" (unsafe "##CDR"))
(def-spec "CAAR" (unsafe "##CAAR"))
(def-spec "CADR" (unsafe "##CADR"))
(def-spec "CDAR" (unsafe "##CDAR"))
(def-spec "CDDR" (unsafe "##CDDR"))
(def-spec "CAAAR" (unsafe "##CAAAR"))
(def-spec "CAADR" (unsafe "##CAADR"))
(def-spec "CADAR" (unsafe "##CADAR"))
(def-spec "CADDR" (unsafe "##CADDR"))
(def-spec "CDAAR" (unsafe "##CDAAR"))
(def-spec "CDADR" (unsafe "##CDADR"))
(def-spec "CDDAR" (unsafe "##CDDAR"))
(def-spec "CDDDR" (unsafe "##CDDDR"))
(def-spec "CAAAAR" (unsafe "##CAAAAR"))
(def-spec "CAAADR" (unsafe "##CAAADR"))
(def-spec "CAADAR" (unsafe "##CAADAR"))
(def-spec "CAADDR" (unsafe "##CAADDR"))
(def-spec "CADAAR" (unsafe "##CADAAR"))
(def-spec "CADADR" (unsafe "##CADADR"))
(def-spec "CADDAR" (unsafe "##CADDAR"))
(def-spec "CADDDR" (unsafe "##CADDDR"))
(def-spec "CDAAAR" (unsafe "##CDAAAR"))
(def-spec "CDAADR" (unsafe "##CDAADR"))
(def-spec "CDADAR" (unsafe "##CDADAR"))
(def-spec "CDADDR" (unsafe "##CDADDR"))
(def-spec "CDDAAR" (unsafe "##CDDAAR"))
(def-spec "CDDADR" (unsafe "##CDDADR"))
(def-spec "CDDDAR" (unsafe "##CDDDAR"))
(def-spec "CDDDDR" (unsafe "##CDDDDR"))
(def-spec "VECTOR" (safe "##VECTOR"))
(def-spec "VECTOR-LENGTH" (unsafe "##VECTOR-LENGTH"))
(def-spec "VECTOR-REF" (unsafe "##VECTOR-REF"))
(def-spec "VECTOR-SET!" (unsafe "##VECTOR-SET!"))
(def-spec "STRING" (safe "##STRING"))
(def-spec "STRING-LENGTH" (unsafe "##STRING-LENGTH"))
(def-spec "STRING-REF" (unsafe "##STRING-REF"))
(def-spec "STRING-SET!" (unsafe "##STRING-SET!"))
(def-spec "TOUCH" (safe "##TOUCH"))
(let ((targ (make-target 4 'm68000)))
  (target-begin!-set! targ (lambda (info-port) (set-bbv-version-limit! #f)  (begin! info-port targ)))
  (put-target targ))

(define input-source-code '
(begin
(declare (standard-bindings) (fixnum) (not safe) (block))

(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1))
         (fib (- n 2)))))

(define (tak x y z)
  (if (not (< y x))
      z
      (tak (tak (- x 1) y z)
           (tak (- y 1) z x)
           (tak (- z 1) x y))))

(define (ack m n)
  (cond ((= m 0) (+ n 1))
        ((= n 0) (ack (- m 1) 1))
        (else (ack (- m 1) (ack m (- n 1))))))

(define (create-x n)
  (define result (make-vector n))
  (do ((i 0 (+ i 1)))
      ((>= i n) result)
    (vector-set! result i i)))

(define (create-y x)
  (let* ((n (vector-length x))
         (result (make-vector n)))
    (do ((i (- n 1) (- i 1)))
        ((< i 0) result)
      (vector-set! result i (vector-ref x i)))))

(define (my-try n)
  (vector-length (create-y (create-x n))))

(define (go n)
  (let loop ((repeat 100)
             (result 0))
    (if (> repeat 0)
        (loop (- repeat 1) (my-try n))
        result)))

(+ (fib 20)
   (tak 18 12 6)
   (ack 3 9)
   (go 200000))
))

(define output-expected '(
"|------------------------------------------------------"
"| #[primitive #!program] ="
"L1:"
" cmpw #1,d0"
" beq L1000"
" TRAP1(9,0)"
" LBL_PTR(L1)"
"L1000:"
" MOVE_PROC(1,a1)"
" movl a1,GLOB(fib)"
" MOVE_PROC(2,a1)"
" movl a1,GLOB(tak)"
" MOVE_PROC(3,a1)"
" movl a1,GLOB(ack)"
" MOVE_PROC(4,a1)"
" movl a1,GLOB(create-x)"
" MOVE_PROC(5,a1)"
" movl a1,GLOB(create-y)"
" MOVE_PROC(6,a1)"
" movl a1,GLOB(my-try)"
" MOVE_PROC(7,a1)"
" movl a1,GLOB(go)"
" movl a0,sp@-"
" movl #160,d1"
" lea L2,a0"
" dbra d5,L1001"
" moveq #9,d5"
" cmpl a5@,sp"
" bcc L1001"
" TRAP2(24)"
" RETURN(L1,1,1)"
"L1002:"
"L1001:"
" JMP_PROC(1,10)"
" RETURN(L1,1,1)"
"L2:"
" movl d1,sp@-"
" moveq #48,d3"
" moveq #96,d2"
" movl #144,d1"
" lea L3,a0"
" JMP_PROC(2,14)"
" RETURN(L1,2,1)"
"L3:"
" movl d1,sp@-"
" moveq #72,d2"
" moveq #24,d1"
" lea L4,a0"
" JMP_PROC(3,10)"
" RETURN(L1,3,1)"
"L4:"
" movl d1,sp@-"
" movl #1600000,d1"
" lea L5,a0"
" JMP_PROC(7,10)"
" RETURN(L1,4,1)"
"L5:"
" dbra d5,L1003"
" moveq #9,d5"
" cmpl a5@,sp"
" bcc L1003"
" TRAP2(24)"
" RETURN(L1,4,1)"
"L1004:"
"L1003:"
"L6:"
" addl sp@(8),d1"
" addl sp@(4),d1"
" addl sp@+,d1"
" addql #8,sp"
" rts"
"L0:"
"|------------------------------------------------------"
"| #[primitive fib] ="
"L1:"
" bmi L1000"
" TRAP1(9,1)"
" LBL_PTR(L1)"
"L1000:"
" moveq #16,d0"
" cmpl d1,d0"
" ble L3"
" bra L4"
" RETURN(L1,2,1)"
"L2:"
" movl d1,sp@-"
" movl sp@(4),d1"
" moveq #-16,d0"
" addl d0,d1"
" lea L5,a0"
" moveq #16,d0"
" cmpl d1,d0"
" bgt L4"
"L3:"
" movl a0,sp@-"
" movl d1,sp@-"
" subql #8,d1"
" lea L2,a0"
" dbra d5,L1001"
" moveq #9,d5"
" cmpl a5@,sp"
" bcc L1001"
" TRAP2(24)"
" RETURN(L1,2,1)"
"L1002:"
"L1001:"
" moveq #16,d0"
" cmpl d1,d0"
" ble L3"
"L4:"
" jmp a0@"
" RETURN(L1,3,1)"
"L5:"
" addl sp@+,d1"
" dbra d5,L1003"
" moveq #9,d5"
" cmpl a5@,sp"
" bcc L1003"
" TRAP2(24)"
" RETURN(L1,2,1)"
"L1004:"
"L1003:"
" addql #4,sp"
" rts"
"L0:"
"|------------------------------------------------------"
"| #[primitive tak] ="
"L1:"
" cmpw #4,d0"
" beq L1000"
" TRAP1(9,3)"
" LBL_PTR(L1)"
"L1000:"
" cmpl d1,d2"
" bge L4"
" bra L3"
" RETURN(L1,6,1)"
"L2:"
" movl d1,d3"
" movl sp@(20),a0"
" movl sp@+,d2"
" movl sp@+,d1"
" dbra d5,L1001"
" moveq #9,d5"
" cmpl a5@,sp"
" bcc L1001"
" movl a0,sp@(12)"
" TRAP2(24)"
" RETURN(L1,4,1)"
"L1002:"
" movl sp@(12),a0"
"L1001:"
" cmpl d1,d2"
" lea sp@(16),sp"
" bge L4"
"L3:"
" movl a0,sp@-"
" movl d1,sp@-"
" movl d2,sp@-"
" movl d3,sp@-"
" subql #8,d1"
" lea L5,a0"
" dbra d5,L1003"
" moveq #9,d5"
" cmpl a5@,sp"
" bcc L1003"
" TRAP2(24)"
" RETURN(L1,4,1)"
"L1004:"
"L1003:"
" cmpl d1,d2"
" blt L3"
"L4:"
" movl d3,d1"
" jmp a0@"
" RETURN(L1,4,1)"
"L5:"
" movl d1,sp@-"
" movl sp@(12),d3"
" movl sp@(4),d2"
" movl sp@(8),d1"
" subql #8,d1"
" lea L6,a0"
" cmpl d1,d2"
" bge L4"
" bra L3"
" RETURN(L1,5,1)"
"L6:"
" movl d1,sp@-"
" movl sp@(12),d3"
" movl sp@(16),d2"
" movl sp@(8),d1"
" subql #8,d1"
" lea L2,a0"
" cmpl d1,d2"
" bge L4"
" bra L3"
"L0:"
"|------------------------------------------------------"
"| #[primitive ack] ="
"L1:"
" beq L1000"
" TRAP1(9,2)"
" LBL_PTR(L1)"
"L1000:"
" movl d1,d0"
" bne L3"
" bra L5"
" RETURN(L1,2,1)"
"L2:"
" movl d1,d2"
" movl sp@+,d1"
" subql #8,d1"
" movl sp@+,a0"
" dbra d5,L1001"
" moveq #9,d5"
" cmpl a5@,sp"
" bcc L1001"
" movl a0,sp@-"
" TRAP2(24)"
" RETURN(L1,1,1)"
"L1002:"
" movl sp@+,a0"
"L1001:"
" movl d1,d0"
" beq L5"
"L3:"
" movl d2,d0"
" bne L6"
"L4:"
" subql #8,d1"
" moveq #8,d2"
" dbra d5,L1003"
" moveq #9,d5"
" cmpl a5@,sp"
" bcc L1003"
" movl a0,sp@-"
" TRAP2(24)"
" RETURN(L1,1,1)"
"L1004:"
" movl sp@+,a0"
"L1003:"
" movl d1,d0"
" bne L3"
"L5:"
" movl d2,d1"
" addql #8,d1"
" jmp a0@"
"L6:"
" movl a0,sp@-"
" movl d1,sp@-"
" movl d2,d1"
" subql #8,d1"
" movl d1,d2"
" movl sp@,d1"
" lea L2,a0"
" dbra d5,L1005"
" moveq #9,d5"
" cmpl a5@,sp"
" bcc L1005"
" TRAP2(24)"
" RETURN(L1,2,1)"
"L1006:"
"L1005:"
" movl d1,d0"
" bne L3"
" bra L5"
"L0:"
"|------------------------------------------------------"
"| #[primitive create-x] ="
"L1:"
" bmi L1000"
" TRAP1(9,1)"
" LBL_PTR(L1)"
"L1000:"
" movl a0,sp@-"
" movl d1,sp@-"
" lea L2,a0"
" dbra d5,L1001"
" moveq #9,d5"
" cmpl a5@,sp"
" bcc L1001"
" TRAP2(24)"
" RETURN(L1,2,1)"
"L1002:"
"L1001:"
" moveq #-1,d0"
" JMP_PRIM(make-vector,0)"
" RETURN(L1,2,1)"
"L2:"
" movl d1,d2"
" movl sp@+,d1"
" moveq #0,d3"
" movl sp@+,a0"
" dbra d5,L1003"
" moveq #9,d5"
" cmpl a5@,sp"
" bcc L1003"
" movl a0,sp@-"
" TRAP2(24)"
" RETURN(L1,1,1)"
"L1004:"
" movl sp@+,a0"
"L1003:"
" cmpl d1,d3"
" bge L4"
"L3:"
" movl d3,d0"
" asrl #1,d0"
" movl d2,a1"
" movl d3,a1@(1,d0:l)"
" addql #8,d3"
" dbra d5,L1005"
" moveq #9,d5"
" cmpl a5@,sp"
" bcc L1005"
" movl a0,sp@-"
" TRAP2(24)"
" RETURN(L1,1,1)"
"L1006:"
" movl sp@+,a0"
"L1005:"
" cmpl d1,d3"
" blt L3"
"L4:"
" movl d2,d1"
" jmp a0@"
"L0:"
"|------------------------------------------------------"
"| #[primitive create-y] ="
"L1:"
" bmi L1000"
" TRAP1(9,1)"
" LBL_PTR(L1)"
"L1000:"
" movl d1,a1"
" movl a1@(-3),d2"
" lsrl #7,d2"
" movl a0,sp@-"
" movl d1,sp@-"
" movl d2,sp@-"
" movl d2,d1"
" lea L2,a0"
" dbra d5,L1001"
" moveq #9,d5"
" cmpl a5@,sp"
" bcc L1001"
" TRAP2(24)"
" RETURN(L1,3,1)"
"L1002:"
"L1001:"
" moveq #-1,d0"
" JMP_PRIM(make-vector,0)"
" RETURN(L1,3,1)"
"L2:"
" movl sp@+,d2"
" subql #8,d2"
" movl d2,d3"
" movl d1,d2"
" movl sp@+,d1"
" movl sp@+,a0"
" dbra d5,L1003"
" moveq #9,d5"
" cmpl a5@,sp"
" bcc L1003"
" movl a0,sp@-"
" TRAP2(24)"
" RETURN(L1,1,1)"
"L1004:"
" movl sp@+,a0"
"L1003:"
" movl d3,d0"
" blt L4"
"L3:"
" movl d3,d0"
" asrl #1,d0"
" movl d1,a1"
" movl a1@(1,d0:l),d4"
" movl d3,d0"
" asrl #1,d0"
" movl d2,a1"
" movl d4,a1@(1,d0:l)"
" subql #8,d3"
" dbra d5,L1005"
" moveq #9,d5"
" cmpl a5@,sp"
" bcc L1005"
" movl a0,sp@-"
" TRAP2(24)"
" RETURN(L1,1,1)"
"L1006:"
" movl sp@+,a0"
"L1005:"
" movl d3,d0"
" bge L3"
"L4:"
" movl d2,d1"
" jmp a0@"
"L0:"
"|------------------------------------------------------"
"| #[primitive my-try] ="
"L1:"
" bmi L1000"
" TRAP1(9,1)"
" LBL_PTR(L1)"
"L1000:"
" movl a0,sp@-"
" lea L2,a0"
" dbra d5,L1001"
" moveq #9,d5"
" cmpl a5@,sp"
" bcc L1001"
" TRAP2(24)"
" RETURN(L1,1,1)"
"L1002:"
"L1001:"
" JMP_PROC(4,10)"
" RETURN(L1,1,1)"
"L2:"
" lea L3,a0"
" JMP_PROC(5,10)"
" RETURN(L1,1,1)"
"L3:"
" movl d1,a1"
" movl a1@(-3),d1"
" lsrl #7,d1"
" dbra d5,L1003"
" moveq #9,d5"
" cmpl a5@,sp"
" bcc L1003"
" TRAP2(24)"
" RETURN(L1,1,1)"
"L1004:"
"L1003:"
" rts"
"L0:"
"|------------------------------------------------------"
"| #[primitive go] ="
"L1:"
" bmi L1000"
" TRAP1(9,1)"
" LBL_PTR(L1)"
"L1000:"
" moveq #0,d3"
" movl #800,d2"
" dbra d5,L1001"
" moveq #9,d5"
" cmpl a5@,sp"
" bcc L1001"
" movl a0,sp@-"
" TRAP2(24)"
" RETURN(L1,1,1)"
"L1002:"
" movl sp@+,a0"
"L1001:"
" movl d2,d0"
" ble L4"
" bra L3"
" RETURN(L1,3,1)"
"L2:"
" movl d1,d3"
" movl sp@+,d1"
" subql #8,d1"
" movl d1,d2"
" movl sp@+,d1"
" movl sp@+,a0"
" dbra d5,L1003"
" moveq #9,d5"
" cmpl a5@,sp"
" bcc L1003"
" movl a0,sp@-"
" TRAP2(24)"
" RETURN(L1,1,1)"
"L1004:"
" movl sp@+,a0"
"L1003:"
" movl d2,d0"
" ble L4"
"L3:"
" movl a0,sp@-"
" movl d1,sp@-"
" movl d2,sp@-"
" lea L2,a0"
" dbra d5,L1005"
" moveq #9,d5"
" cmpl a5@,sp"
" bcc L1005"
" TRAP2(24)"
" RETURN(L1,3,1)"
"L1006:"
"L1005:"
" JMP_PROC(6,10)"
"L4:"
" movl d3,d1"
" jmp a0@"
"L0:"
""))

(define (run1) (set-bbv-version-limit! #f) 
  (let ((expr (unknown input-source-code)) (target (unknown 'm68000)) (opt (unknown 'asm)))
    (ce expr target opt)
    (asm-output-get)))

(define-keys (run !key (n (unknown 2000 1)))
  (let loop ((n n) (result #f))
    (if (SFX> n 0)
        (loop (SFX- n 1) (run1))
        result)))

(define (check result) (set-bbv-version-limit! #f) 
  (equal? result output-expected))
