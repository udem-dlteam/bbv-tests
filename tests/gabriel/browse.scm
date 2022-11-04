(define (lookup key table)
  (let loop ((x table))
    (if (null? x)
      #f
      (let ((pair (Scar x)))
        (if (eq? (Scar pair) key)
          pair
          (loop (Scdr x)))))))

(define properties '())

(define (get key1 key2)
  (let ((x (lookup key1 properties)))
    (if x
      (let ((y (lookup key2 (Scdr x))))
        (if y
          (Scdr y)
          #f))
      #f)))

(define (put key1 key2 val)
  (let ((x (lookup key1 properties)))
    (if x
      (let ((y (lookup key2 (Scdr x))))
        (if y
          (Sset-cdr! y val)
          (Sset-cdr! x (cons (cons key2 val) (Scdr x)))))
      (set! properties
        (cons (list key1 (cons key2 val)) properties)))))

(define *current-gensym* 0)

(define (generate-symbol)
  (set! *current-gensym* (SFX+ *current-gensym* 1))
  (Sstring->symbol (SFXnumber->string *current-gensym*)))

(define (tree-copy x)
  (if (not (pair? x))
      x
      (cons (tree-copy (Scar x))
            (tree-copy (Scdr x)))))

;;; n is # of symbols
;;; m is maximum amount of stuff on the plist
;;; npats is the number of basic patterns on the unit
;;; ipats is the instantiated copies of the patterns

(define *rand* 21)

(define (init n m npats ipats)
  (let ((ipats (tree-copy ipats)))
    (do ((p ipats (Scdr p)))
        ((null? (Scdr p)) (Sset-cdr! p ipats)))
    (do ((n n (SFX- n 1))
         (i m (cond ((SFXzero? i) m)
                    (else (SFX- i 1))))
         (name (generate-symbol) (generate-symbol))
         (a '()))
        ((SFX= n 0) a)
        (set! a (cons name a))
        (do ((i i (SFX- i 1)))
            ((SFXzero? i))
            (put name (generate-symbol) #f))
        (put name
             'pattern
             (do ((i npats (SFX- i 1))
                  (ipats ipats (Scdr ipats))
                  (a '()))
                 ((SFXzero? i) a)
                 (set! a (cons (Scar ipats) a))))
        (do ((j (SFX- m i) (SFX- j 1)))
            ((SFXzero? j))
            (put name (generate-symbol) #f)))))

(define (browse-random)
  (set! *rand* (SFXremainder (SFX* *rand* 17) 251))
  *rand*)

(define (randomize l)
  (do ((a '()))
      ((null? l) a)
      (let ((n (SFXremainder (browse-random) (Slength l))))
        (cond ((SFXzero? n)
               (set! a (cons (Scar l) a))
               (set! l (Scdr l))
               l)
              (else
               (do ((n n (SFX- n 1))
                    (x l (Scdr x)))
                   ((SFX= n 1)
                    (set! a (cons (Scadr x) a))
                    (set-cdr! x (Scddr x))
                    x)))))))

(define database
   (randomize
    (init 100 10 4 '((a a a b b b b a a a a a b b a a a)
                     (a a b b b b a a
                                    (a a)(b b))
                     (a a a b (b a) b a b a)))))

(define (run #!key (pats '((*a ?b *b ?b a *a a *b *a)
                           (*a *b *b *a (*a) (*b))
                           (? ? * (b a) * ? ?))))

;;; BROWSE -- Benchmark to create and browse through
;;; an AI-like data base of units.

(define (append-to-tail! x y)
  (if (null? x)
      y
      (do ((a x b)
           (b (Scdr x) (Scdr b)))
          ((null? b)
           (Sset-cdr! a y)
           x))))

(define (my-match pat dat alist)
  (cond ((null? pat)
         (null? dat))
        ((null? dat) '())
        ((or (eq? (Scar pat) '?)
             (eq? (Scar pat)
                  (Scar dat)))
         (my-match (Scdr pat) (Scdr dat) alist))
        ((eq? (Scar pat) '*)
         (or (my-match (Scdr pat) dat alist)
             (my-match (Scdr pat) (Scdr dat) alist)
             (my-match pat (Scdr dat) alist)))
        (else (cond ((not (pair? (Scar pat)))
                     (cond ((eq? (Sstring-ref (Ssymbol->string (Scar pat)) 0)
                                 #\?)
                            (let ((val (Sassq (Scar pat) alist)))
                              (cond (val (my-match (cons (Scdr val)
                                                      (Scdr pat))
                                                dat alist))
                                    (else (my-match (Scdr pat)
                                                 (Scdr dat)
                                                 (cons (cons (Scar pat)
                                                             (Scar dat))
                                                       alist))))))
                           ((eq? (Sstring-ref (Ssymbol->string (Scar pat)) 0)
                                 #\*)
                            (let ((val (Sassq (Scar pat) alist)))
                              (cond (val (my-match (Sappend (Scdr val)
                                                        (Scdr pat))
                                                dat alist))
                                    (else
                                     (do ((l '()
                                             (append-to-tail!
                                               l
                                               (cons (if (null? d)
                                                         '()
                                                         (Scar d))
                                                     '())))
                                          (e (cons '() dat) (Scdr e))
                                          (d dat (if (null? d) '() (Scdr d))))
                                         ((or (null? e)
                                              (my-match (Scdr pat)
                                                       d
                                                       (cons
                                                        (cons (Scar pat) l)
                                                        alist)))
                                          (if (null? e) #f #t)))))))
                           (else #f))) ;;;; fix suggested by Manuel Serrano (cond did not have an else clause); this changes the run time quite a bit
                    (else (and
                           (pair? (Scar dat))
                           (my-match (Scar pat)
                                  (Scar dat) alist)
                           (my-match (Scdr pat)
                                  (Scdr dat) alist)))))))

(define (browse pats)
  (investigate
    database
    pats))

(define (investigate units pats)
  (do ((units units (Scdr units)))
      ((null? units))
      (do ((pats pats (Scdr pats)))
          ((null? pats))
          (do ((p (get (Scar units) 'pattern)
                  (Scdr p)))
              ((null? p))
              (my-match (Scar pats) (Scar p) '())))))

(browse pats))

(define (check result)
  #t) ;; no easy way to check
