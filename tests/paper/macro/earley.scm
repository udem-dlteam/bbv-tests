;;; EARLEY -- Earley's parser, written by Marc Feeley.

; (make-parser grammar lexer) is used to create a parser from the grammar
; description `grammar' and the lexer function `lexer'.
;
; A grammar is a list of definitions.  Each definition defines a non-terminal
; by a set of rules.  Thus a definition has the form: (nt rule1 rule2...).
; A given non-terminal can only be defined once.  The first non-terminal
; defined is the grammar's goal.  Each rule is a possibly empty list of
; non-terminals.  Thus a rule has the form: (nt1 nt2...).  A non-terminal
; can be any scheme value.  Note that all grammar symbols are treated as
; non-terminals.  This is fine though because the lexer will be outputing
; non-terminals.
;
; The lexer defines what a token is and the mapping between tokens and
; the grammar's non-terminals.  It is a function of one argument, the input,
; that returns the list of tokens corresponding to the input.  Each token is
; represented by a list.  The first element is some `user-defined' information
; associated with the token and the rest represents the token's class(es) (as a
; list of non-terminals that this token corresponds to).
;
; The result of `make-parser' is a function that parses the single input it
; is given into the grammar's goal.  The result is a `parse' which can be
; manipulated with the procedures: `parse->parsed?', `parse->trees'
; and `parse->nb-trees' (see below).
;
; Let's assume that we want a parser for the grammar
;
;  S -> x = E
;  E -> E + E | V
;  V -> V y |
;
; and that the input to the parser is a string of characters.  Also, assume we
; would like to map the characters `x', `y', `+' and `=' into the corresponding
; non-terminals in the grammar.  Such a parser could be created with
;
; (make-parser
;   '(
;      (s (x = e))
;      (e (e + e) (v))
;      (v (v y) ())
;    )
;   (lambda (str) (set-bbv-version-limit! #f) 
;     (map (lambda (char) (set-bbv-version-limit! #f) 
;            (list char ; user-info = the character itself
;                  (case char
;                    ((#\x) 'x)
;                    ((#\y) 'y)
;                    ((#\+) '+)
;                    ((#\=) '=)
;                    (else (fatal-error "lexer error")))))
;          (string->list str)))
; )
;
; An alternative definition (that does not check for lexical errors) is
;
; (make-parser
;   '(
;      (s (#\x #\= e))
;      (e (e #\+ e) (v))
;      (v (v #\y) ())
;    )
;   (lambda (str) (set-bbv-version-limit! #f)  (map (lambda (char) (set-bbv-version-limit! #f)  (list char char)) (string->list str)))
; )
;
; To help with the rest of the discussion, here are a few definitions:
;
; An input pointer (for an input of `n' tokens) is a value between 0 and `n'.
; It indicates a point between two input tokens (0 = beginning, `n' = end).
; For example, if `n' = 4, there are 5 input pointers:
;
;   input                   token1     token2     token3     token4
;   input pointers       0          1          2          3          4
;
; A configuration indicates the extent to which a given rule is parsed (this
; is the common `dot notation').  For simplicity, a configuration is
; represented as an integer, with successive configurations in the same
; rule associated with successive integers.  It is assumed that the grammar
; has been extended with rules to aid scanning.  These rules are of the
; form `nt ->', and there is one such rule for every non-terminal.  Note
; that these rules are special because they only apply when the corresponding
; non-terminal is returned by the lexer.
;
; A configuration set is a configuration grouped with the set of input pointers
; representing where the head non-terminal of the configuration was predicted.
;
; Here are the rules and configurations for the grammar given above:
;
;  S -> .         \
;       0          |
;  x -> .          |
;       1          |
;  = -> .          |
;       2          |
;  E -> .          |
;       3           > special rules (for scanning)
;  + -> .          |
;       4          |
;  V -> .          |
;       5          |
;  y -> .          |
;       6         /
;  S -> .  x  .  =  .  E  .
;       7     8     9     10
;  E -> .  E  .  +  .  E  .
;       11    12    13    14
;  E -> .  V  .
;       15    16
;  V -> .  V  .  y  .
;       17    18    19
;  V -> .
;       20
;
; Starters of the non-terminal `nt' are configurations that are leftmost
; in a non-special rule for `nt'.  Enders of the non-terminal `nt' are
; configurations that are rightmost in any rule for `nt'.  Predictors of the
; non-terminal `nt' are configurations that are directly to the left of `nt'
; in any rule.
;
; For the grammar given above,
;
;   Starters of V   = (17 20)
;   Enders of V     = (5 19 20)
;   Predictors of V = (15 17)

(set-custom-version-limits! 2 1 5 4 4 3 2 9 2 10 7 1 1 2 4 4 9 10 1 9 4 9 7 4 8
  10 5 1 3 7 6 5 3 4 6 2 2 7 2 6 6 10 5 1 8 9)

(define (make-parser grammar lexer) (set-bbv-version-limit! #f) 

  (define (non-terminals grammar) (set-bbv-version-limit! #f)  ; return vector of non-terminals in grammar

    (define (add-nt nt nts) (set-bbv-version-limit! #f) 
      (if (Smember nt nts) nts (cons nt nts))) ; use equal? for equality tests

    (let def-loop ((defs grammar) (nts '()))
      (if (pair? defs)
        (let* ((def (Scar defs))
               (head (Scar def)))
          (let rule-loop ((rules (Scdr def))
                          (nts (add-nt head nts)))
            (if (pair? rules)
              (let ((rule (Scar rules)))
                (let loop ((l rule) (nts nts))
                  (if (pair? l)
                    (let ((nt (Scar l)))
                      (loop (Scdr l) (add-nt nt nts)))
                    (rule-loop (Scdr rules) nts))))
              (def-loop (Scdr defs) nts))))
        (Slist->vector (Sreverse nts))))) ; goal non-terminal must be at index 0

  (define (ind nt nts) (set-bbv-version-limit! #f)  ; return index of non-terminal `nt' in `nts'
    (let loop ((i (SFX- (Svector-length nts) 1)))
      (if (SFX>= i 0)
        (if (LIBequal? (Svector-ref nts i) nt) i (loop (SFX- i 1)))
        #f)))

  (define (nb-configurations grammar) (set-bbv-version-limit! #f)  ; return nb of configurations in grammar
    (let def-loop ((defs grammar) (nb-confs 0))
      (if (pair? defs)
        (let ((def (Scar defs)))
          (let rule-loop ((rules (Scdr def)) (nb-confs nb-confs))
            (if (pair? rules)
              (let ((rule (Scar rules)))
                (let loop ((l rule) (nb-confs nb-confs))
                  (if (pair? l)
                    (loop (Scdr l) (SFX+ nb-confs 1))
                    (rule-loop (Scdr rules) (SFX+ nb-confs 1)))))
              (def-loop (Scdr defs) nb-confs))))
      nb-confs)))

; First, associate a numeric identifier to every non-terminal in the
; grammar (with the goal non-terminal associated with 0).
;
; So, for the grammar given above we get:
;
; s -> 0   x -> 1   = -> 4   e ->3    + -> 4   v -> 5   y -> 6

  (let* ((nts (non-terminals grammar))          ; id map = list of non-terms
         (nb-nts (Svector-length nts))           ; the number of non-terms
         (nb-confs (SFX+ (nb-configurations grammar) nb-nts)) ; the nb of confs
         (starters (Smake-vector2 nb-nts '()))    ; starters for every non-term
         (enders (Smake-vector2 nb-nts '()))      ; enders for every non-term
         (predictors (Smake-vector2 nb-nts '()))  ; predictors for every non-term
         (steps (Smake-vector2 nb-confs #f))      ; what to do in a given conf
         (names (Smake-vector2 nb-confs #f)))     ; name of rules

    (define (setup-tables grammar nts starters enders predictors steps names) (set-bbv-version-limit! #f) 

      (define (add-conf conf nt nts class) (set-bbv-version-limit! #f) 
        (let ((i (ind nt nts)))
          (Svector-set! class i (cons conf (Svector-ref class i)))))

      (let ((nb-nts (Svector-length nts)))

        (let nt-loop ((i (SFX- nb-nts 1)))
          (if (SFX>= i 0)
            (begin
              (Svector-set! steps i (SFX- i nb-nts))
              (Svector-set! names i (list (Svector-ref nts i) 0))
              (Svector-set! enders i (list i))
              (nt-loop (SFX- i 1)))))

        (let def-loop ((defs grammar) (conf (Svector-length nts)))
          (if (pair? defs)
            (let* ((def (Scar defs))
                   (head (Scar def)))
              (let rule-loop ((rules (Scdr def)) (conf conf) (rule-num 1))
                (if (pair? rules)
                  (let ((rule (Scar rules)))
                    (Svector-set! names conf (list head rule-num))
                    (add-conf conf head nts starters)
                    (let loop ((l rule) (conf conf))
                      (if (pair? l)
                        (let ((nt (Scar l)))
                          (Svector-set! steps conf (ind nt nts))
                          (add-conf conf nt nts predictors)
                          (loop (Scdr l) (SFX+ conf 1)))
                        (begin
                          (Svector-set! steps conf (SFX- (ind head nts) nb-nts))
                          (add-conf conf head nts enders)
                          (rule-loop (Scdr rules) (SFX+ conf 1) (SFX+ rule-num 1))))))
                  (def-loop (Scdr defs) conf))))))))

; Now, for each non-terminal, compute the starters, enders and predictors and
; the names and steps tables.

    (setup-tables grammar nts starters enders predictors steps names)

; Build the parser description

    (let ((parser-descr (vector lexer
                                nts
                                starters
                                enders
                                predictors
                                steps
                                names)))
      (lambda (input) (set-bbv-version-limit! #f) 

        (define (ind nt nts) (set-bbv-version-limit! #f)  ; return index of non-terminal `nt' in `nts'
          (let loop ((i (SFX- (Svector-length nts) 1)))
            (if (SFX>= i 0)
              (if (LIBequal? (Svector-ref nts i) nt) i (loop (SFX- i 1)))
              #f)))

        (define (comp-tok tok nts) (set-bbv-version-limit! #f)  ; transform token to parsing format
          (let loop ((l1 (Scdr tok)) (l2 '()))
            (if (pair? l1)
              (let ((i (ind (Scar l1) nts)))
                (if i
                  (loop (Scdr l1) (cons i l2))
                  (loop (Scdr l1) l2)))
              (cons (Scar tok) (Sreverse l2)))))

        (define (input->tokens input lexer nts) (set-bbv-version-limit! #f) 
          (Slist->vector (Smap2 (lambda (tok) (set-bbv-version-limit! #f)  (comp-tok tok nts)) (lexer input))))

        (define (make-states nb-toks nb-confs) (set-bbv-version-limit! #f) 
          (let ((states (Smake-vector2 (SFX+ nb-toks 1) #f)))
            (let loop ((i nb-toks))
              (if (SFX>= i 0)
                (let ((v (Smake-vector2 (SFX+ nb-confs 1) #f)))
                  (Svector-set! v 0 -1)
                  (Svector-set! states i v)
                  (loop (SFX- i 1)))
                states))))

        (define (conf-set-get state conf) (set-bbv-version-limit! #f) 
          (Svector-ref state (SFX+ conf 1)))

        (define (conf-set-get* state state-num conf) (set-bbv-version-limit! #f) 
          (let ((conf-set (conf-set-get state conf)))
            (if conf-set
              conf-set
              (let ((conf-set (Smake-vector2 (SFX+ state-num 6) #f)))
                (Svector-set! conf-set 1 -3) ; old elems tail (points to head)
                (Svector-set! conf-set 2 -1) ; old elems head
                (Svector-set! conf-set 3 -1) ; new elems tail (points to head)
                (Svector-set! conf-set 4 -1) ; new elems head
                (Svector-set! state (SFX+ conf 1) conf-set)
                conf-set))))

        (define (conf-set-merge-new! conf-set) (set-bbv-version-limit! #f) 
          (Svector-set! conf-set
            (SFX+ (Svector-ref conf-set 1) 5)
            (Svector-ref conf-set 4))
          (Svector-set! conf-set 1 (Svector-ref conf-set 3))
          (Svector-set! conf-set 3 -1)
          (Svector-set! conf-set 4 -1))

        (define (conf-set-head conf-set) (set-bbv-version-limit! #f) 
          (Svector-ref conf-set 2))

        (define (conf-set-next conf-set i) (set-bbv-version-limit! #f) 
          (Svector-ref conf-set (SFX+ i 5)))

        (define (conf-set-member? state conf i) (set-bbv-version-limit! #f) 
          (let ((conf-set (Svector-ref state (SFX+ conf 1))))
            (if conf-set
              (conf-set-next conf-set i)
              #f)))

        (define (conf-set-adjoin state conf-set conf i) (set-bbv-version-limit! #f) 
          (let ((tail (Svector-ref conf-set 3))) ; put new element at tail
            (Svector-set! conf-set (SFX+ i 5) -1)
            (Svector-set! conf-set (SFX+ tail 5) i)
            (Svector-set! conf-set 3 i)
            (if (SFX< tail 0)
              (begin
                (Svector-set! conf-set 0 (Svector-ref state 0))
                (Svector-set! state 0 conf)))))

        (define (conf-set-adjoin* states state-num l i) (set-bbv-version-limit! #f) 
          (let ((state (Svector-ref states state-num)))
            (let loop ((l1 l))
              (if (pair? l1)
                (let* ((conf (Scar l1))
                       (conf-set (conf-set-get* state state-num conf)))
                  (if (not (conf-set-next conf-set i))
                    (begin
                      (conf-set-adjoin state conf-set conf i)
                      (loop (Scdr l1)))
                    (loop (Scdr l1))))))))

        (define (conf-set-adjoin** states states* state-num conf i) (set-bbv-version-limit! #f) 
          (let ((state (Svector-ref states state-num)))
            (if (conf-set-member? state conf i)
              (let* ((state* (Svector-ref states* state-num))
                     (conf-set* (conf-set-get* state* state-num conf)))
                (if (not (conf-set-next conf-set* i))
                  (conf-set-adjoin state* conf-set* conf i))
                #t)
              #f)))

        (define (conf-set-union state conf-set conf other-set) (set-bbv-version-limit! #f) 
          (let loop ((i (conf-set-head other-set)))
            (if (SFX>= i 0)
              (if (not (conf-set-next conf-set i))
                (begin
                  (conf-set-adjoin state conf-set conf i)
                  (loop (conf-set-next other-set i)))
                (loop (conf-set-next other-set i))))))

        (define (forw states state-num starters enders predictors steps nts) (set-bbv-version-limit! #f) 

          (define (predict state state-num conf-set conf nt starters enders) (set-bbv-version-limit! #f) 

            ; add configurations which start the non-terminal `nt' to the
            ; right of the dot

            (let loop1 ((l (Svector-ref starters nt)))
              (if (pair? l)
                (let* ((starter (Scar l))
                       (starter-set (conf-set-get* state state-num starter)))
                  (if (not (conf-set-next starter-set state-num))
                    (begin
                      (conf-set-adjoin state starter-set starter state-num)
                      (loop1 (Scdr l)))
                    (loop1 (Scdr l))))))

            ; check for possible completion of the non-terminal `nt' to the
            ; right of the dot

            (let loop2 ((l (Svector-ref enders nt)))
              (if (pair? l)
                (let ((ender (Scar l)))
                  (if (conf-set-member? state ender state-num)
                    (let* ((next (SFX+ conf 1))
                           (next-set (conf-set-get* state state-num next)))
                      (conf-set-union state next-set next conf-set)
                      (loop2 (Scdr l)))
                    (loop2 (Scdr l)))))))

          (define (reduce states state state-num conf-set head preds) (set-bbv-version-limit! #f) 

            ; a non-terminal is now completed so check for reductions that
            ; are now possible at the configurations `preds'

            (let loop1 ((l preds))
              (if (pair? l)
                (let ((pred (Scar l)))
                  (let loop2 ((i head))
                    (if (SFX>= i 0)
                      (let ((pred-set (conf-set-get (Svector-ref states i) pred)))
                        (if pred-set
                          (let* ((next (SFX+ pred 1))
                                 (next-set (conf-set-get* state state-num next)))
                            (conf-set-union state next-set next pred-set)))
                        (loop2 (conf-set-next conf-set i)))
                      (loop1 (Scdr l))))))))

          (let ((state (Svector-ref states state-num))
                (nb-nts (Svector-length nts)))
            (let loop ()
              (let ((conf (Svector-ref state 0)))
                (if (SFX>= conf 0)
                  (let* ((step (Svector-ref steps conf))
                         (conf-set (Svector-ref state (SFX+ conf 1)))
                         (head (Svector-ref conf-set 4)))
                    (Svector-set! state 0 (Svector-ref conf-set 0))
                    (conf-set-merge-new! conf-set)
                    (if (SFX>= step 0)
                      (predict state state-num conf-set conf step starters enders)
                      (let ((preds (Svector-ref predictors (SFX+ step nb-nts))))
                        (reduce states state state-num conf-set head preds)))
                    (loop)))))))

        (define (forward starters enders predictors steps nts toks) (set-bbv-version-limit! #f) 
          (let* ((nb-toks (Svector-length toks))
                 (nb-confs (Svector-length steps))
                 (states (make-states nb-toks nb-confs))
                 (goal-starters (Svector-ref starters 0)))
            (conf-set-adjoin* states 0 goal-starters 0) ; predict goal
            (forw states 0 starters enders predictors steps nts)
            (let loop ((i 0))
              (if (SFX< i nb-toks)
                (let ((tok-nts (Scdr (Svector-ref toks i))))
                  (conf-set-adjoin* states (SFX+ i 1) tok-nts i) ; scan token
                  (forw states (SFX+ i 1) starters enders predictors steps nts)
                  (loop (SFX+ i 1)))))
            states))

        (define (produce conf i j enders steps toks states states* nb-nts) (set-bbv-version-limit! #f) 
          (let ((prev (SFX- conf 1)))
            (if (and (SFX>= conf nb-nts) (SFX>= (Svector-ref steps prev) 0))
              (let loop1 ((l (Svector-ref enders (Svector-ref steps prev))))
                (if (pair? l)
                  (let* ((ender (Scar l))
                         (ender-set (conf-set-get (Svector-ref states j)
                                                  ender)))
                    (if ender-set
                      (let loop2 ((k (conf-set-head ender-set)))
                        (if (SFX>= k 0)
                          (begin
                            (and (SFX>= k i)
                                 (conf-set-adjoin** states states* k prev i)
                                 (conf-set-adjoin** states states* j ender k))
                            (loop2 (conf-set-next ender-set k)))
                          (loop1 (Scdr l))))
                      (loop1 (Scdr l)))))))))

        (define (back states states* state-num enders steps nb-nts toks) (set-bbv-version-limit! #f) 
          (let ((state* (Svector-ref states* state-num)))
            (let loop1 ()
              (let ((conf (Svector-ref state* 0)))
                (if (SFX>= conf 0)
                  (let* ((conf-set (Svector-ref state* (SFX+ conf 1)))
                         (head (Svector-ref conf-set 4)))
                    (Svector-set! state* 0 (Svector-ref conf-set 0))
                    (conf-set-merge-new! conf-set)
                    (let loop2 ((i head))
                      (if (SFX>= i 0)
                        (begin
                          (produce conf i state-num enders steps
                                   toks states states* nb-nts)
                          (loop2 (conf-set-next conf-set i)))
                        (loop1)))))))))

        (define (backward states enders steps nts toks) (set-bbv-version-limit! #f) 
          (let* ((nb-toks (Svector-length toks))
                 (nb-confs (Svector-length steps))
                 (nb-nts (Svector-length nts))
                 (states* (make-states nb-toks nb-confs))
                 (goal-enders (Svector-ref enders 0)))
            (let loop1 ((l goal-enders))
              (if (pair? l)
                (let ((conf (Scar l)))
                  (conf-set-adjoin** states states* nb-toks conf 0)
                  (loop1 (Scdr l)))))
            (let loop2 ((i nb-toks))
              (if (SFX>= i 0)
                (begin
                  (back states states* i enders steps nb-nts toks)
                  (loop2 (SFX- i 1)))))
            states*))

        (define (parsed? nt i j nts enders states) (set-bbv-version-limit! #f) 
          (let ((nt* (ind nt nts)))
            (if nt*
              (let ((nb-nts (Svector-length nts)))
                (let loop ((l (Svector-ref enders nt*)))
                  (if (pair? l)
                    (let ((conf (Scar l)))
                      (if (conf-set-member? (Svector-ref states j) conf i)
                        #t
                        (loop (Scdr l))))
                    #f)))
              #f)))

        (define (deriv-trees conf i j enders steps names toks states nb-nts) (set-bbv-version-limit! #f) 
          (let ((name (Svector-ref names conf)))

            (if name ; `conf' is at the start of a rule (either special or not)
              (if (SFX< conf nb-nts)
                (list (list name (Scar (Svector-ref toks i))))
                (list (list name)))

              (let ((prev (SFX- conf 1)))
                (let loop1 ((l1 (Svector-ref enders (Svector-ref steps prev)))
                            (l2 '()))
                  (if (pair? l1)
                    (let* ((ender (Scar l1))
                           (ender-set (conf-set-get (Svector-ref states j)
                                                    ender)))
                      (if ender-set
                        (let loop2 ((k (conf-set-head ender-set)) (l2 l2))
                          (if (SFX>= k 0)
                            (if (and (SFX>= k i)
                                     (conf-set-member? (Svector-ref states k)
                                                       prev i))
                              (let ((prev-trees
                                      (deriv-trees prev i k enders steps names
                                                   toks states nb-nts))
                                    (ender-trees
                                      (deriv-trees ender k j enders steps names
                                                   toks states nb-nts)))
                                (let loop3 ((l3 ender-trees) (l2 l2))
                                  (if (pair? l3)
                                    (let ((ender-tree (list (Scar l3))))
                                      (let loop4 ((l4 prev-trees) (l2 l2))
                                        (if (pair? l4)
                                          (loop4 (Scdr l4)
                                                 (cons (Sappend (Scar l4)
                                                                ender-tree)
                                                       l2))
                                          (loop3 (Scdr l3) l2))))
                                    (loop2 (conf-set-next ender-set k) l2))))
                              (loop2 (conf-set-next ender-set k) l2))
                            (loop1 (Scdr l1) l2)))
                        (loop1 (Scdr l1) l2)))
                    l2))))))

        (define (deriv-trees* nt i j nts enders steps names toks states) (set-bbv-version-limit! #f) 
          (let ((nt* (ind nt nts)))
            (if nt*
              (let ((nb-nts (Svector-length nts)))
                (let loop ((l (Svector-ref enders nt*)) (trees '()))
                  (if (pair? l)
                    (let ((conf (Scar l)))
                      (if (conf-set-member? (Svector-ref states j) conf i)
                        (loop (Scdr l)
                              (Sappend (deriv-trees conf i j enders steps names
                                                    toks states nb-nts)
                                      trees))
                        (loop (Scdr l) trees)))
                    trees)))
              #f)))

        (define (nb-deriv-trees conf i j enders steps toks states nb-nts) (set-bbv-version-limit! #f) 
          (let ((prev (SFX- conf 1)))
            (if (or (SFX< conf nb-nts) (SFX< (Svector-ref steps prev) 0))
              1
              (let loop1 ((l (Svector-ref enders (Svector-ref steps prev)))
                          (n 0))
                (if (pair? l)
                  (let* ((ender (Scar l))
                         (ender-set (conf-set-get (Svector-ref states j)
                                                  ender)))
                    (if ender-set
                      (let loop2 ((k (conf-set-head ender-set)) (n n))
                        (if (SFX>= k 0)
                          (if (and (SFX>= k i)
                                   (conf-set-member? (Svector-ref states k)
                                                     prev i))
                            (let ((nb-prev-trees
                                    (nb-deriv-trees prev i k enders steps
                                                    toks states nb-nts))
                                  (nb-ender-trees
                                    (nb-deriv-trees ender k j enders steps
                                                    toks states nb-nts)))
                              (loop2 (conf-set-next ender-set k)
                                     (SFX+ n (SFX* nb-prev-trees nb-ender-trees))))
                            (loop2 (conf-set-next ender-set k) n))
                          (loop1 (Scdr l) n)))
                      (loop1 (Scdr l) n)))
                  n)))))

        (define (nb-deriv-trees* nt i j nts enders steps toks states) (set-bbv-version-limit! #f) 
          (let ((nt* (ind nt nts)))
            (if nt*
              (let ((nb-nts (Svector-length nts)))
                (let loop ((l (Svector-ref enders nt*)) (nb-trees 0))
                  (if (pair? l)
                    (let ((conf (Scar l)))
                      (if (conf-set-member? (Svector-ref states j) conf i)
                        (loop (Scdr l)
                              (SFX+ (nb-deriv-trees conf i j enders steps
                                                 toks states nb-nts)
                                 nb-trees))
                        (loop (Scdr l) nb-trees)))
                    nb-trees)))
              #f)))

        (let* ((lexer      (Svector-ref parser-descr 0))
               (nts        (Svector-ref parser-descr 1))
               (starters   (Svector-ref parser-descr 2))
               (enders     (Svector-ref parser-descr 3))
               (predictors (Svector-ref parser-descr 4))
               (steps      (Svector-ref parser-descr 5))
               (names      (Svector-ref parser-descr 6))
               (toks       (input->tokens input lexer nts)))

          (vector nts
                  starters
                  enders
                  predictors
                  steps
                  names
                  toks
                  (backward (forward starters enders predictors steps nts toks)
                            enders steps nts toks)
                  parsed?
                  deriv-trees*
                  nb-deriv-trees*))))))

(define (parse->parsed? parse nt i j) (set-bbv-version-limit! #f) 
  (let* ((nts     (Svector-ref parse 0))
         (enders  (Svector-ref parse 2))
         (states  (Svector-ref parse 7))
         (parsed? (Svector-ref parse 8)))
    (parsed? nt i j nts enders states)))

(define (parse->trees parse nt i j) (set-bbv-version-limit! #f) 
  (let* ((nts          (Svector-ref parse 0))
         (enders       (Svector-ref parse 2))
         (steps        (Svector-ref parse 4))
         (names        (Svector-ref parse 5))
         (toks         (Svector-ref parse 6))
         (states       (Svector-ref parse 7))
         (deriv-trees* (Svector-ref parse 9)))
    (deriv-trees* nt i j nts enders steps names toks states)))

(define (parse->nb-trees parse nt i j) (set-bbv-version-limit! #f) 
  (let* ((nts             (Svector-ref parse 0))
         (enders          (Svector-ref parse 2))
         (steps           (Svector-ref parse 4))
         (toks            (Svector-ref parse 6))
         (states          (Svector-ref parse 7))
         (nb-deriv-trees* (Svector-ref parse 10)))
    (nb-deriv-trees* nt i j nts enders steps toks states)))

(define (test) (set-bbv-version-limit! #f) 
  (let ((p (make-parser '( (s (a) (s s)) )
                        (lambda (l) (set-bbv-version-limit! #f)  (Smap2 (lambda (x) (set-bbv-version-limit! #f)  (list x x)) l)))))
    (let ((x (p '(a a a a a a a a a))))
      (Slength (parse->trees x 's 0 9)))))

(define-keys (run !key (n (unknown 10000 1)))
  (let loop ((n n) (result #f))
    (if (SFX> n 0)
        (loop (SFX- n 1) (test))
        result)))

(define expected-result 1430)

(define (check result) (set-bbv-version-limit! #f) 
   (equal? result expected-result))
