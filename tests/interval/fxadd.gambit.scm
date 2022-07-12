(declare
  (standard-bindings)    ;; +, - and < are the predefined primitives
  (extended-bindings)    ;; identity, etc are the predefined primitives
  (inlining-limit 0)     ;; no inlining
  (not constant-fold)    ;; no constant folding
)
 
(define (program x)
  (if (and (fixnum? x) (fx>= x 0))
      (let ((a (fx- x 1)))
        (list (##first-argument 1) x a))))
