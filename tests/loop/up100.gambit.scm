(declare
  (standard-bindings)    ;; +, - and < are the predefined primitives
  (inlining-limit 0)     ;; no inlining
  (not constant-fold)    ;; no constant folding
)
 
(include "up100.scm")
