(declare
  (standard-bindings)    ;; +, - and < are the predefined primitives
  (extended-bindings)    ;; identity, etc are the predefined primitives
  (inlining-limit 0)     ;; no inlining
  (not constant-fold)    ;; no constant folding
)

(include "fxlt.scm")
