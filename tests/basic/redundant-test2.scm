;; This test checks that redundant tests are removed both in the
;; positive (then) and negative (else) branches regardless of the set
;; of types tested.  Typically this requires a type system that can
;; express excluded types.

(define (program unknown1)

  (define (f x)
    (cond ((fixnum? x)
           (cond ((fixnum? x)
                  11)
                 ((flonum? x)
                  12)
                 ((string? x)
                  13)
                 (else
                  (cons 14 x))))
          ((flonum? x)
           (cond ((fixnum? x)
                  21)
                 ((flonum? x)
                  22)
                 ((string? x)
                  23)
                 (else
                  (cons 24 x))))
          ((string? x)
           (cond ((fixnum? x)
                  31)
                 ((flonum? x)
                  32)
                 ((string? x)
                  33)
                 (else
                  (cons 34 x))))
          (else
           (cond ((fixnum? x)
                  41)
                 ((flonum? x)
                  42)
                 ((string? x)
                  43)
                 (else
                  (cons 44 x))))))

  (f unknown1))
