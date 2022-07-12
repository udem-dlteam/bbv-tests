;; This test checks that redundant tests are removed both in the
;; positive (then) and negative (else) branches.

(define (program unknown1)

  (define (f x)
    (cond ((fixnum? x)
           (cond ((fixnum? x)
                  11)
                 (else
                  12)))
          (else
           (cond ((fixnum? x)
                  21)
                 (else
                  22)))))

  (f unknown1))
