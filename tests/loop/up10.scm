;; This test checks that a loop with a constant but not small number
;; of iterations is not unrolled.

(define (program)

  (define (loop i sum)
    (if (< i 10)
        (loop (+ i 1)
              (+ sum i))
        sum))

  (loop 0 0))
