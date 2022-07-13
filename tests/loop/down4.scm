;; This test checks that a loop with few iterations is completely unrolled.

(define (program)

  (define (loop i sum)
    (if (> i 0)
        (loop (- i 1)
              (+ sum i))
        sum))

  (loop 4 0))
