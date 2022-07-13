;; This test checks that a loop with a variable but known to be small number
;; of iterations is unrolled.

(define (program unknown1)

  (define (loop i sum n)
    (if (< i n)
        (loop (+ i 1)
              (+ sum i)
              n)
        sum))

  (if (< unknown1 5)
      (loop 0 0 unknown1)))
