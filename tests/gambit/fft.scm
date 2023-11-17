;;; FFT - Fast Fourier Transform, translated from "Numerical Recipes in C"

(define (four1 data)
  (let ((n (Svector-length data))
        (pi*2 6.28318530717959)) ; to compute the inverse, negate this value

    ; bit-reversal section

    (let loop1 ((i 0) (j 0))
      (if (< i n)
        (begin
          (if (< i j)
            (begin
              (let ((temp (Svector-ref data i)))
                (Svector-set! data i (Svector-ref data j))
                (Svector-set! data j temp))
              (let ((temp (Svector-ref data (+ i 1))))
                (Svector-set! data (+ i 1) (Svector-ref data (+ j 1)))
                (Svector-set! data (+ j 1) temp))))
          (let loop2 ((m (quotient n 2)) (j j))
            (if (and (>= m 2) (>= j m))
              (loop2 (quotient m 2) (- j m))
              (loop1 (+ i 2) (+ j m)))))))

    ; Danielson-Lanczos section

    (let loop3 ((mmax 2))
      (if (< mmax n)
        (let* ((theta
                (SFL/ pi*2 (exact->inexact mmax)))
               (wpr
                (let ((x (SFLsin (SFL* 0.5 theta))))
                  (SFL* -2.0 (SFL* x x))))
               (wpi
                (SFLsin theta)))
          (let loop4 ((wr 1.0) (wi 0.0) (m 0))
            (if (< m mmax)
              (begin
                (let loop5 ((i m))
                  (if (< i n)
                    (let* ((j
                            (+ i mmax))
                           (tempr
                            (SFL-
                              (SFL* wr (Svector-ref data j))
                              (SFL* wi (Svector-ref data (+ j 1)))))
                           (tempi
                            (SFL+
                              (SFL* wr (Svector-ref data (+ j 1)))
                              (SFL* wi (Svector-ref data j)))))
                      (Svector-set! data j
                        (SFL- (Svector-ref data i) tempr))
                      (Svector-set! data (+ j 1)
                        (SFL- (Svector-ref data (+ i 1)) tempi))
                      (Svector-set! data i
                        (SFL+ (Svector-ref data i) tempr))
                      (Svector-set! data (+ i 1)
                        (SFL+ (Svector-ref data (+ i 1)) tempi))
                      (loop5 (+ j mmax)));***))
                (loop4 (SFL+ (SFL- (SFL* wr wpr) (SFL* wi wpi)) wr)
                       (SFL+ (SFL+ (SFL* wi wpr) (SFL* wr wpi)) wi)
                       (+ m 2)))))
));******
          (loop3 (* mmax 2)))))))

(define (run #!key (n (unknown 1048576 1024)))
  (let ((v (Smake-vector2 n 0.0)))
    (four1 v)
    (Svector-ref v 0)))

(define (check result)
  (equal? result 0.))
