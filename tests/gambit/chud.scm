;;; Program to use the chudnovsky formula to compute pi.
;;; Written by Bakul Shah.
;;; Changed by Bradley Lucier to use standard arithmetic operations
;;; available in Gambit Scheme, and to use (quotient a b
;;; instead of (floor (/ a b)).

;;; This version computes 100,000 digits of pi and tests that the
;;; last five digits are correct.

(define ch-A 13591409)
(define ch-B 545140134)
(define ch-C 640320)
(define ch-C^3 (expt 640320 3))
(define ch-D 12)

(define (ch-split a b)
  (if (GEN= 1 (GEN- b a))
      (let ((g (GEN* (GEN- (GEN* 6 b) 5)
                         (GEN* (GEN- (GEN* 2 b) 1)
                                   (GEN- (GEN* 6 b) 1)))))
        (list g
              (GENquotient (GEN* ch-C^3 (GENexpt b 3)) 24)
              (GEN* (GENexpt -1 b)
                        (GEN* g (GEN+ (GEN* b ch-B) ch-A)))))
      (let* ((mid (GENquotient (GEN+ a b) 2))
             (gpq1 (ch-split a mid))    ;<<<<====
             (gpq2 (ch-split mid b))    ;<<<<====
             (g1 (car gpq1)) (p1 (cadr gpq1)) (q1 (caddr gpq1))
             (g2 (car gpq2)) (p2 (cadr gpq2)) (q2 (caddr gpq2)))
        (list (GEN* g1 g2)
              (GEN* p1 p2)
              (GEN+ (GEN* q1 p2)
                        (GEN* q2 g1))))))

(define (pi digits)
  (let* ((num-terms (inexact->exact (floor (GEN+ 2 (GEN/ digits 14.181647462)))))
         (sqrt-C (integer-sqrt (GEN* ch-C (GENexpt 100 digits)))))
    (let* ((gpq (ch-split 0 num-terms))
           (g (car gpq)) (p (cadr gpq)) (q (caddr gpq)))
      (GENquotient (GEN* p (GEN* ch-C sqrt-C))
                       (GEN* ch-D (GEN+ q (GEN* p ch-A)))))))

(define (run)
  (GENremainder (pi (unknown #e1e5)) 100000))

(define (check result)
  (equal? result 24646))
