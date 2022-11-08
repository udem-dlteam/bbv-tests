;; This is probably from Lars Hansen's MS thesis.
;; The quick-1 benchmark.  (Figure 35, page 132.)

;;(import (scheme base) (scheme read) (scheme write) (scheme time))

(define (quick-1 v less?)

  (define (helper left right)
    (if (SFX< left right)
        (let ((median (partition v left right less?)))
          (if (SFX< (SFX- median left) (SFX- right median))
              (begin (helper left (SFX- median 1))
                     (helper (+ median 1) right))
              (begin (helper (+ median 1) right)
                     (helper left (SFX- median 1)))))
        v))

  (helper 0 (SFX- (Svector-length v) 1)))


(define (partition v left right less?)
  (let ((mid (Svector-ref v right)))

    (define (uploop i)
      (let ((i (+ i 1)))
        (if (and (SFX< i right) (less? (Svector-ref v i) mid))
            (uploop i)
            i)))

    (define (downloop j)
      (let ((j (SFX- j 1)))
        (if (and (> j left) (less? mid (Svector-ref v j)))
            (downloop j)
            j)))

    (define (ploop i j)
      (let* ((i (uploop i))
             (j (downloop j)))
        (let ((tmp (Svector-ref v i)))
          (Svector-set! v i (Svector-ref v j))
          (Svector-set! v j tmp)
          (if (SFX< i j)
              (ploop i j)
              (begin (Svector-set! v j (Svector-ref v i))
                     (Svector-set! v i (Svector-ref v right))
                     (Svector-set! v right tmp)
                     i)))))

    (ploop (SFX- left 1) right)))

;;; Hansen's original code for this benchmark used Larceny's
;;; predefined random procedure.  When Marc Feeley modified
;;; Hansen's benchmark for the Gambit benchmark suite, however,
;;; he added a specific random number generator taken from an
;;; article in CACM.  Feeley's generator used bignums, and was
;;; extremely slow, causing the Gambit version of this benchmark
;;; to spend nearly all of its time generating the random numbers.
;;; For a benchmark called quicksort to become a bignum benchmark
;;; was very misleading, so Clinger left Feeley's version of this
;;; benchmark out of the Larceny benchmark suite.
;;;
;;; The following random number generator is much better and
;;; faster than the one used in the Gambit benchmark.  See
;;;
;;; http://srfi.schemers.org/srfi-27/mail-archive/msg00000.html
;;; http://www.math.purdue.edu/~lucier/random/random.scm

;;; A uniform [0,1] random number generator; is
;;; Pierre L'Ecuyer's generator from his paper
;;; "Good parameters and implementations for combined multiple
;;; recursive random number generators"
;;; available at his web site http://www.iro.umontreal.ca/~lecuyer

(define seed-set! #f)
(define seed-ref #f)
(define random-flonum #f)

(let ((norm 2.328306549295728e-10)
      (m1 4294967087.0)
      (m2 4294944443.0)
      (a12 1403580.0)
      (a13n 810728.0)
      (a21 527612.0)
      (a23n 1370589.0)
      (seed (vector 1.0 0.0 0.0 1.0 0.0 0.0)));; will be mutated

  ;; uses no conversions between flonums and fixnums.

  (set! random-flonum
        (lambda ()
          (let ((seed seed));; make it local
            (let ((p1 (SFL- (SFL* a12 (Svector-ref seed 1))
                            (SFL* a13n (Svector-ref seed 0))))
                  (p2 (SFL- (SFL* a21 (Svector-ref seed 5))
                            (SFL* a23n (Svector-ref seed 3)))))
              (let ((k1 (truncate (/ p1 m1)))
                    (k2 (truncate (/ p2 m2)))
                    (ignore1 (Svector-set! seed 0 (Svector-ref seed 1)))
                    (ignore3 (Svector-set! seed 3 (Svector-ref seed 4))))
                (let ((p1 (SFL- p1 (SFL* k1 m1)))
                      (p2 (SFL- p2 (SFL* k2 m2)))
                      (ignore2 (Svector-set! seed 1 (Svector-ref seed 2)))
                      (ignore4 (Svector-set! seed 4 (Svector-ref seed 5))))
                  (let ((p1 (if (SFL< p1 0.0) (+ p1 m1) p1))
                        (p2 (if (SFL< p2 0.0) (+ p2 m2) p2)))
                    (Svector-set! seed 2 p1)
                    (Svector-set! seed 5 p2)
                    (if (SFL<= p1 p2)
                        (SFL* norm (+ (SFL- p1 p2) m1))
                        (SFL* norm (SFL- p1 p2))))))))))

  (set! seed-ref (lambda () (Svector->list seed)))

  (set! seed-set! (lambda l (set! seed (Slist->vector l)))))

(define (random n)
  (SFLexact (SFLtruncate (SFL* (SFXinexact n) (random-flonum)))))

;;; Even with the improved random number generator,
;;; this benchmark still spends almost all of its time
;;; generating the random vector.  To make this a true
;;; quicksort benchmark, we generate a relatively small
;;; random vector and then sort many copies of it.

(define n 10000)
(define r 1000000)

(define input
  (let ((v (Smake-vector1 n)))
    (do ((i 0 (SFX+ i 1)))
        ((SFX= i n))
      (Svector-set! v i (random r)))
    v))

(define (run #!key (i (unknown input)) (less? (unknown (lambda (x y) (SFX< x y)))))
  (quick-1 (Svector-map2 values i) less?))

(define (check result)
  (Scall-with-current-continuation
   (lambda (return)
     (do ((i 1 (SFX+ i 1)))
         ((= i (Svector-length result))
          #t)
       (unless (SFX<= (Svector-ref result (SFX- i 1))
                      (Svector-ref result i))
         (return #f))))))
