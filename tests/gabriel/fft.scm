;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File:         fft.sc
;;; Description:  FFT benchmark from the Gabriel tests.
;;; Author:       Harry Barrow
;;; Created:      8-Apr-85
;;; Modified:     6-May-85 09:29:22 (Bob Shaw)
;;;               11-Aug-87 (Will Clinger)
;;;               16-Nov-94 (Qobi)
;;;               31-Mar-98 (Qobi)
;;; Language:     Scheme
;;; Status:       Public Domain
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define pi (atan 0 -1))

;;; FFT -- This is an FFT benchmark written by Harry Barrow.
;;; It tests a variety of floating point operations,
;;; including array references.

(define *re* (make-vector 1025 0.0))

(define *im* (make-vector 1025 0.0))

(define (fft areal aimag)
 (let ((ar areal)			;Qobi
       (ai aimag)			;Qobi
       (i 0)
       (j 0)
       (k 0)
       (m 0)
       (n 0)
       (le 0)
       (le1 0)
       (ip 0)
       (nv2 0)
       (nm1 0)
       (ur 0.0)				;Qobi
       (ui 0.0)				;Qobi
       (wr 0.0)				;Qobi
       (wi 0.0)				;Qobi
       (tr 0.0)				;Qobi
       (ti 0.0))			;Qobi
  ;; initialize
  (set! ar areal)
  (set! ai aimag)
  (set! n (vector-length ar))
  (set! n (SFX- n 1))
  (set! nv2 (quotient n 2))
  (set! nm1 (SFX- n 1))
  (set! m 0)				;compute m = log(n)
  (set! i 1)
  (let loop ()
   (if (SFX< i n)
       (begin (set! m (SFX+ m 1))
	      (set! i (SFX+ i i))
	      (loop))))
  (cond ((not (SFX= n (let loop ((i m) (p 1)) ;Qobi
		    (if (SFXzero? i) p (loop (SFX- i 1) (SFX* 2 p))))))
	 (display "array size not a power of two.")
	 (newline)))
  ;; interchange elements in bit-reversed order
  (set! j 1)
  (set! i 1)
  (let l3 ()
   (cond ((SFX< i j)
	  (set! tr (vector-ref ar j))
	  (set! ti (vector-ref ai j))
	  (vector-set! ar j (vector-ref ar i))
	  (vector-set! ai j (vector-ref ai i))
	  (vector-set! ar i tr)
	  (vector-set! ai i ti)))
   (set! k nv2)
   (let l6 ()
    (cond ((SFX< k j)
	   (set! j (SFX- j k))
	   (set! k (quotient k 2))	;Qobi: was / but this violates R4RS
	   (l6))))
   (set! j (SFX+ j k))
   (set! i (SFX+ i 1))
   (cond ((SFX< i n) (l3))))
  ;; loop thru stages (syntax converted from old MACLISP style \bs)
  (do ((l 1 (SFX+ l 1))) ((SFX> l m))
   (set! le (let loop ((i l) (p 1))	;Qobi
	     (if (SFXzero? i) p (loop (SFX- i 1) (SFX* 2 p)))))
   (set! le1 (quotient le 2))
   (set! ur 1.0)
   (set! ui 0.0)
   (set! wr (cos (SFL/ pi le1)))
   (set! wi (sin (SFL/ pi le1)))
   ;; loop thru butterflies
   (do ((j 1 (SFX+ j 1))) ((SFX> j le1))
    ;; do a butterfly
    (do ((i j (SFX+ i le))) ((SFX> i n))
     (set! ip (SFX+ i le1))
     (set! tr (SFL- (SFL* (vector-ref ar ip) ur) (SFL* (vector-ref ai ip) ui)))
     (set! ti (SFL+ (SFL* (vector-ref ar ip) ui) (SFL* (vector-ref ai ip) ur)))
     (vector-set! ar ip (SFL- (vector-ref ar i) tr))
     (vector-set! ai ip (SFL- (vector-ref ai i) ti))
     (vector-set! ar i (SFL+ (vector-ref ar i) tr))
     (vector-set! ai i (SFL+ (vector-ref ai i) ti))))
   (set! tr (SFL- (SFL* ur wr) (SFL* ui wi)))
   (set! ti (SFL+ (SFL* ur wi) (SFL* ui wr)))
   (set! ur tr)
   (set! ui ti))
  #t))

;;; the timer which does 10 calls on fft

(define (run)
   (let ((res #f))
      (do ((ntimes 0 (SFX+ ntimes 1))) ((SFX= ntimes 10))
	  (set! res (fft *re* *im*)))
      res))

(define (check result)
   (eq? result #t))




