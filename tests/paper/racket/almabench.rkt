;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (C) Siegfried Gonzi 2002
;;
;; a translation of the C++ version written
;; by Scott Robert Ladd.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;HELPER;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (make-matrix rows cols)
   (let ((mx (make-vector rows (make-vector 0 0.0))))
      (do ((i 0 (SFX+ i 1)))
	  ((SFX= i rows))
	  (let ((row (make-vector cols 0.0)))
	     (do ((j 0 (SFX+ j 1)))
		 ((SFX= j cols))
		 (Svector-set! row j 0.0))
	     (Svector-set! mx i row)))
      mx))

(define (flremainder x y)
  (let* ((sign (if (SFL> x 0.) 1. -1.))
         (absX (abs x))
         (absY (abs y)))
    (SFL* sign (SFL- absX (SFL* absY (floor (SFL/ absX absY)))))))

;;;;;;;;;;;;;END HELPER;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *PI*      3.14159265358979323846)
(define J2000     2451545.0)
(define JCENTURY  36525.0)
(define JMILLENIA 365250.0)
(define TWOPI     (SFL* 2.0 3.14159265358979323846))
(define A2R       (SFL/ *PI* 648000.0))
(define R2H       (SFL/ 12.0 *PI*))
(define R2D       (SFL/ 180.0 *PI*))
(define GAUSSK    0.01720209895)

;;number of days to include in test
(define TEST_LOOPS  4)
(define TEST_LENGTH (unknown 36525 30))

;;sin and cos of j2000 mean obliquity (iau 1976)
(define sineps 0.3977771559319137)
(define coseps 0.9174820620691818)

(define amas (vector 6023600.0 408523.5 328900.5 3098710.0 1047.355
3498.5 22869.0 19314.0))

;;tables giving the mean keplerian elements, limited to t**2 terms:
;;        a       semi-major axis (au)
;;        dlm     mean longitude (degree and arcsecond)
;;        e       eccentricity
;;        pi      longitude of the perihelion (degree and arcsecond)
;;        dinc    inclination (degree and arcsecond)
;;        omega   longitude of the ascending node (degree and arcsecond)
(define a (vector
    (vector 0.3870983098             0.0        0.0)
      (vector 0.7233298200             0.0        0.0)
      (vector 1.0000010178             0.0        0.0)
      (vector 1.5236793419         3e-10        0.0)
      (vector 5.2026032092     19132e-10  -39e-10)
      (vector 9.5549091915 -0.0000213896  444e-10)
      (vector 19.2184460618     -3716e-10  979e-10)
      (vector 30.1103868694    -16635e-10  686e-10)))

(define dlm (vector
    (vector 252.25090552 5381016286.88982  -1.92789)
     (vector 181.97980085 2106641364.33548   0.59381)
     (vector 100.46645683 1295977422.83429  -2.04411)
     (vector 355.43299958  689050774.93988   0.94264)
     (vector 34.35151874  109256603.77991 -30.60378)
     (vector 50.07744430   43996098.55732  75.61614)
     (vector 314.05500511   15424811.93933  -1.75083)
     (vector 304.34866548    7865503.20744   0.21103)))

(define e (vector
    (vector   0.2056317526   0.0002040653     -28349e-10 )
      (vector   0.0067719164  -0.0004776521      98127e-10 )
      (vector   0.0167086342  -0.0004203654  -0.0000126734 )
      (vector   0.0934006477   0.0009048438     -80641e-10 )
      (vector   0.0484979255   0.0016322542  -0.0000471366 )
      (vector   0.0555481426  -0.0034664062  -0.0000643639 )
      (vector   0.0463812221  -0.0002729293   0.0000078913 )
      (vector   0.0094557470   0.0000603263             0.0  )))

(define pi (vector
    (vector  77.45611904   5719.11590    -4.83016 )
      (vector 131.56370300    175.48640  -498.48184 )
      (vector 102.93734808  11612.35290    53.27577 )
      (vector 336.06023395  15980.45908   -62.32800 )
      (vector  14.33120687   7758.75163   259.95938 )
      (vector  93.05723748  20395.49439   190.25952 )
      (vector 173.00529106   3215.56238   -34.09288 )
      (vector  48.12027554   1050.71912    27.39717 ) ))

(define dinc (vector
    (vector   7.00498625  -214.25629    0.28977 )
      (vector   3.39466189   -30.84437  -11.67836 )
      (vector            0.0   469.97289   -3.35053 )
      (vector   1.84972648  -293.31722   -8.11830 )
      (vector   1.30326698   -71.55890   11.95297 )
      (vector   2.48887878    91.85195  -17.66225 )
      (vector   0.77319689   -60.72723    1.25759 )
      (vector   1.76995259     8.12333    0.08135 ) ))

(define omega (vector
    (vector  48.33089304   -4515.21727   -31.79892 )
      (vector  76.67992019  -10008.48154   -51.32614 )
      (vector 174.87317577   -8679.27034    15.34191 )
      (vector  49.55809321  -10620.90088  -230.57416 )
      (vector 100.46440702    6362.03561   326.52178 )
      (vector 113.66550252   -9240.19942   -66.23743 )
      (vector  74.00595701    2669.15033   145.93964 )
      (vector 131.78405702    -221.94322    -0.78728 )))

;;tables for trigonometric terms to be added to the mean elements
;;of the semi-major axes.
(define kp (vector
    (vector 69613.0   75645.0  88306.0  59899.0  15746.0  71087.0
142173.0   3086.0     0.0 )
      (vector 21863.0   32794.0  26934.0  10931.0  26250.0  43725.0
53867.0  28939.0     0.0 )
      (vector 16002.0   21863.0  32004.0  10931.0  14529.0  16368.0
15318.0  32794.0     0.0 )
      (vector  6345.0    7818.0  15636.0   7077.0   8184.0  14163.0
1107.0   4872.0     0.0 )
      (vector  1760.0    1454.0   1167.0    880.0    287.0   2640.0
 19.0   2047.0  1454.0 )
      (vector   574.0       0.0    880.0    287.0     19.0   1760.0
1167.0    306.0   574.0 )
      (vector   204.0       0.0    177.0   1265.0      4.0    385.0
200.0    208.0   204.0 )
      (vector     0.0     102.0    106.0      4.0     98.0   1367.0
487.0    204.0     0.0 )))

(define ca (vector
    (vector       4.0     -13.0     11.0     -9.0     -9.0     -3.0
-1.0      4.0     0.0 )
      (vector    -156.0      59.0    -42.0      6.0     19.0    -20.0
 -10.0    -12.0     0.0 )
      (vector      64.0    -152.0     62.0     -8.0     32.0    -41.0
  19.0    -11.0     0.0 )
      (vector     124.0     621.0   -145.0    208.0     54.0    -57.0
  30.0     15.0     0.0 )
      (vector  -23437.0   -2634.0   6601.0   6259.0  -1507.0  -1821.0
2620.0  -2115.0 -1489.0 )
      (vector   62911.0 -119919.0  79336.0  17814.0 -24241.0  12068.0
8306.0  -4893.0  8902.0 )
      (vector  389061.0 -262125.0 -44088.0   8387.0 -22976.0  -2093.0
-615.0  -9720.0  6633.0 )
      (vector -412235.0 -157046.0 -31430.0  37817.0  -9740.0    -13.0
-7449.0   9644.0     0.0 )))

(define sa (vector
    (vector     -29.0     -1.0      9.0      6.0     -6.0      5.0
4.0      0.0     0.0 )
      (vector     -48.0   -125.0    -26.0    -37.0     18.0    -13.0
-20.0     -2.0     0.0 )
      (vector    -150.0    -46.0     68.0     54.0     14.0     24.0
-28.0     22.0     0.0 )
      (vector    -621.0    532.0   -694.0    -20.0    192.0    -94.0
 71.0    -73.0     0.0 )
      (vector  -14614.0 -19828.0  -5869.0   1881.0  -4372.0  -2255.0
782.0    930.0   913.0 )
      (vector  139737.0      0.0  24667.0  51123.0  -5102.0   7429.0
-4095.0  -1976.0 -9566.0 )
      (vector -138081.0      0.0  37205.0 -49039.0 -41901.0 -33872.0
-27037.0 -12474.0 18797.0 )
      (vector       0.0  28492.0 133236.0  69654.0  52322.0 -49577.0
-26430.0  -3593.0     0.0 )))

;;tables giving the trigonometric terms to be added to the mean
;;elements of the mean longitudes.
(define kq (vector
    (vector  3086.0  15746.0  69613.0  59899.0  75645.0  88306.0
12661.0  2658.0   0.0    0.0 )
      (vector 21863.0  32794.0  10931.0     73.0   4387.0  26934.0
1473.0  2157.0   0.0    0.0 )
      (vector    10.0  16002.0  21863.0  10931.0   1473.0  32004.0
4387.0    73.0   0.0    0.0 )
      (vector    10.0   6345.0   7818.0   1107.0  15636.0   7077.0
8184.0   532.0  10.0    0.0 )
      (vector    19.0   1760.0   1454.0    287.0   1167.0    880.0
574.0  2640.0  19.0 1454.0 )
      (vector    19.0    574.0    287.0    306.0   1760.0     12.0
31.0    38.0  19.0  574.0 )
      (vector     4.0    204.0    177.0      8.0     31.0    200.0
1265.0   102.0   4.0  204.0 )
      (vector     4.0    102.0    106.0      8.0     98.0   1367.0
487.0   204.0   4.0  102.0 )))

(define cl (vector
    (vector      21.0    -95.0  -157.0    41.0    -5.0    42.0    23.0
  30.0      0.0     0.0 )
      (vector    -160.0   -313.0  -235.0    60.0   -74.0   -76.0   -27.0
   34.0      0.0     0.0 )
      (vector    -325.0   -322.0   -79.0   232.0   -52.0    97.0    55.0
  -41.0      0.0     0.0 )
      (vector    2268.0   -979.0   802.0   602.0  -668.0   -33.0   345.0
  201.0    -55.0     0.0 )
      (vector    7610.0  -4997.0 -7689.0 -5841.0 -2617.0  1115.0  -748.0
 -607.0   6074.0   354.0 )
      (vector  -18549.0  30125.0 20012.0  -730.0   824.0    23.0  1289.0
 -352.0 -14767.0 -2062.0 )
      (vector -135245.0 -14594.0  4197.0 -4030.0 -5630.0 -2898.0  2540.0
 -306.0   2939.0  1986.0 )
      (vector   89948.0   2103.0  8963.0  2695.0  3682.0  1648.0   866.0
 -154.0  -1963.0  -283.0 )))

(define sl (vector
    (vector   -342.0    136.0   -23.0    62.0    66.0   -52.0   -33.0
 17.0      0.0     0.0 )
      (vector    524.0   -149.0   -35.0   117.0   151.0   122.0   -71.0
  -62.0      0.0     0.0 )
      (vector   -105.0   -137.0   258.0    35.0  -116.0   -88.0  -112.0
  -80.0      0.0     0.0 )
      (vector    854.0   -205.0  -936.0  -240.0   140.0  -341.0   -97.0
 -232.0    536.0     0.0 )
      (vector -56980.0   8016.0  1012.0  1448.0 -3024.0 -3710.0   318.0
  503.0   3767.0   577.0 )
      (vector 138606.0 -13478.0 -4964.0  1441.0 -1319.0 -1482.0   427.0
 1236.0  -9167.0 -1918.0 )
      (vector  71234.0 -41116.0  5334.0 -4935.0 -1848.0    66.0   434.0
-1748.0   3780.0  -701.0 )
      (vector -47645.0  11647.0  2166.0  3194.0   679.0     0.0  -244.0
 -419.0  -2531.0    48.0 )))



;;---------------------------------------------------------------------------
;;Normalize angle into the range -pi <= A < +pi.
(define (anpm a)
  (let ((w (flremainder a TWOPI)))
    (if (SFL>= (abs w) *PI*)
    (set! w (SFL- w (if (SFL< a 0.0)
             (SFL- 0.0 TWOPI)
             TWOPI))))
w))

;;---------------------------------------------------------------------------

;; The reference frame is equatorial and is with respect to the
;;    mean equator and equinox of epoch j2000.
(define (planetpv epoch np pv)
   (let* ;; working storage
	 ((t (SFL/ (SFL+ (SFL- (Svector-ref epoch 0) J2000)
		       (Svector-ref epoch 1))
		  JMILLENIA))
	  (da (SFL+ (Svector-ref (Svector-ref a np) 0)
		   (SFL* (SFL+ (Svector-ref (Svector-ref a np) 1)
			     (SFL* t (Svector-ref (Svector-ref a np) 2)))
			t)))
	  (dl (SFL* (SFL+ (SFL* 3600.0  (Svector-ref (Svector-ref dlm np) 0))
			(SFL* t (SFL+ (Svector-ref (Svector-ref dlm np) 1)
				    (SFL* t (Svector-ref (Svector-ref dlm np) 2)))))
		   A2R))
	  (de (SFL+ (Svector-ref (Svector-ref e np) 0)
		   (SFL* t (SFL+ (Svector-ref (Svector-ref e np) 1)
			       (SFL* t (Svector-ref (Svector-ref e np) 2))))))
	  (dp (anpm (SFL* (SFL+ (SFL* 3600.0 (Svector-ref (Svector-ref pi np) 0))
			      (SFL* t (SFL+ (Svector-ref (Svector-ref pi np) 1)
					  (SFL* t (Svector-ref (Svector-ref pi np) 2)))))
			 A2R)))
	  (di (SFL* (SFL+ (SFL* 3600.0 (Svector-ref (Svector-ref dinc np) 0))
			(SFL* t (SFL+ (Svector-ref (Svector-ref dinc np) 1)
				    (SFL* t (Svector-ref (Svector-ref dinc np) 2)))))
		   A2R))
	  (doh (anpm (SFL* (SFL+ (SFL* 3600.0 (Svector-ref (Svector-ref omega
								   np) 0))
			       (SFL* t (SFL+ (Svector-ref (Svector-ref omega np) 1)
					   (SFL* t (Svector-ref (Svector-ref omega np) 2)))))
			  A2R)))
	  (dmu (SFL* 0.35953620 t)))
      ;;time: julian millennia since j2000.

      ;;compute the mean elements.
      ;;apply the trigonometric terms.
      (do ((k 0 (SFX+ k 1)))
	  ((SFX= k 8))
	  (let ((arga (SFL* dmu (Svector-ref (Svector-ref kp np) k)))
		(argl (SFL* dmu (Svector-ref (Svector-ref kq np) k))))
	     (set! da (SFL+ da (SFL* 0.0000001
				   (SFL+ (SFL* (Svector-ref (Svector-ref ca np) k)
					     (cos arga))
					(SFL* (Svector-ref (Svector-ref sa np) k)
					     (sin arga))))))
	     (set! dl (SFL+ dl (SFL* 0.0000001
				   (SFL+ (SFL* (Svector-ref (Svector-ref cl np) k)
					     (cos argl))
					(SFL* (Svector-ref (Svector-ref sl np) k)
					     (sin argl))))))))
      (let ((arga (SFL* dmu (Svector-ref (Svector-ref kp np) 8))))
	 (set! da (SFL+ da
		       (SFL* (SFL* t 0.0000001)
			    (SFL+ (SFL* (Svector-ref (Svector-ref ca np) 8)
				      (cos arga))
				 (SFL* (Svector-ref (Svector-ref sa np) 8)
				      (sin arga)))))))

      (do ((k 8 (SFX+ k 1)))
	  ((SFX= k 10))
	  (let ((argl (SFL* dmu (Svector-ref (Svector-ref kq np) k))))
	     (set! dl (SFL+ dl
			   (SFL* (SFL* t 0.0000001)
				(SFL+ (SFL* (Svector-ref (Svector-ref cl np) k)
					  (cos argl))
				     (SFL* (Svector-ref (Svector-ref sl np) k)
					  (sin argl))))))))
      (set! dl (flremainder dl TWOPI))
      ;;iterative solution of kepler's equation to get eccentric anomaly.
      (let* ((am (SFL- dl dp))
	     (ae (SFL+ am (SFL* de  (sin am)))))
	 (let loop ( (k 0.) )
	    (let ((dae (SFL/ (SFL+ (SFL- am ae) (SFL* de (sin ae)))
			    (SFL- 1.0 (SFL* de (cos ae))))))
	       (set! ae (SFL+ ae dae))
	       (if (or (SFL>= k 9.) (SFL< (abs dae) 1.0e-12))
		   'ok
		   (loop (SFL+ k 1.)) )))
	 ;      (do ((k 0 (SFL+ k 1)))
	 ;      ((or (SFL>= k 11)
	 ;           (SFL< (abs dae) 1.0e-12)))
	 ;    (set! dae (SFL/ (SFL- am (SFL+ ae (SFL* de (sin ae))))
	 ;             (SFL- 1.0 (SFL* de (cos ae)))))
	 ;    (set! ae (SFL+ ae dae)))
	 ;;true anomaly.
	 (let* ((ae2 (SFL/ ae 2.0))
		(at  (SFL* 2.0 (GENatan2 (SFL* (SFLsqrt (SFL/ (SFL+ 1.0 de)
							(SFL- 1.0 de)))
					     (sin ae2))
					(cos ae2))))
		(r (SFL* da (SFL- 1.0  (SFL* de (cos ae)))))
		(v (SFL* GAUSSK (SFLsqrt (SFL/ (SFL+ 1.0  (SFL/ 1.0 (Svector-ref amas
									 np)))
					  (SFL* da  (SFL* da  da))))))
		(si2   (sin (SFL/ di 2.0)))
		(xq    (SFL* si2  (cos doh)))
		(xp    (SFL* si2 (sin doh)))
		(tl    (SFL+ at  dp))
		(xsw   (sin tl))
		(xcw   (cos tl))
		(xm2   (SFL* 2.0 (SFL- (SFL* xp  xcw) (SFL* xq  xsw))))
		(xf    (SFL/ da (SFLsqrt (SFL- 1.0 (SFL* de de)))))
		(ci2   (cos (SFL/ di 2.0)))
		(xms   (SFL* (SFL+ (SFL* de (sin dp))  xsw)
			    xf))
		(xmc   (SFL* (SFL+ (SFL* de  (cos dp)) xcw)  xf))
		(xpxq2 (SFL* 2.0  (SFL* xp  xq)))
		;;position (j2000 ecliptic x,y,z in au).
		(x (SFL* r (SFL- xcw (SFL* xm2  xp))))
		(y (SFL* r (SFL+ xsw (SFL* xm2  xq))))
		(z (SFL* r (SFL* (SFL- 0.0 xm2) ci2))))
	    ;;rotate to equatorial.
	    (Svector-set! (Svector-ref pv 0) 0 x)
	    (Svector-set! (Svector-ref pv 0) 1 (SFL- (SFL* y coseps) (SFL* z sineps)))
	    (Svector-set! (Svector-ref pv 0) 2 (SFL+ (SFL* y sineps) (SFL* z coseps)))
	    ;;velocity (j2000 ecliptic xdot,ydot,zdot in au/d).
	    (set! x (SFL* v (SFL+ (SFL* xms (SFL+ -1.0 (SFL* 2.0 (SFL* xp xp))))
				(SFL* xpxq2 xmc))))
	    (set! y (SFL* v (SFL- (SFL* xmc (SFL- 1.0 (SFL* 2.0 (SFL* xq xq))))
				(SFL* xms xpxq2))))
	    (set! z (SFL* v (SFL* (SFL* 2.0 ci2) (SFL+ (SFL* xp xms) (SFL* xq xmc)))))
	    ;;rotate to equatorial.
	    (Svector-set! (Svector-ref pv 1) 0  x)
	    (Svector-set! (Svector-ref pv 1) 1 (SFL- (SFL* y coseps) (SFL* z sineps)))
	    (Svector-set! (Svector-ref pv 1) 2 (SFL+ (SFL* y sineps) (SFL* z
								      coseps)))))))


;;---------------------------------------------------------------------------
;; Computes RA, Declination, and distance from a state vector returned by
;; planetpv.
(define (radecdist state  rdd)
  ;;distance
  (Svector-set! rdd 2 (SFLsqrt (SFL+ (SFL* (Svector-ref (Svector-ref state 0) 0)
                 (Svector-ref (Svector-ref state 0) 0))
                (SFL+ (SFL* (Svector-ref (Svector-ref state 0) 1)
                      (Svector-ref (Svector-ref state 0) 1))
                     (SFL* (Svector-ref (Svector-ref state 0) 2)
                      (Svector-ref (Svector-ref state 0) 2))))))
  ;;RA
  (Svector-set! rdd 0 (SFL* (GENatan2 (Svector-ref (Svector-ref state 0) 1)
			      (Svector-ref (Svector-ref state 0) 0))
               R2H))
  (if (SFL< (Svector-ref rdd 0) 0.0)
      (Svector-set! rdd 0 (SFL+ 24.0 (Svector-ref rdd 0))))
  ;;Declination
  (Svector-set! rdd 1 (SFL* (asin (SFL/ (Svector-ref (Svector-ref state 0) 2)
                 (Svector-ref rdd 2))) R2D)))

;;---------------------------------------------------------------------------
;;Entry point
;;Calculate RA and Dec for noon on every day in 1900-2100
(define-keys (run !key (K TEST_LENGTH))
   (let* ((jd (make-vector 2 0.0))
	  (pv (make-matrix 2 3))
	  (position (make-vector 3 0.0)))
      ;;#pragma omp parallel for private(i, n, p, jd, pv, position)
      (do ((i 0 (SFX+ i 1)))
	  ((SFX= i TEST_LOOPS))
	  (Svector-set! jd 0 J2000)
	  (Svector-set! jd 1 0.0)
	  (do ((n 0 (SFX+ n 1)))
	      ((SFX= n K))
	      (Svector-set! jd 0 (SFL+ (Svector-ref jd 0) 1.0))
	      ;;#pragma omp critical
	      ;;(print "JD = " (vector-ref jd 0)  newline)
	      (do ((p 0 (SFX+ p 1)))
		  ((SFX= p 8))
		  (planetpv jd p pv)
		  (radecdist pv position))))
      ;;#pragma omp critical
      (list position jd pv)))

(define (check result)
   (and (= (length result) 3)
	(vector? (car result))
	(= (vector-length (car result)) 3)
	(vector? (cadr result))
	(= (vector-length (cadr result)) 2)
	(zero? (vector-ref (cadr result) 1))
	(vector? (caddr result))
	(= (vector-length (cadr result)) 2)))
