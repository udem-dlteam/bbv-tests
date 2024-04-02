;*=====================================================================*/
;*    serrano/prgm/project/bbv-tests/tests/paper/micro/bague.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Pierre Weis                                       */
;*    Creation    :  Fri Apr  1 10:00:21 1994                          */
;*    Last change :  Mon Nov 14 11:52:08 2022 (serrano)                */
;*    -------------------------------------------------------------    */
;*    Resolution recursive du Baguenaudier: bench les appels de        */
;*    fonctions et les acces aux vecteurs                              */
;*    avec 21 pierres le nombre de coups est 1398101                   */
;*    avec 24 pierres le nombre de coups est 11184810                  */
;*    f (n+1) = 2*f(n) + n mod 2 avec f 1 = 1                          */
;*=====================================================================*/

(define nombre-de-coups 0)
(define nombre-de-pierres 28)

(define une-pierre 1)
(define une-case-vide 0)

(define jeu (Smake-vector2 nombre-de-pierres une-pierre))

(define (init-jeu)
   (set! nombre-de-coups 0)
   (let loop ((i (SFX- nombre-de-pierres 1)))
      (if (SFX< i 0)
	  'done
	  (begin
	     (Svector-set! jeu i une-pierre)
	     (loop (SFX- i 1))))))

(define (la-case n)
   (SFX- n 1))

(define (enleve-la-pierre n)
   (if (eq? (Svector-ref jeu (la-case n))  une-pierre)
       (Svector-set! jeu (la-case n) une-case-vide)
       (error "bague" "cannot remove a stone from an empty slot" n)))

(define (pose-la-pierre n)
   (if (eq? (Svector-ref jeu (la-case n)) une-case-vide)
       (Svector-set! jeu (la-case n) une-pierre)
       (error "bague" "cannot lay a stone on a non empty slot" n)))

(define (autorise-mouvement n)
   (case n
      ((1) #t)
      ((2) (eq? (Svector-ref jeu (la-case 1)) une-pierre))
      (else
       (and (eq? (Svector-ref jeu (la-case (SFX- n 1))) une-pierre)
	    (letrec ((ok (lambda (b i)
			    (if (SFX> i (la-case (SFX- n 2)))
				b
				(ok (and b (eq? (Svector-ref jeu i)
						une-case-vide))
				    (SFX+ i 1))))))
	       (ok #t 0))))))

(define (enleve-pierre n)
   (set! nombre-de-coups (SFX+ nombre-de-coups 1))
   (if (autorise-mouvement n)
       (enleve-la-pierre n)
       (error "bague" "forbidden action" n)))

(define (pose-pierre n)
   (set! nombre-de-coups (SFX+ nombre-de-coups 1))
   (if (autorise-mouvement n)
       (pose-la-pierre n)
       (error "bague" "forbidden action" n)))

(define (bague n)
   (case n
      ((1) (enleve-pierre 1))
      ((2) (enleve-pierre 2)
       (enleve-pierre 1))
      (else
       (bague (SFX- n 2))
       (enleve-pierre n)
       (repose (SFX- n 2))
       (bague (SFX- n 1)))))

(define (repose n)
   (case n
      ((1) (pose-pierre 1))
      ((2) (pose-pierre 1)
       (pose-pierre 2))
      (else
       (repose (SFX- n 1))
       (bague (SFX- n 2))
       (pose-pierre n)
       (repose (SFX- n 2)))))

(define-keys (run !key (nombre-de-pierres (unknown 28 10)))
   (init-jeu)
   (bague nombre-de-pierres)
   (cons nombre-de-pierres nombre-de-coups))

(define (check result)
   (eq? (cdr result)
      (case (car result)
	 ((1) 1)
	 ((2) 2)
	 ((10) 682)
	 ((14) 10922)
	 ((20) 699050)
	 ((24) 11184810)
	 ((25) 22369621)
	 ((26) 44739242)
	 ((27) 89478485)
	 ((28) 178956970)
	 (else #f))))
