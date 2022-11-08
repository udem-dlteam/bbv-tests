;;; TRIANGL -- Board game benchmark.

;;(import (scheme base) (scheme read) (scheme write) (scheme time))

(define *board*
  (Slist->vector '(1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1)))

(define *sequence*
  (Slist->vector '(0 0 0 0 0 0 0 0 0 0 0 0 0 0)))

(define *a*
  (Slist->vector '(1 2 4 3 5 6 1 3 6 2 5 4 11 12
                     13 7 8 4 4 7 11 8 12 13 6 10
                     15 9 14 13 13 14 15 9 10
                     6 6)))

(define *b*
  (Slist->vector '(2 4 7 5 8 9 3 6 10 5 9 8
                     12 13 14 8 9 5 2 4 7 5 8
                     9 3 6 10 5 9 8 12 13 14
                     8 9 5 5)))

(define *c*
  (Slist->vector '(4 7 11 8 12 13 6 10 15 9 14 13
                     13 14 15 9 10 6 1 2 4 3 5 6 1
                     3 6 2 5 4 11 12 13 7 8 4 4)))

(define *answer* '())

(define (attempt i depth)
  (cond ((SFX= depth 14)
         (set! *answer*
               (cons (Scdr (Svector->list *sequence*)) *answer*))
         #t)
        ((and (SFX= 1 (Svector-ref *board* (Svector-ref *a* i)))
              (SFX= 1 (Svector-ref *board* (Svector-ref *b* i)))
              (SFX= 0 (Svector-ref *board* (Svector-ref *c* i))))
         (Svector-set! *board* (Svector-ref *a* i) 0)
         (Svector-set! *board* (Svector-ref *b* i) 0)
         (Svector-set! *board* (Svector-ref *c* i) 1)
         (Svector-set! *sequence* depth i)
         (do ((j 0 (SFX+ j 1))
              (depth (SFX+ depth 1)))
             ((or (SFX= j 36) (attempt j depth)) #f))
         (Svector-set! *board* (Svector-ref *a* i) 1)
         (Svector-set! *board* (Svector-ref *b* i) 1)
         (Svector-set! *board* (Svector-ref *c* i) 0) #f)
        (else #f)))

(define (test i depth)
  (set! *answer* '())
  (attempt i depth)
  (Scar *answer*))

(define (run #!key (i (unknown 22 22)) (depth (unknown 1 7)))
  (test i depth))

(define (check result)
  (equal? result '(22 34 31 15 7 1 20 17 25 6 5 13 32)))
