;;; NQUEENS -- Compute number of solutions to 8-queens problem.

;;(import (scheme base) (scheme read) (scheme write) (scheme time))

(define trace? #f)

(define (nqueens n)

  (define (iota1 n)
    (let loop ((i n) (l '()))
      (if (SFX= i 0) l (loop (SFX- i 1) (cons i l)))))

  (define (my-try x y z)
    (if (null? x)
        (if (null? y)
            (begin
              (when trace? (begin (write z) (newline)))
              1)
            0)
        (SFX+ (if (ok? (Scar x) 1 z)
                  (my-try (Sappend (Scdr x) y) '() (cons (Scar x) z))
                  0)
              (my-try (Scdr x) (cons (Scar x) y) z))))

  (define (ok? row dist placed)
    (if (null? placed)
        #t
        (and (not (SFX= (Scar placed) (SFX+ row dist)))
             (not (SFX= (Scar placed) (SFX- row dist)))
             (ok? row (SFX+ dist 1) (Scdr placed)))))

  (my-try (iota1 n) '() '()))

(define (run #!key (n (unknown 13 8)))
  (nqueens n))

(define (check result)
  (equal? result 73712))
