;; primes.scm benchmark from r7rs-benchmarks

(define (run #!key (n (unknown 100)))

  (define  (interval-list m n)
    (if (SFX> m n)
        '()
        (cons m (interval-list (SFX+ 1 m) n))))

  (define (sieve l)
    (letrec ((remove-multiples
              (lambda (n l)
                (if (null? l)
                    '()
                    (if (SFX= (remainder (car l) n) 0)
                        (remove-multiples n (cdr l))
                        (cons (car l)
                              (remove-multiples n (cdr l))))))))
      (if (null? l)
          '()
          (cons (car l)
                (sieve (remove-multiples (car l) (cdr l)))))))

  (define (primes<= n)
    (sieve (interval-list 2 n)))

  (primes<= n))

(define (check result)
  (equal?
    result
    '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97)))
