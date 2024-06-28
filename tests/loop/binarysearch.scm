(define-keys (run !key (n (unknown 10000 1000)))

  (define vec (unknown
    '#(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20)
    '#(0 1 2 3 4 5 6 7 8 9)))
   
  (define (binarysearch vec val)
    (let loop ((len (Svector-length vec))
               (lo 0)
               (hi (SFX- (Svector-length vec) 1)))
        (if (SFX> len 1)
            (let ((mid (SFX+ lo (SFXquotient len 2))))
                (if (SFX> (Svector-ref vec mid) val)
                    (loop (SFX- hi lo) lo mid)
                    (loop (SFX- hi lo) mid hi)))
            lo)))

  (binarysearch vec (unknown 4 2)))

(define (check result)
  (equal? result (unknown 4 2)))
