(define-keys (run !key (x (unknown 18)) (y (unknown 12)) (z (unknown 6)))
   
  (define (tak x y z)
    (if (not (SFX< y x))
        z
        (tak (tak (SFX- x 1) y z)
             (tak (SFX- y 1) z x)
             (tak (SFX- z 1) x y))))

  (tak x y z))

(define (check result)
  (equal? result 7))
