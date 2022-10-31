(define (run #!key (x (unknown 18)) (y (unknown 12)) (z (unknown 6)))
   
  (define (tak x y z)
    (if (not (GFX< y x))
        z
        (tak (tak (GFX- x 1) y z)
             (tak (GFX- y 1) z x)
             (tak (GFX- z 1) x y))))

  (tak x y z))
