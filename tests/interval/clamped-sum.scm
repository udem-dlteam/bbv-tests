(define (clamped-sum X Y Z)
  (let loop ((x X) (y Y) (z Z))
    (cond
      ((fx< x 10)
        (loop 11 y z))
      ((fx< y 20)
        (loop x 21 z))
      ((fx< z 30)
        (loop x y 31))
      (else
        (fx+ x y z)))))
