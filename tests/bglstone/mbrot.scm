(define *r -1.0)
(define *i -0.5)
(define *n 75)
(define *step 0.005)

(define (_write-char c)
   c)

(define (_newline)
   #\n)

(define (count cr ci)
   (let ((max-count 64)
         (radius^2  16.0))
      (let loop ((c 0)
                 (zr cr)
                 (zi ci))
         (if (SFX= c max-count)
             c
             (let ((zr^2 (SFL* zr zr))
                   (zi^2 (SFL* zi zi)))
                (if (SFL> (SFL+ zr^2 zi^2) radius^2)
                    c
                    (loop (SFX+ c 1)
                          (SFL+ (SFL- zr^2 zi^2) cr)
                          (SFL+ (SFL* 2.0 (SFL* zr zi)) ci))))))))

(define (mbrot r i n step p?)
  (let loop1 ((y n) (ci i))
    (if (SFX> y 0)
      (let loop2 ((x n) (cr r))
        (if (SFX> x 0)
          (let ((c (count cr ci)))
             (if p? (_write-char (integer->char (SFX+ c 32))))
            (loop2 (SFX- x 1) (SFL+ cr step)))
          (begin
             (if p?
                 (_newline))
            (loop1 (SFX- y 1) (SFL+ ci step))))))))

(define (run #!key (num (unknown 10 2)))
   (let loop ((num num)
              (res (mbrot *r *i *n *step (SFX= num 1))))
      (if (SFX= num 1)
          'done
          (loop (SFX- num 1) (mbrot *r *i *n *step (SFX= num 2))))))

(define (check result)
   (eq? result 'done))
