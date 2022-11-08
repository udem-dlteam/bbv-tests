;;; MAZEFUN -- Constructs a maze in a purely functional way,
;;; written by Marc Feeley.

;;(import (scheme base) (scheme read) (scheme write) (scheme time))

(define foldr
  (lambda (f base lst)
    (define foldr-aux
      (lambda (lst)
        (if (null? lst)
            base
            (f (Scar lst) (foldr-aux (Scdr lst))))))

    (foldr-aux lst)))

(define foldl
  (lambda (f base lst)
    (define foldl-aux
      (lambda (base lst)
        (if (null? lst)
            base
            (foldl-aux (f base (Scar lst)) (Scdr lst)))))

    (foldl-aux base lst)))

(define for
  (lambda (lo hi f)
    (define for-aux
      (lambda (lo)
        (if (SFX< lo hi)
            (cons (f lo) (for-aux (SFX+ lo 1)))
            '())))

    (for-aux lo)))

(define concat
  (lambda (lists)
    (foldr append '() lists)))

(define list-read
  (lambda (lst i)
    (if (SFX= i 0)
        (Scar lst)
        (list-read (Scdr lst) (SFX- i 1)))))

(define list-write
  (lambda (lst i val)
    (if (SFX= i 0)
        (cons val (Scdr lst))
        (cons (Scar lst) (list-write (Scdr lst) (SFX- i 1) val)))))

(define list-remove-pos
  (lambda (lst i)
    (if (SFX= i 0)
        (Scdr lst)
        (cons (Scar lst) (list-remove-pos (Scdr lst) (SFX- i 1))))))

(define duplicates?
  (lambda (lst)
    (if (null? lst)
        #f
        (or (Smember (Scar lst) (Scdr lst))
            (duplicates? (Scdr lst))))))

(define make-matrix
  (lambda (n m init)
    (for 0 n (lambda (i) (for 0 m (lambda (j) (init i j)))))))

(define matrix-read
  (lambda (mat i j)
    (list-read (list-read mat i) j)))

(define matrix-write
  (lambda (mat i j val)
    (list-write mat i (list-write (list-read mat i) j val))))

(define matrix-size
  (lambda (mat)
    (cons (Slength mat) (Slength (Scar mat)))))

(define matrix-map
  (lambda (f mat)
    (Smap2 (lambda (lst) (Smap2 f lst)) mat)))

(define initial-random 0)

(define next-random-number
  (lambda (current-random)
    (SFXremainder (SFX+ (SFX* current-random 3581) 12751) 131072)))

(define shuffle
  (lambda (lst)
    (shuffle-aux lst initial-random)))

(define shuffle-aux
  (lambda (lst current-random)
    (if (null? lst)
        '()
        (let ((new-random (next-random-number current-random)))
          (let ((i (SFXremainder new-random (Slength lst))))
            (cons (list-read lst i)
                  (shuffle-aux (list-remove-pos lst i)
                               new-random)))))))

(define make-maze
  (lambda (n m) ; n and m must be odd
    (if (not (and (SFXodd? n) (SFXodd? m)))
        'error
        (let ((cave
               (make-matrix n m (lambda (i j)
                                  (if (and (SFXeven? i) (SFXeven? j))
                                      (cons i j)
                                      #f))))
              (possible-holes
               (concat
                (for 0 n (lambda (i)
                           (concat
                            (for 0 m (lambda (j)
                                       (if (equal? (SFXeven? i) (SFXeven? j))
                                           '()
                                           (list (cons i j)))))))))))
          (cave-to-maze (pierce-randomly (shuffle possible-holes) cave))))))

(define cave-to-maze
  (lambda (cave)
    (matrix-map (lambda (x) (if x '_ '*)) cave)))

(define pierce
  (lambda (pos cave)
    (let ((i (Scar pos)) (j (Scdr pos)))
      (matrix-write cave i j pos))))

(define pierce-randomly
  (lambda (possible-holes cave)
    (if (null? possible-holes)
        cave
        (let ((hole (Scar possible-holes)))
          (pierce-randomly (Scdr possible-holes)
                           (try-to-pierce hole cave))))))

(define try-to-pierce
  (lambda (pos cave)
    (let ((i (Scar pos)) (j (Scdr pos)))
      (let ((ncs (neighboring-cavities pos cave)))
        (if (duplicates?
             (Smap2 (lambda (nc) (matrix-read cave (Scar nc) (Scdr nc))) ncs))
            cave
            (pierce pos
                    (foldl (lambda (c nc) (change-cavity c nc pos))
                           cave
                           ncs)))))))

(define change-cavity
  (lambda (cave pos new-cavity-id)
    (let ((i (Scar pos)) (j (Scdr pos)))
      (change-cavity-aux cave pos new-cavity-id (matrix-read cave i j)))))

(define change-cavity-aux
  (lambda (cave pos new-cavity-id old-cavity-id)
    (let ((i (Scar pos)) (j (Scdr pos)))
      (let ((cavity-id (matrix-read cave i j)))
        (if (equal? cavity-id old-cavity-id)
            (foldl (lambda (c nc)
                     (change-cavity-aux c nc new-cavity-id old-cavity-id))
                   (matrix-write cave i j new-cavity-id)
                   (neighboring-cavities pos cave))
            cave)))))

(define neighboring-cavities
  (lambda (pos cave)
    (let ((size (matrix-size cave)))
      (let ((n (Scar size)) (m (Scdr size)))
        (let ((i (Scar pos)) (j (Scdr pos)))
          (append (if (and (SFX> i 0) (matrix-read cave (SFX- i 1) j))
                      (list (cons (SFX- i 1) j))
                      '())
                  (if (and (SFX< i (SFX- n 1)) (matrix-read cave (SFX+ i 1) j))
                      (list (cons (SFX+ i 1) j))
                      '())
                  (if (and (SFX> j 0) (matrix-read cave i (SFX- j 1)))
                      (list (cons i (SFX- j 1)))
                      '())
                  (if (and (SFX< j (SFX- m 1)) (matrix-read cave i (SFX+ j 1)))
                      (list (cons i (SFX+ j 1)))
                      '())))))))

(define (run #!key (n (unknown 11 5)) (m (unknown 11 5)))
  (make-maze n m))

(define (check result)
  (equal? result
          '((_ * _ _ _ _ _ _ _ _ _)
            (_ * * * * * * * _ * *)
            (_ _ _ * _ _ _ * _ _ _)
            (_ * _ * _ * _ * _ * _)
            (_ * _ _ _ * _ * _ * _)
            (* * _ * * * * * _ * _)
            (_ * _ _ _ _ _ _ _ * _)
            (_ * _ * _ * * * * * *)
            (_ _ _ * _ _ _ _ _ _ _)
            (_ * * * * * * * _ * *)
            (_ * _ _ _ _ _ _ _ _ _))))
