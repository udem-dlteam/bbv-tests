(define (program unknown1)
  (if (u8vector? unknown1)
      (let ((byte (u8vector-ref unknown1 0)))
        (identity byte))))
