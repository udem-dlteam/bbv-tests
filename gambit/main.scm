(define (exec-bench)
  (let loop1 ((args (map (lambda (arg) (call-with-input-string arg read))
                        (cdr (command-line))))
              (rev-args '())
              (repeat 1)
              (align-stack 0))
    (if (and (pair? args) (pair? (cdr args)))
        (let ((arg (car args)))
          (cond ((eq? arg 'repeat:)
                (loop1 (cddr args) rev-args (cadr args) align-stack))
                ((eq? arg 'align-stack:)
                (loop1 (cddr args) rev-args repeat (cadr args)))
                (else
                (loop1 (cddr args) (cons (cadr args) (cons (car args) rev-args)) repeat align-stack))))
        (if (pair? args)
            (error "invalid arguments")
            (let ((args (reverse rev-args)))
              (let loop2 ((repeat repeat) (result #f))
                (define (deep-apply align-stack run args)
                  (if (> align-stack 0)
                      (##first-argument (deep-apply (- align-stack 1) run args))
                      (if (pair? args) (apply run args) (run))))
                (if (> repeat 0)
                    (loop2 (- repeat 1) (deep-apply align-stack run args))
                    (if (and (null? args) (not (check result)))
                        (error "wrong result =" result)
                        result))))))))

(let ((result (##exec-stats exec-bench)))
  (for-each (lambda (data) (pp (list (car data) (cdr data)))) result))
