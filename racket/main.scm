(define (exec-bench)
  (let loop1 ((args (map (lambda (s) (read (open-input-string s)))
                         (vector->list (current-command-line-arguments))))
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
            (error 'exec-bench "invalid arguments")
            (let ((args (reverse rev-args)))
              (let loop2 ((repeat repeat) (result (mcons 0 '())));;;;;;;changé
                (define (deep-apply align-stack run args)
                  (if (> align-stack 0)
                      (unknown (deep-apply (- align-stack 1) run args))
                      (if (pair? args) (apply run args) (run))))
                (if (> repeat 0)
                    (loop2 (- repeat 1) (mcons (deep-apply align-stack run args) result));;;;;;;changé
                    (if (and (null? args) (not (check result)))
                        (error 'wrong-result result)
                        (mcar result)))))))));;;;;;;changé

(exec-bench)
