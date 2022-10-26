(let ((args
       (map (lambda (arg)
              (call-with-input-string arg read))
            (cdr (command-line)))))
  (apply run args))
