(set! eval
  (let ((eval eval))
    (lambda (x)
      (let ((x (if (and (pair? x)
			(string? (car x))
			(string=? (car x) "noexpand"))
		   (cadr x)
		   x)))
	(compiler$visit-time (cons x (compiler$visit-time)))
	(eval x)))))

(define (make-gensym-session-key)
  (string-append "sessionkey-"
		 (number->string (current-seconds))
		 "-"
		 (number->string (current-milliseconds))
		 "-"))

(define $sc-put-cte 'forward)
