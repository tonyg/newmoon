(define (flatten-body body)
  (cond
   ((null? body) '())
   ((and (list? (car body))
	 (eq? (caar body) 'begin))
    (append (flatten-body (cdar body))
	    (flatten-body (cdr body))))
   (else (cons (car body) (flatten-body (cdr body))))))

(define (flatten-toplevel-begins expr)
  (if (and (pair? expr)
	   (eq? (car expr) 'begin))
      (cons 'begin (flatten-body (cdr expr)))
      expr))

(define (rewrite-toplevel-defines expr)
  (if (pair? expr)
      (case (car expr)
	((begin)	(cons 'begin (map rewrite-toplevel-defines (cdr expr))))
	((define)	(let ((decl (cadr expr))
			      (body (cddr expr)))
			  (if (pair? decl)
			      `(#%define-global-variable ',(car decl)
							 (lambda ,(cdr decl)
							   ,@body))
			      `(#%define-global-variable ',decl
							 ,@body))))
	(else		expr))
      expr))
