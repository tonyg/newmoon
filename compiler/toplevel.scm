(define (flatten-body body)
  (if (null? body)
      '()
      (node-match (car body)
		  ((begin exprs) (append (flatten-body exprs)
					 (flatten-body (cdr body))))
		  (else (cons (car body) (flatten-body (cdr body)))))))

(define (flatten-begins expr)
  (node-match expr
	      ((begin exprs) (make-node 'begin 'exprs (flatten-body exprs)))
	      (else expr)))

(define (rewrite-toplevel-defines expr)
  (node-match expr
	      ((begin exprs) (make-node 'begin 'exprs (map rewrite-toplevel-defines exprs)))
	      ((define name expr) (make-node 'apply
					     'rator (make-node 'var 'name '%define-global-variable)
					     'rands (list (make-node 'lit 'value name)
							  expr)))
	      (else expr)))
