(define (flatten-body body)
  (if (null? body)
      '()
      (node-match (car body)
	((@core-begin exprs) (append (flatten-body exprs)
				     (flatten-body (cdr body))))
	(else (cons (car body) (flatten-body (cdr body)))))))

(define (flatten-begins expr)
  (node-match expr
    ((@core-begin exprs)
     (make-node @core-begin (flatten-body exprs)))
    (else expr)))

(define (rewrite-toplevel-defines expr)
  (node-match expr
    ((@core-begin exprs) (make-node @core-begin (map rewrite-toplevel-defines exprs)))
    ((@core-define name expr) (make-node @core-apply
					 (make-node @core-var '$define-global-variable)
					 (list (make-node @core-lit name)
					       expr)))
    (else expr)))

(define (wrap-outer-lambda node)
  (make-node @core-lambda '() #f (list node)))
