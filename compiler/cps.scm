(define (head-exprs expr)
  (node-collect-subnodes expr (case (node-kind expr)
				((lit singleton var lambda) '())
				((begin) '(head))
				((apply) '(rator . rands))
				((if) '(test))
				((set) '(value))
				((jvm-assemble) 'actuals)
				(else (error "internal-compiler-error"
					     "unknown node kind in head-exprs:"
					     expr)))))

(define (tail-exprs expr)
  (node-collect-subnodes expr (case (node-kind expr)
				((lit singleton var lambda apply set jvm-assemble) '())
				((begin) '(tail))
				((if) '(true false))
				(else (error "internal-compiler-error"
					     "unknown node kind in tail-exprs:"
					     expr)))))

(define (simple? expr)
  (and (not (memq (node-kind expr)
		  '(apply if)))
       (every simple? (head-exprs expr))
       (every simple? (tail-exprs expr))))

(define (initial-expr expr)
  (let loop ((heads (head-exprs expr)))
    (cond
     ((null? heads) expr)	  ; expr is its own initial expression
     ((simple? (car heads))
      (loop (cdr heads)))      ; this head-expr is not an initial-expr
     (else
      (initial-expr (car heads)))))) ; this head-expr may be an initial-expr

(define (subst-expr expr what with)
  (cond
   ((eq? what with)
    expr)
   ((eq? expr what)
    with)
   (else
    (let loop-expr ((expr expr))
      (let loop-subs ((subs (node-child-attr-names expr)))
	(cond
	 ((null? subs))
	 ((pair? subs)
	  (let ((subexpr (node-get expr (node-kind expr) (car subs))))
	    (if (eq? subexpr what)
		(node-set! expr (node-kind expr) (car subs) with)
		(loop-expr subexpr)))
	  (loop-subs (cdr subs)))
	 (else
	  (let loop-tail ((tail (node-get expr (node-kind expr) subs)))
	    (if (pair? tail)
		(begin
		  (if (eq? (car tail) what)
		      (set-car! tail with)
		      (loop-expr (car tail)))
		  (loop-tail (cdr tail)))))))))
    expr)))

(define (gen-cont-sym) (gensym "CONT"))
(define (gen-val-sym) (gensym "VAL"))

(define (make-one-armed-lambda argsyms bodynode)
  (let ((arginfos (map (lambda (argsym) (make-arginfo argsym #f)) argsyms)))
    (make-lambda (list arginfos)
		 (list #f)
		 (list bodynode))))

(define (cps-pushing-transform cont-arg node)
  (cond
   ((simple? node)
    (make-apply (make-var cont-arg #f)
		(list (cps-transform node))))
   (else
    (let ((ie (initial-expr node)))
      (node-match ie
		  ((begin head tail)
		   (compiler-assert begin-is-never-another-exprs-head-expr (eq? ie node) ie node)
		   (make-begin (cps-transform head)
			       (cps-pushing-transform cont-arg tail)))
		  ((apply rator rands)
		   (let ((make-call-cps (lambda (the-cont-node)
					  (make-apply (cps-transform rator)
						      (cons the-cont-node
							    (map cps-transform rands))))))
		     (if (eq? ie node)	; if node is its own init-expr
			 (make-call-cps (make-var cont-arg #f))
			 (make-call-cps (let ((cont-val (gen-val-sym)))
					  (make-one-armed-lambda
					   (list cont-val)
					   (cps-pushing-transform
					    cont-arg
					    (subst-expr node ie (make-var cont-val #f)))))))))
		  ((if test true false)
		   (let ((make-cps-if (lambda (the-cont-var)
					(make-if (cps-transform test)
						 (cps-pushing-transform the-cont-var true)
						 (cps-pushing-transform the-cont-var false)))))
		     (if (eq? ie node)
			 (make-cps-if cont-arg)
			 (let ((the-cont-var (gen-cont-sym))
			       (the-cont-arg (gen-val-sym)))
			   (make-apply (make-one-armed-lambda (list the-cont-var)
							      (make-cps-if the-cont-var))
				       (list (make-one-armed-lambda
					      (list the-cont-arg)
					      (cps-pushing-transform
					       cont-arg
					       (subst-expr node
							   ie
							   (make-var the-cont-arg #f))))))))))
		  ((jvm-assemble formals actuals code)
		   (compiler-assert jvm-assemble-is-always-its-own-head-expr (eq? ie node))
		   (make-jvm-assemble formals
				      (map cps-transform actuals)
				      code))
		  (else
		   (error "internal-compiler-error"
			  "invalid initial-expression node-kind; node =" node)))))))

(define (cps-transform node)
  (node-match node
	      ((lambda all-arginfos all-bodies)
	       (let ((cont-arg (gen-cont-sym)))
		 (map! (lambda (arginfos) (cons (make-arginfo cont-arg #f) arginfos)) all-arginfos)
		 (map! (lambda (body) (cps-pushing-transform cont-arg body)) all-bodies)))
	      (else
	       (for-each cps-transform
			 (node-collect-subnodes node (node-child-attr-names node)))))
  node)
