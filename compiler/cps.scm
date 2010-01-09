(define (head-exprs expr)
  (node-collect-subnodes expr (case (node-kind expr)
				((ds-lit ds-void ds-var ds-lambda ds-backend) '())
				((ds-apply) '(rator . rands))
				((ds-begin) '(head))
				((ds-if) '(test))
				((ds-set) '(expr))
				((ds-asm) 'actuals)
				(else (error "internal-compiler-error"
					     "unknown node kind in head-exprs:"
					     expr)))))

(define (tail-exprs expr)
  (node-collect-subnodes expr (case (node-kind expr)
				((ds-lit ds-void ds-var ds-lambda ds-apply ds-set
					 ds-asm ds-backend) '())
				((ds-begin) '(tail))
				((ds-if) '(true false))
				(else (error "internal-compiler-error"
					     "unknown node kind in tail-exprs:"
					     expr)))))

(define (simple? expr)
  (and (not (memq (node-kind expr) '(ds-apply ds-if)))
       (every simple? (head-exprs expr))
       (every simple? (tail-exprs expr))))

(define (initial-expr expr)
  (let loop ((heads (head-exprs expr)))
    (cond
     ((null? heads) expr)		; expr is its own initial expression
     ((simple? (car heads))
      (loop (cdr heads)))		; this head-expr is not an initial-expr
     (else
      (initial-expr (car heads))))))	; this head-expr may be an initial-expr

(define (subst-expr expr what with)
  (cond
   ((eq? what with)
    expr)
   ((eq? expr what)
    with)
   (else
    (let loop-expr ((expr expr))
      (let loop-subs ((subs (ds-child-attrs expr)))
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
	    (when (pair? tail)
	      (if (eq? (car tail) what)
		  (set-car! tail with)
		  (loop-expr (car tail)))
	      (loop-tail (cdr tail))))))))
    expr)))

(define (make-lambda-cont outer-cont-sym inner-cont-sym node ie)
  (make-node 'cps-lambda
	     'cont #f
	     'formals (list (make-arginfo #f inner-cont-sym))
	     'varargs #f
	     'expr (cps-pushing-transform outer-cont-sym
					  (subst-expr node ie
						      (make-node 'ds-var 'name inner-cont-sym)))))

(define (gen-cont-sym) (gensym "CONT"))
(define (gen-val-sym) (gensym "VAL"))

(define (cps-pushing-transform cont-arg node)
  (cond
   ((simple? node)
    (make-node 'cps-apply
	       'cont #t
	       'rator (make-node 'cps-var 'name cont-arg)
	       'rands (list (cps-transform node))))
   (else
    (let ((ie (initial-expr node)))
      (node-match ie
		  ((ds-begin head tail)
		   (compiler-assert begin-is-never-another-exprs-head-expr (eq? ie node) ie node)
		   (make-node 'cps-begin
			      'head (cps-transform head)
			      'tail (cps-pushing-transform cont-arg tail)))
		  ((ds-apply rator rands)
		   (let ((make-call-cps (lambda (the-cont-node)
					  (make-node 'cps-apply
						     'cont #f
						     'rator (cps-transform rator)
						     'rands (cons the-cont-node
								  (map cps-transform rands))))))
		     (if (eq? ie node)	; if node is its own init-expr
			 (make-call-cps (make-node 'cps-var 'name cont-arg))
			 (make-call-cps (let ((cont-val (gen-val-sym)))
					  (make-lambda-cont cont-arg cont-val node ie))))))
		  ((ds-if test true false)
		   (let ((make-cps-if
			  (lambda (the-cont-var)
			    (make-node 'cps-if
				       'test (cps-transform test)
				       'true (cps-pushing-transform the-cont-var true)
				       'false (cps-pushing-transform the-cont-var false)))))
		     (if (eq? ie node)
			 (make-cps-if cont-arg)
			 (let ((the-cont-var (gen-cont-sym))
			       (the-cont-arg (gen-val-sym)))
			   (make-node 'cps-apply
				      'cont #t
				      'rator (make-node 'cps-lambda
							'cont #f
							'formals (list (make-arginfo #f
										     the-cont-var))
							'varargs #f
							'expr (make-cps-if the-cont-var))
				      'rands (list (make-lambda-cont cont-arg the-cont-arg
								     node ie)))))))
		  ((ds-asm formals actuals code)
		   (compiler-assert assembly-is-always-its-own-head-expr (eq? ie node))
		   (make-node 'cps-asm
			      'formals formals
			      'actuals (map cps-transform actuals)
			      'code code))
		  ((ds-backend backend-name arguments)
		   (compiler-assert backend-is-always-its-own-head-expr (eq? ie node))
		   (make-node 'cps-backend
			      'backend-name backend-name
			      'arguments arguments))
		  (else
		   (error "internal-compiler-error"
			  "invalid initial-expression node-kind; node =" node)))))))

(define (cps-transform node)
  (node-match node
	      ((ds-lit value) (make-node 'cps-lit 'value value))
	      ((ds-void) (make-node 'cps-void))
	      ((ds-var name) (make-node 'cps-var 'name name))
	      ((ds-lambda formals varargs expr)
	       (let ((cont-arg (gen-cont-sym)))
		 (make-node 'cps-lambda
			    'cont (make-arginfo #t cont-arg)
			    'formals formals
			    'varargs varargs
			    'expr (cps-pushing-transform cont-arg expr))))
	      ((ds-apply rator rands)
	       (make-node 'cps-apply
			  'cont #f
			  'rator (cps-transform rator)
			  'rands (map cps-transform rands)))
	      ((ds-begin head tail)
	       (make-node 'cps-begin
			  'head (cps-transform head)
			  'tail (cps-transform tail)))
	      ((ds-if test true false)
	       (make-node 'cps-if
			  'test (cps-transform test)
			  'true (cps-transform true)
			  'false (cps-transform false)))
	      ((ds-set name expr)
	       (make-node 'cps-set
			  'name name
			  'expr (cps-transform expr)))
	      ((ds-asm formals actuals code)
	       (make-node 'cps-asm
			  'formals formals
			  'actuals (map cps-transform actuals)
			  'code code))
	      ((ds-backend backend-name arguments)
	       (make-node 'cps-backend
			  'backend-name backend-name
			  'arguments arguments))))
