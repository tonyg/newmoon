(define (make-arginfo is-cont name)
  (make-node 'arginfo
	     'name name
	     'cont is-cont
	     'captured #f
	     'mutated #f
	     'is-rest #f))

(define (make-normal-arginfo n)
  (make-arginfo #f n))

(define (arginfo-capture! ai)
  (node-set! ai 'arginfo 'captured #t))

(define (arginfo-mutate! ai)
  (node-set! ai 'arginfo 'mutated #t))

(define (make-begin begin-kind empty-value exprs)
  (reduce-right (lambda (head tail)
		  (make-node begin-kind
			     'head head
			     'tail tail))
		empty-value
		exprs))

(define (rewrite-body-internal-definitions body)
  (let collect-definitions ((defs '()) (exprs (flatten-body body)))
    (if (and (not (null? exprs))
	     (node-kind? (car exprs) 'define))
	(collect-definitions (cons (car exprs) defs)
			     (cdr exprs))
	(if (null? defs)
	    (rewrite-internal-definitions (make-node 'begin 'exprs exprs))
	    (let* ((defs (reverse defs)) ;; only really to preserve ordering. not required.
		   (temps (map (lambda (def)
				 (let ((name (node-get def 'define 'name)))
				   (cons (gensym (symbol->string name)) name)))
			       defs)))
	      (let* ((temp-formals (map make-normal-arginfo (map car temps)))
		     (def-formals (map make-normal-arginfo (map cdr temps)))
		     (dummy-initialisers (map (lambda (x) (make-node 'ds-void)) defs))
		     (updaters (map (lambda (temp-entry)
				      (make-node 'ds-set
						 'name (cdr temp-entry)
						 'expr (make-node 'ds-var
								  'name (car temp-entry))))
				    temps))
		     (rewritten-exprs (map rewrite-internal-definitions exprs))
		     (updaters-and-exprs (make-begin 'ds-begin
						     (make-node 'ds-void)
						     (append updaters rewritten-exprs)))
		     (inner-apply (make-node 'ds-apply
					     'rator (make-node 'ds-lambda
							       'formals temp-formals
							       'varargs #f
							       'expr updaters-and-exprs)
					     'rands (map rewrite-internal-definitions
							 (map (node-getter 'define 'expr)
							      defs))))
		     (outer-apply (make-node 'ds-apply
					     'rator (make-node 'ds-lambda
							       'formals def-formals
							       'varargs #f
							       'expr inner-apply)
					     'rands dummy-initialisers)))
		outer-apply))))))

(define (rewrite-lambda-internal-definitions formals varargs body)
  (let* ((rev-formals (reverse formals))
	 (rev-normals (map make-normal-arginfo (if varargs (cdr rev-formals) rev-formals)))
	 (rev-arginfos (if varargs
			   (cons (make-node 'arginfo
					    'name (car rev-formals)
					    'cont #f
					    'captured #f
					    'mutated #f
					    'is-rest #t)
				 rev-normals)
			   rev-normals))
	 (arginfos (reverse rev-arginfos)))
    (make-node 'ds-lambda
	       'formals arginfos
	       'varargs varargs
	       'expr (rewrite-body-internal-definitions body))))

(define (rewrite-internal-definitions expr)
  (node-match expr
	      ((lit value) (make-node 'ds-lit 'value value))
	      ((void) (make-node 'ds-void))
	      ((var name) (make-node 'ds-var 'name name))
	      ((lambda formals varargs body)
	       (rewrite-lambda-internal-definitions formals varargs body))
	      ((apply rator rands)
	       (make-node 'ds-apply
			  'rator (rewrite-internal-definitions rator)
			  'rands (map rewrite-internal-definitions rands)))
	      ((begin exprs)
	       (make-begin 'ds-begin
			   (make-node 'ds-void)
			   (map rewrite-internal-definitions (flatten-body exprs))))
	      ((if test true false)
	       (make-node 'ds-if
			  'test (rewrite-internal-definitions test)
			  'true (rewrite-internal-definitions true)
			  'false (rewrite-internal-definitions false)))
	      ((set name expr)
	       (make-node 'ds-set
			  'name name
			  'expr (rewrite-internal-definitions expr)))
	      ((define name expr)
	       (error "Internal definition invalid unless at the start of a body:" name))
	      ((asm formals actuals code)
	       (make-node 'ds-asm
			  'formals formals
			  'actuals (map rewrite-internal-definitions actuals)
			  'code code))
	      ((backend backend-name arguments)
	       (make-node 'ds-backend
			  'backend-name backend-name
			  'arguments arguments))))
