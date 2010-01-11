(define (make-begin begin-kind empty-value exprs)
  (reduce-right (lambda (head tail) (make-node begin-kind head tail))
		empty-value
		exprs))

(define (rewrite-body-internal-definitions body)
  (let collect-definitions ((defs '()) (exprs (flatten-body body)))
    (if (and (not (null? exprs))
	     (node-kind? (car exprs) @core-define))
	(collect-definitions (cons (car exprs) defs)
			     (cdr exprs))
	(if (null? defs)
	    (rewrite-internal-definitions (make-node @core-begin exprs))
	    (let* ((defs (reverse defs)) ;; only really to preserve ordering. not required.
		   (temps (map (lambda (def)
				 (let ((name (@core-define-name def)))
				   (cons (gensym (symbol->string name)) name)))
			       defs)))
	      (let* ((temp-formals (map make-normal-arginfo (map car temps)))
		     (def-formals (map make-normal-arginfo (map cdr temps)))
		     (dummy-initialisers (map (lambda (x) (make-node @ds-void)) defs))
		     (updaters (map (lambda (temp-entry)
				      (make-node @ds-set
						 (cdr temp-entry)
						 (make-node @ds-var (car temp-entry))))
				    temps))
		     (rewritten-exprs (map rewrite-internal-definitions exprs))
		     (updaters-and-exprs (make-begin @ds-begin
						     (make-node @ds-void)
						     (append updaters rewritten-exprs)))
		     (inner-apply (make-node @ds-apply
					     (make-node @ds-lambda
							temp-formals
							#f
							updaters-and-exprs)
					     (map rewrite-internal-definitions
						  (map @core-define-expr defs))))
		     (outer-apply (make-node @ds-apply
					     (make-node @ds-lambda
							def-formals
							#f
							inner-apply)
					     dummy-initialisers)))
		outer-apply))))))

(define (rewrite-lambda-internal-definitions formals varargs body)
  (let* ((rev-formals (reverse formals))
	 (rev-normals (map make-normal-arginfo (if varargs (cdr rev-formals) rev-formals)))
	 (rev-arginfos (if varargs
			   (cons (make-node @arginfo (car rev-formals) #f #t)
				 rev-normals)
			   rev-normals))
	 (arginfos (reverse rev-arginfos)))
    (make-node @ds-lambda
	       arginfos
	       varargs
	       (rewrite-body-internal-definitions body))))

(define (rewrite-internal-definitions expr)
  (node-match expr
    ((@core-lit value) (make-node @ds-lit value))
    ((@core-void) (make-node @ds-void))
    ((@core-var name) (make-node @ds-var name))
    ((@core-lambda formals varargs body)
     (rewrite-lambda-internal-definitions formals varargs body))
    ((@core-apply rator rands)
     (make-node @ds-apply
		(rewrite-internal-definitions rator)
		(map rewrite-internal-definitions rands)))
    ((@core-begin exprs)
     (make-begin @ds-begin
		 (make-node @ds-void)
		 (map rewrite-internal-definitions (flatten-body exprs))))
    ((@core-if test true false)
     (make-node @ds-if
		(rewrite-internal-definitions test)
		(rewrite-internal-definitions true)
		(rewrite-internal-definitions false)))
    ((@core-set name expr)
     (make-node @ds-set
		name
		(rewrite-internal-definitions expr)))
    ((@core-define name expr)
     (error "Internal definition in invalid position" name))
    ((@core-asm formals actuals code)
     (make-node @ds-asm
		formals
		(map rewrite-internal-definitions actuals)
		code))
    ((@core-backend backend-name arguments)
     (make-node @ds-backend
		backend-name
		arguments))))
