(define (head-exprs expr)
  (node-match expr
    ((@ds-apply rator rands) (cons rator rands))
    ((@ds-begin head) (list head))
    ((@ds-if test) (list test))
    ((@ds-set expr) (list expr))
    ((@ds-asm actuals) actuals)
    (else '())))

(define (tail-exprs expr)
  (node-match expr
    ((@ds-begin tail) (list tail))
    ((@ds-if true false) (list true false))
    (else '())))

(define simple?
  (let ((non-simple-types (list @ds-apply @ds-if)))
    (lambda (expr)
      (and (not (memq (node-type expr) non-simple-types))
	   (every simple? (head-exprs expr))
	   (every simple? (tail-exprs expr))))))

(define (initial-expr expr)
  (let loop ((heads (head-exprs expr)))
    (cond
     ((null? heads) expr)		; expr is its own initial expression
     ((simple? (car heads))
      (loop (cdr heads)))		; this head-expr is not an initial-expr
     (else
      (initial-expr (car heads))))))	; this head-expr may be an initial-expr

(define (subst-expr expr what with)
  (if (eq? what with)
      expr
      (node-fold ds-child-attrs
		 (lambda (node k)
		   (cond
		    ((eq? node what) with)
		    (else (k node))))
		 expr)))

(define (expr-references* node mode)
  ;; Icky: treats %cps-value and %cps-exp together
  (let walk ((node node))
    (node-match node
      ((@cps-apply rator rands) (apply lset-union eq? (walk rator) (map walk rands)))
      ((@cps-exp-begin head tail) (lset-union eq? (walk head) (walk tail)))
      ((@cps-exp-if test true false) (lset-union eq? (walk test) (walk true) (walk false)))

      ((@cps-lit) '())
      ((@cps-void) '())
      ((@cps-var name) (case mode
			 ((child-captures) (list name))
			 ((updates) '())
			 ((references) (list name))))
      ((@cps-lambda formals expr expr-references expr-updates)
       (lset-difference eq?
			(case mode
			  ((child-captures) expr-references)
			  ((updates) expr-updates)
			  ((references) expr-references))
			(map @arginfo-name formals)))
      ((@cps-asm actuals) (apply lset-union eq? (map walk actuals)))
      ((@cps-backend) '())
      ((@cps-set name expr) (case mode
			      ((child-captures) '())
			      ((updates) (lset-adjoin eq? (walk expr) name))
			      ((references) (lset-adjoin eq? (walk expr) name))))
      ((@cps-value-begin head tail) (lset-union eq? (walk head) (walk tail)))
      ((@cps-value-if test true false) (lset-union eq? (walk test) (walk true) (walk false))))))

(define (make-cps-lambda cont formals varargs cps-expr)
  (make-node @cps-lambda
	     cont
	     formals
	     varargs
	     cps-expr
	     (expr-references* cps-expr 'references)
	     (expr-references* cps-expr 'updates)))

(define (make-lambda-cont outer-cont-sym inner-cont-sym node ie)
  (make-cps-lambda #f 
		   (list (make-arginfo #f inner-cont-sym))
		   #f
		   (cps-pushing-transform outer-cont-sym
					  (subst-expr node ie
						      (make-node @ds-var inner-cont-sym)))))

(define (gen-cont-sym) (gensym "CONT"))
(define (gen-val-sym) (gensym "VAL"))

(define (cps-pushing-transform cont-arg node)
  (cond
   ((simple? node)
    (make-node @cps-apply
	       #t
	       (make-node @cps-var cont-arg)
	       (list (cps-transform node))))
   (else
    (let ((ie (initial-expr node)))
      (node-match ie
	((@ds-begin head tail)
	 (compiler-assert begin-is-never-another-exprs-head-expr (eq? ie node) ie node)
	 (make-node @cps-exp-begin
		    (cps-transform head)
		    (cps-pushing-transform cont-arg tail)))
	((@ds-apply rator rands)
	 (let ((make-call-cps (lambda (the-cont-node)
				(make-node @cps-apply
					   #f
					   (cps-transform rator)
					   (cons the-cont-node (map cps-transform rands))))))
	   (if (eq? ie node)	; if node is its own init-expr
	       (make-call-cps (make-node @cps-var cont-arg))
	       (make-call-cps (let ((cont-val (gen-val-sym)))
				(make-lambda-cont cont-arg cont-val node ie))))))
	((@ds-if test true false)
	 (let ((make-cps-if
		(lambda (the-cont-var)
		  (make-node @cps-exp-if
			     (cps-transform test)
			     (cps-pushing-transform the-cont-var true)
			     (cps-pushing-transform the-cont-var false)))))
	   (if (eq? ie node)
	       (make-cps-if cont-arg)
	       (let ((the-cont-var (gen-cont-sym))
		     (the-cont-arg (gen-val-sym)))
		 (make-node @cps-apply
			    #t
			    (make-cps-lambda #f
					     (list (make-arginfo #f the-cont-var))
					     #f
					     (make-cps-if the-cont-var))
			    (list (make-lambda-cont cont-arg the-cont-arg node ie)))))))
	((@ds-asm formals actuals code)
	 (compiler-assert assembly-is-always-its-own-head-expr (eq? ie node))
	 (make-node @cps-asm
		    formals
		    (map cps-transform actuals)
		    code))
	((@ds-backend backend-name arguments)
	 (compiler-assert backend-is-always-its-own-head-expr (eq? ie node))
	 (make-node @cps-backend
		    backend-name
		    arguments))
	(else
	 (error "Internal compiler error: invalid initial expression node kind" node)))))))

(define (cps-transform-exp node)
  (node-match node
    ((@ds-begin head tail)
     (make-node @cps-exp-begin
		(cps-transform head)
		(cps-transform-exp tail)))
    ((@ds-if test true false)
     (make-node @cps-exp-if
		(cps-transform test)
		(cps-transform-exp true)
		(cps-transform-exp false)))
    (else (cps-transform node))))

(define (cps-transform node)
  (node-match node
    ((@ds-lit value) (make-node @cps-lit value))
    ((@ds-void) (make-node @cps-void))
    ((@ds-var name) (make-node @cps-var name))
    ((@ds-lambda formals varargs expr)
     (let ((cont-arg (gen-cont-sym)))
       (make-cps-lambda (make-arginfo #t cont-arg)
			formals
			varargs
			(cps-pushing-transform cont-arg expr))))
    ((@ds-apply rator rands)
     (make-node @cps-apply
		#f
		(cps-transform rator)
		(map cps-transform rands)))
    ((@ds-begin head tail)
     (make-node @cps-value-begin
		(cps-transform head)
		(cps-transform tail)))
    ((@ds-if test true false)
     (make-node @cps-value-if
		(cps-transform test)
		(cps-transform true)
		(cps-transform false)))
    ((@ds-set name expr)
     (make-node @cps-set
		name
		(cps-transform expr)))
    ((@ds-asm formals actuals code)
     (make-node @cps-asm
		formals
		(map cps-transform actuals)
		code))
    ((@ds-backend backend-name arguments)
     (make-node @cps-backend
		backend-name
		arguments))))
