;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Annotation of ASTs with information about location, capturing and
; mutation of variables.

; For each reference,
;  - compute its runtime location
;
; For each update,
;  - compute its runtime location
;
; For each lambda,
;  - define locations for formals, for use by recursive annotation of the body
;  - define location mappings for captures
;  - compute the set of global names referenced or set

(define (variables-referenced* node include-gets include-sets lambda-expr-handler)
  ;; Icky: treats %cps-value and %cps-exp together
  (let walk ((node node))
    (node-match node
      ((@cps-apply rator rands) (apply lset-union eq? (walk rator) (map walk rands)))
      ((@cps-exp-begin head tail) (lset-union eq? (walk head) (walk tail)))
      ((@cps-exp-if test true false) (lset-union eq? (walk test) (walk true) (walk false)))

      ((@cps-lit) '())
      ((@cps-void) '())
      ((@cps-var name) (if include-gets (list name) '()))
      ((@cps-lambda formals expr) (lset-difference eq?
						   (lambda-expr-handler expr)
						   (map @arginfo-name formals)))
      ((@cps-asm actuals) (apply lset-union eq? (map walk actuals)))
      ((@cps-backend) '())
      ((@cps-set name expr) (if include-sets
				(lset-adjoin eq? (walk expr) name)
				(walk expr)))
      ((@cps-value-begin head tail) (lset-union eq? (walk head) (walk tail)))
      ((@cps-value-if test true false) (lset-union eq? (walk test) (walk true) (walk false))))))

(define (variables-referenced node g s)
  (variables-referenced* node g s (lambda (expr) (variables-referenced expr g s))))

(define (variables-captured node)
  (variables-referenced* node #f #f (lambda (expr) (variables-referenced expr #t #t))))

(define-record-type rib
  (make-rib* lambda-node scope capture-mapping global-set)
  rib?
  (lambda-node rib-lambda-node)
  (scope rib-scope)
  (capture-mapping rib-capture-mapping set-rib-capture-mapping!)
  (global-set rib-global-set set-rib-global-set!))

(define (make-rib lambda-node)
  (make-rib* lambda-node
	     (let ((names-set (variables-referenced (@cps-lambda-expr lambda-node) #f #t))
		   (names-cap (variables-captured (@cps-lambda-expr lambda-node)))
		   (all-formals (let ((c (@cps-lambda-cont lambda-node)))
				  (if c
				      (cons c (@cps-lambda-formals lambda-node))
				      (@cps-lambda-formals lambda-node)))))
	       (map (lambda (ai i) (cons (@arginfo-name ai)
					 (make-node @loc-local
						    'argument
						    ai
						    (if (memq (@arginfo-name ai) names-cap) #t #f)
						    (if (memq (@arginfo-name ai) names-set) #t #f)
						    i)))
		    all-formals
		    (iota (length all-formals))))
	     '()
	     '()))

(define (find-location name env)
  (define (walk env)
    (if (null? env)
	(make-node @loc-global)
	(let ((rib (car env)))
	  (cond
	   ((assq name (rib-scope rib)) => cdr)
	   ((assq name (rib-capture-mapping rib)) =>
	    (lambda (entry) (@capture-new-location (cdr entry))))
	   (else
	    (let ((loc (walk (cdr env))))
	      (node-match loc
		((@loc-global)
		 loc)
		((@loc-local arginfo captured mutable)
		 (let* ((old-mapping (rib-capture-mapping rib))
			(new-loc (make-node @loc-local 'environment
					    arginfo captured mutable (length old-mapping))))
		   (set-rib-capture-mapping! rib
					     (cons (cons (@arginfo-name arginfo)
							 (make-node @capture loc new-loc))
						   old-mapping))
		   new-loc)))))))))
  (let ((loc (walk env)))
    (when (node-kind? loc @loc-global)
      (compiler-assert at-least-one-rib-present (pair? env))
      (set-rib-global-set! (car env)
			   (lset-adjoin eq? (rib-global-set (car env)) name)))
    loc))

(define (annotate-root node)
  ;; FIXME: superlinear! possibly O(n^2) or worse
  (annotate-in-env '() (lambda (e v) (v node))))

(define (annotate-in-env env callback)
  (define (annotate-exp node)
    (pretty-print `(exp ,(node-type-name (node-type node))))
    (node-match node
      ((@cps-apply cont rator rands)
       (make-node @cps2-apply cont (annotate-value rator) (map annotate-value rands)))
      ((@cps-exp-begin head tail)
       (make-node @cps2-exp-begin (annotate-value head) (annotate-exp tail)))
      ((@cps-exp-if test true false)
       (make-node @cps2-exp-if (annotate-value test) (annotate-exp true) (annotate-exp false)))))

  (define (annotate-value node)
    (pretty-print `(val ,(node-type-name (node-type node))))
    (node-match node
      ((@cps-lit value) (make-node @cps2-lit value))
      ((@cps-void) (make-node @cps2-void))
      ((@cps-var name) (make-node @cps2-get name (find-location name env)))
      ((@cps-lambda cont varargs expr)
       (let* ((rib (make-rib node))
	      (expr2 (annotate-in-env (cons rib env)
				      (lambda (e v) (e expr)))))
	 (make-node @cps2-lambda
		    (not cont) ;; no continuation-argument means we *are* a continuation
		    (map cdr (rib-scope rib))
		    varargs
		    expr2
		    (map cdr (rib-capture-mapping rib))
		    (rib-global-set rib))))
      ((@cps-asm formals actuals code)
       (make-node @cps2-asm formals (map annotate-value actuals) code))
      ((@cps-backend backend-name arguments) (make-node @cps2-backend backend-name arguments))
      ((@cps-set name expr)
       (make-node @cps2-set name (find-location name env) (annotate-value expr)))
      ((@cps-value-begin head tail)
       (make-node @cps2-value-begin (annotate-value head) (annotate-exp tail)))
      ((@cps-value-if test true false)
       (make-node @cps2-value-if
		  (annotate-value test) (annotate-value true) (annotate-value false)))))

  (callback annotate-exp annotate-value))
