(define (make-arginfo name is-rest)
  (list name	;; name
	#f	;; arg is ever captured
	#f	;; arg is ever mutated
	is-rest	;; arg is the rest-list argument
	))

(define (arginfo-name ai)		(car ai))
(define (arginfo-captured? ai)		(cadr ai))
(define (arginfo-mutated? ai)		(caddr ai))
(define (arginfo-is-rest? ai)		(cadddr ai))

(define (arginfo-capture! ai)		(set-car! (cdr ai) #t))
(define (arginfo-mutate! ai)		(set-car! (cddr ai) #t))

(define (parse-lambda-formals formals)
  (define (make-arginfos args) (map (lambda (arg) (make-arginfo arg #f)) args))
  (let walk ((formals formals)
	     (acc '()))
    (cond ((pair? formals)
	   (walk (cdr formals)
		 (cons (car formals) acc)))
	  ((null? formals)
	   (let ((new-args (reverse acc)))
	     (values #f
		     new-args
		     (make-arginfos new-args))))
	  (else
	   (values #t
		   (reverse (cons formals acc))
		   (reverse (cons (make-arginfo formals #t) (make-arginfos acc))))))))

(define (parse-body scope body)
  (let collect-definitions ((defs '()) (exprs (flatten-body body)))
    (if (and (not (null? exprs))
	     (list? (car exprs))
	     (eq? (caar exprs) '#%define))
	(if (pair? (cadar exprs))
	    (let* ((definition (cdar exprs))
		   (template (car definition))
		   (body (cdr definition))
		   (varname (car template))
		   (arglist (cdr template)))
	      (collect-definitions (cons `(,varname (#%lambda ,arglist ,@body)) defs)
				   (cdr exprs)))
	    (collect-definitions (cons (cdar exprs) defs) (cdr exprs)))
	(if (null? defs)
	    (parse scope (cons '#%begin exprs))
	    (let* ((defs (reverse defs)) ;; only really to preserve ordering. not required.
		   (temps (map (lambda (binding) (list (gensym (symbol->string (car binding)))
						       (car binding)))
			       defs)))
	      (parse scope
		     `((#%lambda ,(map car defs)
			 ((#%lambda ,(map car temps)
			    ,@(map (lambda (xbinding) `(#%set! ,(cadr xbinding) ,(car xbinding)))
				   temps)
			    ,@exprs)
			  ,@(map cadr defs)))
		       ,@(map (lambda (x) #f) defs))))))))

(define (parse-case-lambda scope formals-declarations body-exprs)
  (let loop ((remaining-formals formals-declarations)
	     (remaining-bodies body-exprs)
	     (acc-arginfos '())
	     (acc-varargs '())
	     (acc-bodies '()))
    (if (or (null? remaining-formals)
	    (null? remaining-bodies))
	(begin
	  (compiler-assert case-lambda-unzip-same-length
			   (and (null? remaining-formals)
				(null? remaining-bodies)))
	  (make-lambda (reverse acc-arginfos)
		       (reverse acc-varargs)
		       (reverse acc-bodies)))
	(let-values (((is-varargs new-args arginfos)
		      (parse-lambda-formals (car remaining-formals))))
	  (syntax-assert lambda-requires-formals-to-be-symbols
			 (every symbol? new-args))
	  (let ((body (parse-body (cons new-args scope) (car remaining-bodies))))
	    (loop (cdr remaining-formals)
		  (cdr remaining-bodies)
		  (cons arginfos acc-arginfos)
		  (cons is-varargs acc-varargs)
		  (cons body acc-bodies)))))))

(define (parse-combination scope expr)
  (make-apply (parse scope (car expr))
	      (map (lambda (x) (parse scope x)) (cdr expr))))

(define (variable-global-in-scope? var scope)
  (not (any (lambda (x) (memq var x)) scope)))

(define (remove-quote x)
  (if (not (and (pair? x)
                (eq? (car x) '#%quote)))
      (error "%jvm-assemble missing required quoting:" x)
      (cadr x)))

(define (extract-lambda-formals expr)
  (if (boolean? (cadr expr)) (caddr expr) (cadr expr)))

(define (extract-lambda-body expr)
  (if (boolean? (cadr expr)) (cddddr expr) (cddr expr)))

(define (parse scope expr)
  (cond
   ((symbol? expr)
    (make-var expr (variable-global-in-scope? expr scope)))
   ((not (pair? expr))
    (make-lit expr))
;; This clause only relevant when not able to use SISC's syntactic-tokens and sc-expand.
;; We don't need it here because #%begin etc are unambiguous non-symbols - they are not
;; scoped as symbols would be.
;;   ((not (and (symbol? (car expr))
;;	      (variable-global-in-scope? (car expr) scope)))
;;    (parse-combination scope expr))
   (else
    (case (car expr)
      ((quote #%quote)
       (make-lit (cadr expr)))
      ((#%define)
       (error "syntax-error"
	      "Internal definition invalid unless at the start of a body:"
	      (and (not (null? (cdr expr)))
		   (cadr expr))))
      ((#%lambda)
       (parse-case-lambda scope
			  (list (extract-lambda-formals expr))
			  (list (extract-lambda-body expr))))
      ((#%letrec)
       (let ((bindings (if (boolean? (cadr expr)) (caddr expr) (cadr expr)))
	     (body (if (boolean? (cadr expr)) (cddddr expr) (cddr expr))))
	 (parse scope `((#%lambda ()
				  ,@(map (lambda (binding)
					   `(#%define ,(car binding) ,(cadr binding)))
					 bindings)
				  ((#%lambda () ,@body)))))))
      ((#%annotate)
       (make-singleton 'undefined))
      ((#%begin)
       (let ((exprs (flatten-body (cdr expr))))
	 (cond 
	  ((null? exprs) (make-singleton 'undefined))
	  ((null? (cdr exprs)) (parse scope (car exprs)))
	  (else
	   (make-begin (parse scope (car exprs))
		       (parse scope (cons '#%begin (cdr exprs))))))))
      ((#%if)
       (if (null? (cdddr expr))
	   (make-if (parse scope (cadr expr))
		    (parse scope (caddr expr))
		    (make-singleton 'undefined))
	   (make-if (parse scope (cadr expr))
		    (parse scope (caddr expr))
		    (parse scope (cadddr expr)))))
      ((#%set!)
       (make-set (cadr expr)
		 (parse scope (caddr expr))))
      ((%case-lambda)
       (let ((lambdas (cdr expr)))
	 (parse-case-lambda scope
			    (map extract-lambda-formals lambdas)
			    (map extract-lambda-body lambdas))))
      ((%jvm-assemble)
       (make-jvm-assemble (remove-quote (cadr expr))
			  (map (lambda (x) (parse scope x)) (caddr expr))
			  (remove-quote (cadddr expr))))
      (else
       (parse-combination scope expr))))))
