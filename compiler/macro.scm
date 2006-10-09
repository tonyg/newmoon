; Macro expansion pass.

(define sys$macros (make-hash-table))

(define (%define-macro-transformer name kind transformer)
  (hash-table-put! sys$macros name (cons kind transformer)))

(define macro-expand
  (let ()
    (define (lookup-macro name)
      (hash-table-get sys$macros name (lambda () #f)))

    (define (listify rib)
      (if (list? rib)
	  rib
	  (let listify ((rib rib))
	    (if (pair? rib)
		(cons (car rib) (listify (cdr rib)))
		(list rib)))))

    (define (shadowed? id scope)
      (any (lambda (x) (memq id x)) scope))

    (define (make-begin exprs)
      (make-node 'begin
		 'exprs exprs))

    (define (make-lit literal-value)
      (make-node 'lit
		 'value literal-value))

    (define (make-apply rator rands)
      (make-node 'apply
		 'rator rator
		 'rands rands))

    (define (make-defmacro name kind transformer)
      (make-apply (make-node 'var 'name '%define-macro-transformer)
		  (list (make-lit name)
			(make-lit kind)
			transformer)))

    (define (parse-backend-asm clause)
      (make-node 'backend-asm
		 'name (car clause)
		 'code (cdr clause)))

    (define (build-and-define-macro name kind transformer)
      (%define-macro-transformer name kind
				 (let ((p (core-scheme-eval transformer)))
				   (lambda args
				     (apply p (lambda (dummy-k result) result) args))))
      (make-defmacro name kind transformer))

    (lambda (expr)
      (let expand/scope ((scope '()) (expr expr))

	(define (make-lambda formals body)
	  (let* ((rib (listify formals))
		 (new-scope (cons rib scope))
		 (varargs (not (list? formals))))
	    (make-node 'lambda
		       'formals rib
		       'varargs varargs
		       'body (map (lambda (x) (expand/scope new-scope x)) body))))

	(let expand ((expr expr))
	  (cond
	   ;; If it's a symbol, it's not shadowed by our lexical
	   ;; scope, and it's bound to a macro, see if it's an ID
	   ;; macro - if it is, call its transformer; otherwise,
	   ;; return the identifier unchanged.
	   ((and (symbol? expr)
		 (not (shadowed? expr scope))
		 (lookup-macro expr))
	    => (lambda (m)
		 (if (eq? (car m) 'id)
		     (expand (make-apply (cdr m) '()))
		     (make-node 'var 'name expr))))
	   ;; If it's a symbol here, it's either present in our
	   ;; lexical scope, or a non-identifier-macro reference. This
	   ;; makes it a variable reference.
	   ((symbol? expr)
	    (make-node 'var 'name expr))
	   ;; If it's not a pair here, it can't expand. Return it.
	   ((not (pair? expr))
	    (make-lit expr))
	   ;; If the car isn't a symbol, or it is, but is in the list
	   ;; of ribs of identifiers lexically in scope, it can't be a
	   ;; macro expansion.  Expand each element of the combination
	   ;; in turn.
	   ((not (and (symbol? (car expr))
		      (not (shadowed? (car expr) scope))))
	    (make-apply (expand (car expr))
			(map expand (cdr expr))))
	   ;; The car of the combination is a symbol which *might* be
	   ;; a macro or primitive syntax. Test it.
	   (else
	    (case (car expr)
	      ((quote)		(make-lit (cadr expr)))
	      ((%assemble)	(make-node 'asm
					   'formals (cadr expr)
					   'actuals (map expand (caddr expr))
					   'code (map parse-backend-asm (cdddr expr))))
	      ((define)		(if (pair? (cadr expr))
				    (make-node 'define
					       'name (caadr expr)
					       'expr (make-lambda (cdadr expr)
								  (cddr expr)))
				    (make-node 'define
					       'name (cadr expr)
					       'expr (expand (caddr expr)))))
	      ((defmacro)	(let ((name (cadr expr))
				      (args (caddr expr))
				      (body (cdddr expr)))
				  (let ((transformer-source (expand `(lambda ,args ,@body))))
				    (build-and-define-macro name 'form transformer-source))))
	      ((defmacro-id)	(let ((name (cadr expr))
				      (body (cddr expr)))
				  (let ((transformer-source (expand `(lambda () ,@body))))
				    (build-and-define-macro name 'id transformer-source))))
	      ((lambda)		(make-lambda (cadr expr) (cddr expr)))
	      ((begin)		(make-begin (map expand (cdr expr))))
	      ((begin-for-syntax)
				(let ((exprs (make-begin (map expand (cdr expr)))))
				  (core-scheme-eval exprs)
				  exprs))
	      ((if)		(make-node 'if
					   'test (expand (cadr expr))
					   'true (expand (caddr expr))
					   'false (if (null? (cdddr expr))
						      (make-node 'void)
						      (expand (car (cdddr expr))))))
	      ((set!)		(make-node 'set
					   'name (cadr expr)
					   'expr (expand (caddr expr))))
	      (else
	       ;; It's not primitive syntax.
	       (cond
		((lookup-macro (car expr))
		 ;; This symbol is bound to a macro!
		 => (lambda (kind/transformer)
		      (let ((kind (car kind/transformer))
			    (transformer (cdr kind/transformer)))
			(if (eq? kind 'form)
			    (expand (apply transformer
					   (lambda (dummy-k result) result)
					   (cdr expr)))
			    (error "Non-form macro used in form context" expr)))))
		(else
		 ;; The symbol isn't bound to a macro. It's just a
		 ;; plain combination - expand it as one.
		 (make-apply (expand (car expr))
			     (map expand (cdr expr))))))))))))))
