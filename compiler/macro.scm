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

    (lambda (expr)
      (let expand/scope ((scope '()) (expr expr))

	(define (expander/new-scope rib)
	  (let ((new-scope (cons (listify rib) scope)))
	    (lambda (x)
	      (expand/scope new-scope x))))

	(let expand ((expr expr))
	  (cond
	   ;; If it's a symbol, not shadowed by our lexical scope, and
	   ;; bound to a macro, see if it's an ID macro - if it is,
	   ;; call its transformer; otherwise, return the identifier
	   ;; unchanged.
	   ((and (symbol? expr)
		 (not (shadowed? expr scope))
		 (lookup-macro expr))
	    => (lambda (m)
		 (if (eq? (car m) 'id)
		     (expand ((cdr m)))
		     expr)))
	   ;; If it's not a pair here, it can't expand. Return it.
	   ((not (pair? expr))
	    expr)
	   ;; If the car isn't a symbol, or it is, but is in the list
	   ;; of ribs of identifiers lexically in scope, it can't be a
	   ;; macro expansion.  Expand each element of the combination
	   ;; in turn.
	   ((not (and (symbol? (car expr))
		      (not (shadowed? (car expr) scope))))
	    (map expand expr))
	   ;; The car of the combination is a symbol which *might* be
	   ;; a macro or primitive syntax. Test it.
	   (else
	    (case (car expr)
	      ((quote)		expr)
	      ((%jvm-assemble)	(let ((formals (cadr expr))
				      (actuals (caddr expr))
				      (code (cdddr expr)))
				  `(%jvm-assemble ,formals
						  ,(map expand actuals)
						  ,@code)))
	      ((define)		(if (pair? (cadr expr))
				    `(define ,(caadr expr)
				       (lambda ,(cdadr expr)
					 ,@(map (expander/new-scope (cdadr expr))
						(cddr expr))))
				    `(define ,(cadr expr) ,(expand (caddr expr)))))
	      ((defmacro)	(let ((name (cadr expr))
				      (args (caddr expr))
				      (body (cdddr expr)))
				  (let ((transformer-source
					 (expand `(lambda ,args ,@body))))
				    (%define-macro-transformer name 'form
							       (eval transformer-source))
				    `(%define-macro-transformer ',name 'form
								,transformer-source))))
	      ((defmacro-id)	(let ((name (cadr expr))
				      (body (cddr expr)))
				  (let ((transformer-source
					 (expand `(lambda () ,@body))))
				    (%define-macro-transformer name 'id
							       (eval transformer-source))
				    `(%define-macro-transformer ',name 'id
								,transformer-source))))
	      ((lambda)		`(lambda ,(cadr expr)
				   ,@(map (expander/new-scope (cadr expr))
					  (cddr expr))))
	      ((begin)		`(begin ,@(map expand (cdr expr))))
	      ((begin-for-syntax)
				(let ((exprs (expand `(begin ,@(cdr expr)))))
				  (eval exprs)
				  exprs))
	      ((if)		`(if ,@(map expand (cdr expr))))
	      ((set!)		`(set! ,(cadr expr) ,(expand (caddr expr))))
	      (else
	       ;; It's not primitive syntax.
	       (cond
		((lookup-macro (car expr))
		 ;; This symbol is bound to a macro!
		 => (lambda (kind/transformer)
		      (let ((kind (car kind/transformer))
			    (transformer (cdr kind/transformer)))
			(if (eq? kind 'form)
			    (expand (apply transformer (cdr expr)))
			    (error "Non-form macro used in form context" expr)))))
		(else
		 ;; The symbol isn't bound to a macro. It's just a
		 ;; plain combination - expand it as one.
		 (map expand expr))))))))))))
