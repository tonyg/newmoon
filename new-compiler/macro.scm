; Macro expansion pass.

(define sys$macros (make-hash-table))

(define ($define-macro-transformer name kind transformer)
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
      (make-node @core-begin exprs))

    (define (make-lit literal-value)
      (make-node @core-lit literal-value))

    (define (make-apply rator rands)
      (make-node @core-apply rator rands))

    (define (make-defmacro name kind transformer)
      (make-apply (make-node @core-var '$define-macro-transformer)
		  (list (make-lit name)
			(make-lit kind)
			transformer)))

    (define (parse-backend-asm clause)
      (make-node @backend-asm (car clause) (cdr clause)))

    (define (build-and-define-macro name kind transformer)
      ($define-macro-transformer name kind (core-scheme-eval transformer))
      (make-defmacro name kind transformer))

    (lambda (expr)
      (let expand/scope ((scope '()) (expr expr))

	(define (make-lambda formals body)
	  (let* ((rib (listify formals))
		 (new-scope (cons rib scope)))
	    (make-node @core-lambda
		       rib
		       (not (list? formals))
		       (map (lambda (x) (expand/scope new-scope x)) body))))

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
		     (make-node @core-var expr))))
	   ;; If it's a symbol here, it's either present in our
	   ;; lexical scope, or a non-identifier-macro reference. This
	   ;; makes it a variable reference.
	   ((symbol? expr)
	    (make-node @core-var expr))
	   ;; If it's not a pair here, either it's a pre-expanded
	   ;; node, in which case it should be returned without
	   ;; further wrapping, or it's a literal, in which case it
	   ;; can't expand any further and should be returned within a
	   ;; lit node.
	   ((not (pair? expr))
	    (if (node? expr)
		expr
		(make-lit expr)))
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
	      ((%assemble)	(make-node @core-asm
					   (cadr expr)
					   (map expand (caddr expr))
					   (map parse-backend-asm (cdddr expr))))
	      ((%backend)	(make-node @core-backend
					   (cadr expr)
					   (cddr expr)))
	      ((define)		(if (pair? (cadr expr))
				    (make-node @core-define
					       (caadr expr)
					       (make-lambda (cdadr expr) (cddr expr)))
				    (make-node @core-define
					       (cadr expr)
					       (expand (caddr expr)))))
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
	      ((if)		(make-node @core-if
					   (expand (cadr expr))
					   (expand (caddr expr))
					   (if (null? (cdddr expr))
					       (make-node @core-void)
					       (expand (car (cdddr expr))))))
	      ((set!)		(make-node @core-set
					   (cadr expr)
					   (expand (caddr expr))))
	      ((require)	(visit-libraries (cdr expr))
				(expand `(require-libraries ',(cdr expr))))
	      (else
	       ;; It's not primitive syntax.
	       (cond
		((lookup-macro (car expr))
		 ;; This symbol is bound to a macro!
		 => (lambda (kind/transformer)
		      (when (debug-mode=? 'expansion)
			(pretty-print `(expanding ',expr)))
		      (let ((kind (car kind/transformer))
			    (transformer (cdr kind/transformer)))
			(if (eq? kind 'form)
			    (let ((next-form (apply transformer (cdr expr))))
			      (expand next-form))
			    (error "Non-form macro used in form context" expr)))))
		(else
		 ;; The symbol isn't bound to a macro. It's just a
		 ;; plain combination - expand it as one.
		 (make-apply (expand (car expr))
			     (map expand (cdr expr))))))))))))))
