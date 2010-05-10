;; Evaluator backend, useful for macro execution. Takes the output of
;; the "annotate" phase (a program, in the %il language).

(define (core-scheme-eval sexp)
  (when (debug-mode=? 'coreeval)
    (pretty-print `(core-scheme-eval ',(if (node? sexp)
					   (vector "NODE"
						   (node->list sexp))
					   sexp))))
  (let* ((il (compiler-front-end-phases 'scheme sexp))
	 (thunk (il->closure il))
	 (result ((thunk '#() '#()))))
    (when (debug-mode=? 'coreeval)
      (pretty-print `("core-scheme-eval-result" ,result)))
    result))

(define *globals* (make-hash-table))

(defmacro debug-pretty-print (exp)
  `(begin (when (debug-mode=? 'coreeval-pp) (pretty-print ,exp)) 'debug-pretty-print-result)
  ;;`(begin 'debug-pretty-print-result)
  )

(define ($define-global-variable name value)
  (debug-pretty-print `($define-global-variable ,name ,value))
  (hash-table-put! *globals* name value))

(define *coreeval-magic* (cons "coreeval" "magic"))

(define (coreeval$apply magic args)
  (if (eq? magic *coreeval-magic*)
      (let ((k (car args))
	    (fn (cadr args))
	    (args (cddr args)))
	(k *coreeval-magic* (list (apply apply fn args))))
      ;; Now, we probably could do something sensible here, I'm just
      ;; too lazy at present to figure out what that thing might be.
      (error "Expected magic in coreeval$apply")))

(define (il->closure program-node)
  (define (magic-wrapper proc)
    (lambda actuals
      (debug-pretty-print `(magic-wrapper ,proc ,actuals))
      (if (or (null? actuals)
	      (not (eq? (car actuals) *coreeval-magic*)))
	  (let ((wrapping-outer-k (lambda (dummy-magic-variable actuals) (car actuals))))
	    (proc (cons wrapping-outer-k actuals)))
	  (proc (cadr actuals)))))
  (define (magic-k-wrapper proc)
    (lambda actuals
      (debug-pretty-print `(magic-k-wrapper ,proc ,actuals))
      (if (or (null? actuals)
	      (not (eq? (car actuals) *coreeval-magic*)))
	  (proc actuals)
	  (proc (cadr actuals)))))
  (define (magic-unwrapper name proc)
    (lambda actuals
      (debug-pretty-print `(magic-unwrapper ,name ,actuals))
      (if (or (null? actuals)
	      (not (eq? (car actuals) *coreeval-magic*)))
	  (apply proc actuals)
	  (let ((v (apply proc (cdr (cadr actuals)))))
	    (debug-pretty-print `(unwrapper-value ,v))
	    ((car (cadr actuals)) *coreeval-magic* (list v))))))
  (define (maybe-unwrap name value)
    (if (procedure? value)
	(magic-unwrapper name value)
	value))
  (define (cache-global name) ;; %%% needs fixing properly
    (hash-table-get *globals* name
		    (lambda ()
		      (let* ((orig-value (eval name))
			     (value (maybe-unwrap name orig-value)))
			(hash-table-put! *globals* name value)
			value))))
  (define (evaluate-chain args env chain)
    (if (null? chain)
	'<void>
	(let walk ((chain chain))
	  (cond
	   ((null? (cdr chain)) ((car chain) args env))
	   (else
	    ((car chain) args env)
	    (walk (cdr chain)))))))

  (node-match program-node
    ((@il-program literal-table closure-table root-closure-index)

     (define (compile-closure index capture-exprs)
       (node-match (list-ref closure-table index)
	 ((@il-closure-template is-continuation formals varargs environment instructions)
	  (let ((closure-table-index index)
		(num-formals (length formals))
		(num-non-rest-formals (- (length formals) 1))
		(num-captures (length environment))
		(capture-actions
		 (do ((new-index (- (length capture-exprs) 1) (- new-index 1))
		      (capture-exprs capture-exprs (cdr capture-exprs))
		      (environment environment (cdr environment))
		      (acc '() (cons (let* ((capture-expr (car capture-exprs))
					    (arginfo (car environment))
					    (name (@arginfo-name arginfo))
					    (get (compile capture-expr)))
				       (lambda (args oldenv newenv)
					 (let ((v (get args oldenv)))
					   (debug-pretty-print `(store.ec ,name ,new-index))
					   (vector-set! newenv new-index v))))
				     acc)))
		     ((null? capture-exprs)
		      (reverse acc))))
		(compiled-instructions (map compile instructions)))
	    (if varargs
		(lambda (oldargs oldenv)
		  (debug-pretty-print `(va-lambda ,closure-table-index
						  ,oldargs ,oldenv ,num-formals ,num-captures))
		  (let ((env (make-vector num-captures 'uninitialized)))
		    (for-each (lambda (action) (action oldargs oldenv env)) capture-actions)
		    ((if is-continuation magic-k-wrapper magic-wrapper)
		     (lambda (actuals)
		       (debug-pretty-print `(va-app ,closure-table-index ,actuals))
		       (let ((args (make-vector num-formals 'bug)))
			 (do ((i 0 (+ i 1))
			      (actuals actuals (cdr actuals)))
			     ((= i num-non-rest-formals)
			      (vector-set! args i actuals))
			   (vector-set! args i (car actuals)))
			 (evaluate-chain args env compiled-instructions))))))
		(lambda (oldargs oldenv)
		  (debug-pretty-print `(lambda ,closure-table-index
					 ,oldargs ,oldenv ,num-formals ,num-captures))
		  (let ((env (make-vector num-captures 'uninitialized)))
		    (for-each (lambda (action) (action oldargs oldenv env)) capture-actions)
		    ((if is-continuation magic-k-wrapper magic-wrapper)
		     (lambda (actuals)
		       (debug-pretty-print `(app ,closure-table-index ,actuals))
		       (let ((args (list->vector actuals)))
			 (evaluate-chain args env compiled-instructions)))))))))))

     (define (compile node)
       ;;(debug-pretty-print `(COMPILING ,(node->list node)))
       (node-match node
	 ((@il-lit index) (let ((value (list-ref literal-table index)))
			    (lambda (args env)
			      (debug-pretty-print `(lit ,value))
			      value)))
	 ((@il-void) (lambda (args env)
		       (debug-pretty-print `(void))
		       '<void>))
	 ((@il-closure index capture-exprs)
	  (compile-closure index capture-exprs))
	 ((@il-asm formals actuals code)
	  (let* ((code-expr `(lambda ,formals ,@code))
		 (code-proc (eval code-expr)) ;; !
		 (actual-thunks (map compile actuals)))
	    (lambda (args env)
	      (let ((actual-values (map (lambda (t) (t args env)) actual-thunks)))
		(debug-pretty-print `(asm ,actual-values ,code-expr))
		(apply code-proc actual-values)))))
	 ((@il-install-box name index)
	  (lambda (args env)
	    (debug-pretty-print `(box.a ,name ,index))
	    (vector-set! args index (box (vector-ref args index)))))
	 ((@il-load-argument name index)
	  (lambda (args env)
	    (debug-pretty-print `(load.a ,name ,index))
	    (vector-ref args index)))
	 ((@il-load-environment name index)
	  (lambda (args env)
	    (debug-pretty-print `(load.e ,name ,index))
	    (vector-ref env index)))
	 ((@il-load-box expr)
	  (let ((get (compile expr)))
	    (lambda (args env)
	      (let ((b (get args env)))
		(debug-pretty-print `(load.b))
		(unbox b)))))
	 ((@il-load-global name)
	  (lambda (args env)
	    (debug-pretty-print `(load.g ,name))
	    (cache-global name)))
	 ((@il-store-argument name index expr)
	  (let ((v (compile expr)))
	    (lambda (args env)
	      (debug-pretty-print `(store.a ,name ,index))
	      (vector-set! args index (v args env)))))
	 ((@il-store-environment name index expr)
	  (let ((v (compile expr)))
	    (lambda (args env)
	      (debug-pretty-print `(store.e ,name ,index))
	      (vector-set! env index (v args env)))))
	 ((@il-store-box location-expr value-expr)
	  (let ((lv (compile location-expr))
		(vv (compile value-expr)))
	    (lambda (args env)
	      (let ((value (vv args env))
		    (b (lv args env)))
		(debug-pretty-print `(store.b))
		(set-box! b value)))))
	 ((@il-store-global name expr)
	  (let ((v (compile expr)))
	    (lambda (args env)
	      (debug-pretty-print `(store.g ,name))
	      (hash-table-put! *globals* name (v args env)))))
	 ((@il-apply rator rands)
	  (let ((rator-closure (compile rator))
		(rands-closures (map compile rands)))
	    (lambda (args env)
	      (let* ((argvals (map (lambda (c) (c args env)) rands-closures))
		     (ratorval (rator-closure args env)))
		(debug-pretty-print `(app-outer ,ratorval ,@argvals))
		(ratorval *coreeval-magic* argvals)))))
	 ((@il-begin head tail)
	  (let ((head-closure (compile head))
		(tail-closure (compile tail)))
	    (lambda (args env)
	      (head-closure args env)
	      (tail-closure args env))))
	 ((@il-if test true false)
	  (let ((test-closure (compile test))
		(true-closure (compile true))
		(false-closure (compile false)))
	    (lambda (args env)
	      (let ((test-result (test-closure args env)))
		(debug-pretty-print `(if ,test-result))
		(if test-result
		    (true-closure args env)
		    (false-closure args env))))))))

     (compile-closure root-closure-index '()))))