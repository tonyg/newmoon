;; Evaluator backend, useful for macro execution. Takes the output of
;; the "annotate" phase (a lambda term, in the cps2-value language).

(define (core-scheme-eval sexp)
  (if (debug-mode=? 'coreeval) (pretty-print `(core-scheme-eval ',(if (node? sexp)
								      (vector "NODE"
									      (node->list sexp))
								      sexp))))
  (let* ((cps2 (compiler-front-end-phases sexp))
	 (thunk (cps2->closure cps2))
	 (outer-k (lambda (dummy-magic dummy-k v) v))
	 (result ((thunk outer-k '#() '#()))))
    (if (debug-mode=? 'coreeval) (pretty-print `("core-scheme-eval-result" ,result)))
    result))

(define-record-type core-scheme-cell
  (make-core-scheme-cell name value)
  core-scheme-cell?
  (name core-scheme-cell-name)
  (value core-scheme-cell-value set-core-scheme-cell-value!))

(define (arginfo-boxed? arginfo)
  (and (node-get arginfo 'arginfo 'captured)
       (node-get arginfo 'arginfo 'mutated)))

(define *globals* (make-hash-table))

(defmacro debug-pretty-print (exp)
  `(begin (if (debug-mode=? 'coreeval-pp) (pretty-print ,exp)) 'debug-pretty-print-result)
  ;;`(begin 'debug-pretty-print-result)
  )

(define (%define-global-variable name value)
  (debug-pretty-print `(%define-global-variable ,name ,value))
  (hash-table-put! *globals* name value))

(define *coreeval-magic* (cons "coreeval" "magic"))

(define (cps2->closure node)
  (define (make-accessor generate-box-loads arginfo location)
    (node-match location
		((loc-continuation)
		 (if (and generate-box-loads (arginfo-boxed? arginfo))
		     (error "Boxed continuation")
		     (lambda (k args env)
		       (debug-pretty-print `(load.k ,(node-get arginfo 'arginfo 'name)))
		       k)))
		((loc-argument index)
		 (if (and generate-box-loads (arginfo-boxed? arginfo))
		     (lambda (k args env)
		       (debug-pretty-print `(load.ab ,(node-get arginfo 'arginfo 'name)))
		       (core-scheme-cell-value (vector-ref args index)))
		     (lambda (k args env)
		       (debug-pretty-print `(load.a ,(node-get arginfo 'arginfo 'name)))
		       (vector-ref args index))))
		((loc-environment index)
		 (if (and generate-box-loads (arginfo-boxed? arginfo))
		     (lambda (k args env)
		       (debug-pretty-print `(load.eb ,(node-get arginfo 'arginfo 'name)))
		       (core-scheme-cell-value (vector-ref env index)))
		     (lambda (k args env)
		       (debug-pretty-print `(load.e ,(node-get arginfo 'arginfo 'name)))
		       (vector-ref env index))))))
  (define (make-mutator generate-box-stores arginfo location value-closure)
    (node-match location
		((loc-continuation)
		 (error "Store into continuation"))
		((loc-argument index)
		 (if (and generate-box-stores (arginfo-boxed? arginfo))
		     (lambda (k args env)
		       (debug-pretty-print `(store.ab ,(node-get arginfo 'arginfo 'name)))
		       (set-core-scheme-cell-value!
			(vector-ref args index)
			(value-closure k args env)))
		     (lambda (k args env)
		       (debug-pretty-print `(store.a ,(node-get arginfo 'arginfo 'name)))
		       (vector-set! args index (value-closure k args env)))))
		((loc-environment index)
		 (if (and generate-box-stores (arginfo-boxed? arginfo))
		     (lambda (k args env)
		       (debug-pretty-print `(store.eb ,(node-get arginfo 'arginfo 'name)))
		       (set-core-scheme-cell-value!
			(vector-ref env index)
			(value-closure k args env)))
		     (lambda (k args env)
		       (debug-pretty-print `(store.e ,(node-get arginfo 'arginfo 'name)))
		       (vector-set! env index (value-closure k args env)))))))
  (define (magic-wrapper proc)
    (lambda actuals
      (debug-pretty-print `(magic-wrapper ,proc ,actuals))
      (if (or (null? actuals)
	      (not (eq? (car actuals) *coreeval-magic*)))
	  (proc (lambda (dummy-magic dummy-k argvals) (car argvals)) actuals)
	  (proc (cadr actuals) (caddr actuals)))))
  (define (magic-unwrapper name proc)
    (lambda actuals
      (debug-pretty-print `(magic-unwrapper ,name ,actuals))
      (if (or (null? actuals)
	      (not (eq? (car actuals) *coreeval-magic*)))
	  (apply proc actuals)
	  (let ((v (apply proc (caddr actuals))))
	    (debug-pretty-print `(unwrapper-value ,v))
	    ((cadr actuals) *coreeval-magic* 'dummy-unwrapper-continuation-value (list v))))))
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
  (define (evaluate-chain k args env chain)
    (for-each (lambda (entry) (entry k args env)) chain))
  (let walk ((node node))
    (node-match node
		((cps-lit value) (lambda (k args env)
				   (debug-pretty-print `(lit ,value))
				   value))
		((cps-void) (lambda (k args env)
			      (debug-pretty-print `(void))
			      '<void>))
		((cps-local-get name arginfo location)
		 (make-accessor #t arginfo location))
		((cps-global-get name)
		 (lambda (k args env)
		   (debug-pretty-print `(load.g ,name))
		   (cache-global name)))
		((cps-lambda cont formals varargs captures globals expr)
		 (let* ((body-closure (walk expr))
			(num-formals (length formals))
			(num-non-rest-formals (- (length formals) 1))
			(num-captures (length captures))
			(formal-locations (map (lambda (i) (make-node 'loc-argument 'index i))
					       (iota num-formals)))
			(boxing-actions
			 (filter-map (lambda (formal location)
				       (and (arginfo-boxed? formal)
					    (make-mutator #f
							  formal
							  location
							  (lambda (k args env)
							    (make-core-scheme-cell
							     (node-get formal 'arginfo 'name)
							     ((make-accessor #f formal location)
							      k args env))))))
				     formals
				     formal-locations))
			(capture-actions
			 (map (lambda (capture)
				(let* ((arginfo (node-get capture 'capture 'arginfo))
				       (old-loc (node-get capture 'capture 'old-location))
				       (new-loc (node-get capture 'capture 'new-location))
				       (new-index (node-get new-loc 'loc-environment 'index))
				       (get (make-accessor #f arginfo old-loc)))
				  (lambda (k args oldenv newenv)
				    (debug-pretty-print
				     `(capture ,(node-get arginfo 'arginfo 'name)))
				    (vector-set! newenv new-index (get k args oldenv)))))
			      captures)))
		   ;; If the continuation is captured *and* mutated,
		   ;; this codegen will need changing.
		   (compiler-assert continuation-arginfo-not-boxed
				    (not (and cont (arginfo-boxed? cont))))
		   (if varargs
		       (lambda (k args oldenv)
			 (debug-pretty-print `(va-lambda ,args ,oldenv ,num-formals ,num-captures))
			 (let ((env (make-vector num-captures)))
			   (for-each (lambda (action) (action k args oldenv env)) capture-actions)
			   (magic-wrapper (lambda (k actuals)
					    (debug-pretty-print `(va-app ,k ,@actuals))
					    (let ((args (make-vector num-formals)))
					      (do ((i 0 (+ i 1))
						   (actuals actuals (cdr actuals)))
						  ((= i num-non-rest-formals)
						   (vector-set! args i actuals))
						(vector-set! args i (car actuals)))
					      (evaluate-chain k args env boxing-actions)
					      (body-closure k args env))))))
		       (lambda (k args oldenv)
			 (debug-pretty-print `(lambda ,args ,oldenv ,num-formals ,num-captures))
			 (let ((env (make-vector num-captures)))
			   (for-each (lambda (action) (action k args oldenv env)) capture-actions)
			   (magic-wrapper (lambda (k actuals)
					    (debug-pretty-print `(app ,k ,@actuals))
					    (let ((args (list->vector actuals)))
					      (evaluate-chain k args env boxing-actions)
					      (body-closure k args env)))))))))
		((cps-asm formals actuals code)
		 (cond
		  ((find (lambda (clause) (eq? 'scheme (node-get clause 'backend-asm 'name))) code)
		   => (lambda (clause)
			(let* ((code-expr `(lambda ,formals
					     ,@(node-get clause 'backend-asm 'code)))
			       (code-proc (eval code-expr)) ;; !
			       (actual-thunks (map walk actuals)))
			  (lambda (k args env)
			    (maybe-unwrap
			     'anonymous-result-of-scheme-assembly
			     (apply code-proc
				    (map (lambda (t) (t k args env)) actual-thunks)))))))
		  (else
		   (error "cps-asm missing scheme clause" node))))
		((cps-local-set name arginfo location expr)
		 (make-mutator #t
			       arginfo
			       location
			       (walk expr)))
		((cps-global-set name expr)
		 (let ((body-closure (walk expr)))
		   (lambda (k args env)
		     (debug-pretty-print `(store.g ,name))
		     (hash-table-put! *globals* name (body-closure k args env)))))
		((cps-apply cont rator rands)
		 (let ((rator-closure (walk rator))
		       (rands-closures (map walk rands)))
		   (lambda (k args env)
		     (let ((argvals (map (lambda (c) (c k args env)) rands-closures)))
		       (if cont
			   ((rator-closure k args env) *coreeval-magic*
			    'dummy-continuation-value argvals)
			   ((rator-closure k args env) *coreeval-magic*
			    (car argvals) (cdr argvals)))))))
		((cps-begin head tail)
		 (let ((head-closure (walk head))
		       (tail-closure (walk tail)))
		   (lambda (k args env)
		     (head-closure k args env)
		     (tail-closure k args env))))
		((cps-if test true false)
		 (let ((test-closure (walk test))
		       (true-closure (walk true))
		       (false-closure (walk false)))
		   (lambda (k args env)
		     (let ((test-result (test-closure k args env)))
		       (debug-pretty-print `(if ,test-result))
		       (if test-result
			   (true-closure k args env)
			   (false-closure k args env)))))))))
