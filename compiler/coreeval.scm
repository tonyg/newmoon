;; Evaluator backend, useful for macro execution. Takes the output of
;; the "annotate" phase (a lambda term, in the cps2-value language).

(define (core-scheme-eval sexp)
  (let* ((cps2 (compiler-front-end-phases sexp))
	 (thunk (cps2->closure cps2))
	 (outer-k (lambda (v) v)))
    ((thunk '#() '#()) outer-k)))

(define-record-type core-scheme-cell
  (make-core-scheme-cell name value)
  core-scheme-cell?
  (name core-scheme-cell-name)
  (value core-scheme-cell-value set-core-scheme-cell-value!))

(define (arginfo-boxed? arginfo)
  (and (node-get arginfo 'arginfo 'captured)
       (node-get arginfo 'arginfo 'mutated)))

(define *globals* (make-hash-table))

(define (%define-global-variable name value)
  (hash-table-put! *globals* name value))

(defmacro debug-pretty-print (exp)
  `(begin 'debugging-switched-off))

(define (cps2->closure node)
  (define (make-accessor generate-box-loads arginfo location)
    (node-match location
		((loc-argument index)
		 (if (and generate-box-loads (arginfo-boxed? arginfo))
		     (lambda (args env)
		       (debug-pretty-print `(load.ab ,(node-get arginfo 'arginfo 'name)))
		       (core-scheme-cell-value (vector-ref args index)))
		     (lambda (args env)
		       (debug-pretty-print `(load.a ,(node-get arginfo 'arginfo 'name)))
		       (vector-ref args index))))
		((loc-environment index)
		 (if (and generate-box-loads (arginfo-boxed? arginfo))
		     (lambda (args env)
		       (debug-pretty-print `(load.eb ,(node-get arginfo 'arginfo 'name)))
		       (core-scheme-cell-value (vector-ref env index)))
		     (lambda (args env)
		       (debug-pretty-print `(load.e ,(node-get arginfo 'arginfo 'name)))
		       (vector-ref env index))))))
  (define (make-mutator generate-box-stores arginfo location value-closure)
    (node-match location
		((loc-argument index)
		 (if (and generate-box-stores (arginfo-boxed? arginfo))
		     (lambda (args env)
		       (debug-pretty-print `(store.ab ,(node-get arginfo 'arginfo 'name)))
		       (set-core-scheme-cell-value!
			(vector-ref args index)
			(value-closure args env)))
		     (lambda (args env)
		       (debug-pretty-print `(store.a ,(node-get arginfo 'arginfo 'name)))
		       (vector-set! args index (value-closure args env)))))
		((loc-environment index)
		 (if (and generate-box-stores (arginfo-boxed? arginfo))
		     (lambda (args env)
		       (debug-pretty-print `(store.eb ,(node-get arginfo 'arginfo 'name)))
		       (set-core-scheme-cell-value!
			(vector-ref env index)
			(value-closure args env)))
		     (lambda (args env)
		       (debug-pretty-print `(store.e ,(node-get arginfo 'arginfo 'name)))
		       (vector-set! env index (value-closure args env)))))))
  (define (cache-global name) ;; %%% needs fixing properly
    (hash-table-get *globals* name
		    (lambda ()
		      (let* ((orig-value (eval name))
			     (value (if (procedure? orig-value)
					(lambda (k . actuals)
					  (debug-pretty-print `(primitive ,name))
					  (k (apply orig-value actuals)))
					orig-value)))
			(hash-table-put! *globals* name value)
			value))))
  (define (evaluate-chain args env chain)
    (for-each (lambda (entry) (entry args env)) chain))
  (let walk ((node node))
    (node-match node
		((cps-lit value) (lambda (args env)
				   (debug-pretty-print `(lit ,value))
				   value))
		((cps-local-get name arginfo location)
		 (make-accessor #t arginfo location))
		((cps-global-get name)
		 (lambda (args env)
		   (debug-pretty-print `(load.g ,name))
		   (cache-global name)))
		((cps-lambda formals varargs captures globals expr)
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
							  (lambda (args env)
							    (make-core-scheme-cell
							     (node-get formal 'arginfo 'name)
							     ((make-accessor #f formal location)
							      args env))))))
				     formals
				     formal-locations))
			(capture-actions
			 (map (lambda (capture)
				(let* ((arginfo (node-get capture 'capture 'arginfo))
				       (old-loc (node-get capture 'capture 'old-location))
				       (new-loc (node-get capture 'capture 'new-location))
				       (new-index (node-get new-loc 'loc-environment 'index))
				       (get (make-accessor #f arginfo old-loc)))
				  (lambda (args oldenv newenv)
				    (debug-pretty-print
				     `(capture ,(node-get arginfo 'arginfo 'name)))
				    (vector-set! newenv new-index (get args oldenv)))))
			      captures)))
		   (if varargs
		       (lambda (args oldenv)
			 (let ((env (make-vector num-captures)))
			   (for-each (lambda (action) (action args oldenv env)) capture-actions)
			   (lambda actuals
			     (let ((args (make-vector num-formals)))
			       (do ((i 0 (+ i 1))
				    (actuals actuals (cdr actuals)))
				   ((= i num-non-rest-formals)
				    (vector-set! args i actuals))
				 (vector-set! args i (car actuals)))
			       (evaluate-chain args env boxing-actions)
			       (body-closure args env)))))
		       (lambda (args oldenv)
			 (debug-pretty-print `(lambda ,args ,oldenv ,num-formals ,num-captures))
			 (let ((env (make-vector num-captures)))
			   (for-each (lambda (action) (action args oldenv env)) capture-actions)
			   (lambda actuals
			     (let ((args (list->vector actuals)))
			       (evaluate-chain args env boxing-actions)
			       (body-closure args env))))))))
		((cps-asm formals actuals code)
		 (cond
		  ((find (lambda (clause) (eq? 'scheme (node-get clause 'backend-asm 'name))) code)
		   => (lambda (clause)
			(let* ((code-expr `(lambda ,formals
					     ,@(node-get clause 'backend-asm 'code)))
			       (code-proc (eval code-expr)) ;; !
			       (actual-thunks (map walk actuals)))
			  (lambda (args env)
			    (apply code-proc
				   (map (lambda (t) (t args env)) actual-thunks))))))
		  (else
		   (error "cps-asm missing scheme clause" node))))
		((cps-local-set name arginfo location expr)
		 (make-mutator #t
			       arginfo
			       location
			       (walk expr)))
		((cps-global-set name expr)
		 (let ((body-closure (walk expr)))
		   (lambda (args env)
		     (debug-pretty-print `(store.g ,name))
		     (hash-table-put! *globals* name (body-closure args env)))))
		((cps-apply rator rands)
		 (let ((rator-closure (walk rator))
		       (rands-closures (map walk rands)))
		   (lambda (args env)
		     (apply (rator-closure args env)
			    (map (lambda (c) (c args env)) rands-closures)))))
		((cps-begin head tail)
		 (let ((head-closure (walk head))
		       (tail-closure (walk tail)))
		   (lambda (args env)
		     (head-closure args env)
		     (tail-closure args env))))
		((cps-if test true false)
		 (let ((test-closure (walk test))
		       (true-closure (walk true))
		       (false-closure (walk false)))
		   (lambda (args env)
		     (if (test-closure args env)
			 (true-closure args env)
			 (false-closure args env))))))))
