;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Annotation of parse tree environments.

;; Main entry point:
(define (annotate-tree cps-parse-tree)
  (annotate-env '() cps-parse-tree))

(define (find-capture name l)
  (cond
   ((null? l) #f)
   ((eq? (node-get (node-get (car l) 'capture 'arginfo) 'arginfo 'name) name)
    (car l))
   (else (find-capture name (cdr l)))))

(define (memq-arginfo name l)
  (cond
   ((null? l) #f)
   ((eq? (node-get (car l) 'arginfo 'name) name) l)
   (else (memq-arginfo name (cdr l)))))

(define (find-location lambda-stack varname global-k local-k)
  (let search-lambda-stack ((require-global-entry? #t)
			    (lambda-stack lambda-stack)
			    (global-k global-k)
			    (local-k local-k))
    (if (null? lambda-stack)
	(global-k)
	(let* ((lambda-node (car lambda-stack))
	       (cont (node-get lambda-node 'cps-lambda 'cont))
	       (formals (node-get lambda-node 'cps-lambda 'formals))
	       (old-captures (node-get/default! lambda-node 'captures '()))
	       (old-globals (node-get/default! lambda-node 'globals '())))
	  (cond
	   ((find-capture varname old-captures) =>
	    ;; We've already captured a variable of this name. Return
	    ;; the location we're storing the captured cell in (and
	    ;; the captured arginfo).
	    (lambda (capture-record)
	      (local-k (node-get capture-record 'capture 'arginfo)
		       (node-get capture-record 'capture 'new-location))))

	   ((memq varname old-globals)
	    ;; We've already checked, and none of our parents define
	    ;; or capture this variable - we know it's a global.
	    (global-k))

	   ((and cont (eq? (node-get cont 'arginfo 'name) varname))
	    ;; This refers to our continuation argument, which acts
	    ;; more-or-less like a normal local.
	    (local-k cont
		     (make-node 'loc-continuation)))

	   ((memq-arginfo varname formals) =>
	    ;; This lambda defines this variable! Return the location
	    ;; of the argument in this lambda's argument list, as well
	    ;; as the arginfo structure for the argument, so that we
	    ;; can later update it with capture/mutation information.
	    (lambda (memq-tail)
	      (let ((position (- (length formals)
				 (length memq-tail))))
		(local-k (car memq-tail)
			 (make-node 'loc-argument 'index position)))))

	   (else
	    ;; We don't know. Ask our lexically-enclosing scope.
	    (search-lambda-stack #f ; we do NOT require an entry for a global here
				 (cdr lambda-stack)
				 (lambda ()
				   ;; It's a global variable. Insert an entry here only
				   ;; if we need to.
				   (if require-global-entry?
				       (node-set! lambda-node 'cps-lambda 'globals
						  (cons varname old-globals)))
				   (global-k))
				 (lambda (arginfo old-location)
				   ;; It's a captured variable. Recapture it here.
				   (let ((new-location (make-node 'loc-environment
								  'index (length old-captures))))
				     (arginfo-capture! arginfo)
				     (node-set! lambda-node 'cps-lambda 'captures
						(cons (make-node 'capture
								 'arginfo arginfo
								 'old-location old-location
								 'new-location new-location)
						      old-captures))
				     (local-k arginfo new-location))))))))))

(define (annotate-env lambda-stack node)
  (node-match node
	      ((cps-var name)
	       (find-location lambda-stack
			      name
			      (lambda () (make-node 'cps-global-get
						    'name name))
			      (lambda (arginfo location)
				(make-node 'cps-local-get
					   'name name
					   'arginfo arginfo
					   'location location))))
	      ((cps-set name expr)
	       (let ((expr (annotate-env lambda-stack expr)))
		 (find-location lambda-stack
				name
				(lambda () (make-node 'cps-global-set
						      'name name
						      'expr expr))
				(lambda (arginfo location)
				  (arginfo-mutate! arginfo)
				  (make-node 'cps-local-set
					     'name name
					     'arginfo arginfo
					     'location location
					     'expr expr)))))
	      ((cps-lambda cont formals varargs expr)
	       (node-set! node 'cps-lambda 'expr (annotate-env (cons node lambda-stack) expr))
	       node)
	      (else
	       (node-children-map! cps-child-attrs
				   (lambda (n) (annotate-env lambda-stack n))
				   node)
	       node)))
