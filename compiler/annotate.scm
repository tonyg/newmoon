;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Annotation of parse tree environments.

;; Main entry point:
(define (annotate-tree cps-parse-tree)
  (annotate-env '() cps-parse-tree)
  cps-parse-tree)

(define (assoc-arginfo name l)
  (cond
   ((null? l) #f)
   ((eq? (arginfo-name (caar l)) name) (car l))
   (else (assoc-arginfo name (cdr l)))))

(define (member-arginfo name l)
  (cond
   ((null? l) #f)
   ((eq? (arginfo-name (car l)) name) l)
   (else (member-arginfo name (cdr l)))))

(define (find-location require-global-entry? lambda-stack varname)
  (if (null? lambda-stack)
      varname
      (let* ((lambda-node (caar lambda-stack))
	     (args (cdar lambda-stack))
	     (old-captures (or (node-get-or-false lambda-node 'captures) '()))
	     (old-globals (or (node-get-or-false lambda-node 'globals) '())))
	(cond

	 ((assoc-arginfo varname old-captures) =>
	  ;; We've already captured a variable of this name. Return
	  ;; the location we're storing the captured cell in (and
	  ;; the captured arginfo).
	  (lambda (cell)
	    (cons (cadr cell)
		  (car cell))))

	 ((memq varname old-globals)
	  ;; We've already checked, and none of our parents define
	  ;; or capture this variable - we know it's a global.
	  varname)

	 ((member-arginfo varname args) =>
	  ;; This branch of this case-lambda defines this variable!
	  ;; Return the negative of the ONE-based index of the
	  ;; argument in our argument list, as well as the arginfo
	  ;; structure for the argument, so that we can later update
	  ;; it with capture/mutation information.
	  (lambda (member-tail)
	    (let ((position (- (length args)
			       (length member-tail))))
	      (cons (- -1 position)
		    (car member-tail)))))

	 (else
	  ;; We don't know. Ask our lexically-enclosing scope.
	  (let ((old-location0 (find-location #f ; we do NOT require an entry for a global
					      (cdr lambda-stack)
					      varname)))
	    (if (pair? old-location0)
		;; It's a captured variable. Recapture it here.
		(let* ((old-location (car old-location0))
		       (arginfo (cdr old-location0))
		       (new-location (length old-captures)))
		  (arginfo-capture! arginfo)
		  (node-set! lambda-node 'lambda 'captures
			     (cons (list arginfo new-location old-location) old-captures))
		  (cons new-location
			arginfo))
		;; It's a global variable. Insert an entry here only
		;; if we need to.
		(begin
		  (if require-global-entry?
		      (node-set! lambda-node 'lambda 'globals
				 (cons varname old-globals)))
		  varname))))))))

(define (annotate-children lambda-stack node)
  (for-each (lambda (n) (annotate-env lambda-stack n))
	    (node-collect-subnodes node (node-child-attr-names node))))

(define (annotate-env lambda-stack node)
  (let ((kind-of-node (node-kind node)))
    (if (memq kind-of-node '(var set))
	(let ((location (find-location #t ; we require an entry for a global.
				       lambda-stack
				       (node-get node kind-of-node 'name))))
	  (if (pair? location)
	      (let ((location (car location))
		    (arginfo (cdr location)))
		(if (eq? kind-of-node 'set)
		    (arginfo-mutate! arginfo))
		(node-set! node kind-of-node 'location location)
		(node-set! node kind-of-node 'arginfo arginfo))
	      (node-set! node kind-of-node 'location location))))
    (if (eq? kind-of-node 'lambda)
	(for-each (lambda (arginfos body)
		    (annotate-env (cons (cons node arginfos) lambda-stack) body))
		  (node-get node 'lambda 'all-arginfos)
		  (node-get node 'lambda 'all-bodies))
	(annotate-children lambda-stack node))))
