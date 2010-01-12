;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Simple optimizations.

(define (remove-thunk-in-head-position node)
  (node-fold ds-child-attrs
	     (lambda (node k)
	       (k (node-match node
		    ((@ds-apply rator rands)
		     (if (null? rands)
			 (node-match rator
			   ((@ds-lambda formals expr)
			    (if (null? formals)
				expr
				node))
			   (else node))
			 node))
		    (else node))))
	     node))

(define (prune-head-noop node head tail)
  (node-match head
    ((@cps-lit) tail)
    ((@cps-void) tail)
    ((@cps-var) tail)
    ((@cps-lambda) tail)
    ((@cps-value-begin) (compiler-assert begin-should-never-be-head-of-itself #f))
    ((@cps-exp-begin) (compiler-assert begin-should-never-be-head-of-itself #f))
    (else node)))

(define (remove-begin-head-noops node)
  (node-fold cps-child-attrs
	     (lambda (node k)
	       (k (node-match node
		    ((@cps-value-begin head tail) (prune-head-noop node head tail))
		    ((@cps-exp-begin head tail) (prune-head-noop node head tail))
		    (else node))))
	     node))
