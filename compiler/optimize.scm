;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Simple optimizations.

(define (remove-thunk-in-head-position node)
  (node-children-map! ds-child-attrs remove-thunk-in-head-position node)
  (node-match node
	      ((ds-apply rator rands)
	       (if (null? rands)
		   (node-match rator
			       ((ds-lambda formals varargs expr)
				(if (null? formals)
				    expr
				    node))
			       (else node))
		   node))
	      (else node)))

(define (remove-begin-head-noops node)
  (node-children-map! cps-child-attrs remove-begin-head-noops node)
  (node-match node
	      ((cps-begin head tail)
	       (case (node-kind head)
		 ((cps-lit cps-void cps-var cps-lambda) tail)
		 ((cps-begin) (compiler-assert
			       begin-should-never-be-head-of-itself
			       #f))
		 ((cps-apply cps-if cps-set cps-asm) node)
		 (else
		  (for-each display
			    (list ";; Warning: unknown node-type in remove-begin-head-noops: "
				  (node-kind head)
				  #\newline))
		  node)))
	      (else
	       node)))
