;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Simple optimizations.

(define (remove-thunk-in-head-position ast)
  (node-tree-map! (lambda (node)
		    (node-match node
				((apply rator rands)
				 (node-match rator
					     ((lambda args body)
					      (if (and (null? rands)
						       (null? args))
						  body
						  node))
					     (else
					      node)))
				(else
				 node)))
		  ast))

(define (remove-begin-head-noops ast)
  (node-tree-map! (lambda (node)
		    (node-match node
				((begin head tail)
				 (case (node-kind head)
				   ((lit singleton var lambda) tail)
				   ((begin) (compiler-assert
					     begin-should-never-be-head-of-itself
					     #f))
				   ((apply if set extern-apply) node)
				   (else
				    (for-each display
					      (list ";; Warning: unknown node-type in "
						    "remove-begin-head-noops: "
						    (node-kind head)
						    #\newline)))))
				(else
				 node)))
		  ast))
