;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Simple optimizations.

(define (remove-thunk-in-head-position ast)
  (node-tree-map! (lambda (node)
		    (node-match node
				((apply rator rands)
				 (if (null? rands)
				     (node-match rator
						 ((lambda all-arginfos all-bodies)
						  (let loop ((all-arginfos all-arginfos)
							     (all-bodies all-bodies))
						    (cond
						     ((null? all-arginfos) node)
						     ((null? (car all-arginfos)) (car all-bodies))
						     (else (loop (cdr all-arginfos)
								 (cdr all-bodies))))))
						 (else node))
				     node))
				(else node)))
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
				   ((apply if set jvm-assemble) node)
				   (else
				    (for-each display
					      (list ";; Warning: unknown node-type in "
						    "remove-begin-head-noops: "
						    (node-kind head)
						    #\newline)))))
				(else
				 node)))
		  ast))
