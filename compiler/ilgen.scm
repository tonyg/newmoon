(define-record-type list-table
  (make-list-table* table count)
  list-table?
  (table list-table-table-rev set-list-table-table-rev!)
  (count list-table-count set-list-table-count!))

(define (make-list-table)
  (make-list-table* '() 0))

(define (extend-list-table! t item)
  (let ((old-count (list-table-count t)))
    (let search ((table (list-table-table-rev t))
		 (n (- old-count 1)))
      (cond
       ((null? table)
	(set-list-table-table-rev! t (cons item (list-table-table-rev t)))
	(set-list-table-count! t (+ old-count 1))
	old-count)
       ((equal? (car table) item) n)
       (else (search (cdr table) (- n 1)))))))

(define (list-table-table t)
  (reverse (list-table-table-rev t)))

(define (generate-il current-back-end-name node)
  (let ((literal-table (make-list-table))
	(closure-table (make-list-table)))

    (define (gen-local-load loc)
      (make-node (case (@loc-local-source loc)
		   ((argument) @il-load-argument)
		   ((environment) @il-load-environment))
		 (@arginfo-name (@loc-local-arginfo loc))
		 (@loc-local-index loc)))

    (define (gen-get name location)
      (node-match location
	((@loc-local boxed?)
	 (if boxed?
	     (make-node @il-load-box (gen-local-load location))
	     (gen-local-load location)))
	((@loc-global)
	 (make-node @il-load-global name))))

    (define (gen-closure-invocation node)
      (make-node @il-closure
		 (gen-closure! node)
		 (map (lambda (cap) (gen-local-load (@capture-old-location cap)))
		      (@cps2-lambda-captures node))))

    (define (gen-asm formals actuals code)
      (make-node @il-asm
		 formals
		 (map gen actuals)
		 (cond
		  ((find (lambda (clause) (eq? current-back-end-name
					       (@backend-asm-name clause))) code)
		   => @backend-asm-code)
		  (else (error `(cps2-asm missing-backend-clause
					  ,current-back-end-name
					  ,(node->list node)))))))

    (define (gen-backend backend-name arguments)
      (if (eq? backend-name current-back-end-name)
	  (make-node @il-backend arguments)
	  (make-node @il-void)))

    (define (gen-local-store loc instr)
      (make-node (case (@loc-local-source loc)
		   ((argument) @il-store-argument)
		   ((environment) @il-store-environment))
		 (@arginfo-name (@loc-local-arginfo loc))
		 (@loc-local-index loc)
		 instr))

    (define (gen-set name location expr)
      (node-match location
	((@loc-local boxed?)
	 (if boxed?
	     (make-node @il-store-box (gen-local-load location) (gen expr))
	     (gen-local-store location (gen expr))))
	((@loc-global)
	 (make-node @il-store-global name (gen expr)))))

    (define (gen-begin head tail)
      (make-node @il-begin (gen head) (gen tail)))

    (define (gen-if test true false)
      (make-node @il-if (gen test) (gen true) (gen false)))

    (define (gen-apply cont rator rands)
      (make-node @il-apply cont (gen rator) (map gen rands)))

    (define (gen node)
      (node-match node
	((@cps2-lit value) (make-node @il-lit (extend-list-table! literal-table value)))
	((@cps2-void) (make-node @il-void))
	((@cps2-get name location) (gen-get name location))
	((@cps2-lambda) (gen-closure-invocation node))
	((@cps2-asm formals actuals code) (gen-asm formals actuals code))
	((@cps2-backend backend-name arguments) (gen-backend backend-name arguments))
	((@cps2-set name location expr) (gen-set name location expr))
	((@cps2-value-begin head tail)
	 ;; are these even permitted?
	 (error 'yes-in-cases-of-set-in-head-or-similar))
	((@cps2-value-if test true false) (gen-if test true false))
	((@cps2-apply cont rator rands) (gen-apply cont rator rands))
	((@cps2-exp-begin head tail) (gen-begin head tail))
	((@cps2-exp-if test true false) (gen-if test true false))))

    (define (gen-closure! node)
      (node-match node
	((@cps2-lambda is-continuation formals captures varargs expr globals)
	 (let ((box-instrs (filter-map (lambda (loc)
					 (and (@loc-local-boxed? loc)
					      (make-node @il-install-box
							 (@arginfo-name (@loc-local-arginfo loc))
							 (@loc-local-index loc))))
				       formals)))
	   (extend-list-table! closure-table
			       (make-node @il-closure-template
					  is-continuation
					  (map @loc-local-arginfo formals)
					  varargs
					  (map @capture-arginfo captures)
					  globals
					  (append box-instrs (list (gen expr)))))))))

    (let ((root-closure-index (gen-closure! node)))
      (make-node @il-program
		 (list-table-table literal-table)
		 (list-table-table closure-table)
		 root-closure-index))))
