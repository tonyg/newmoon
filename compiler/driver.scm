;; (require (lib "pretty.ss"))

(define (sequence-phases datum phase-alist)
  (if (null? phase-alist)
      (begin
	(if (main$debug)
	    (begin
	      (display ";; Final phase result is ")
	      (write datum)
	      (newline)))
	datum)
      (let ((phase-name (caar phase-alist))
	    (phase-body (cadar phase-alist))
	    (rest (cdr phase-alist)))
	(if (main$debug)
	    (begin
	      (display ";;--------------------------------------------------")
	      (newline)
	      (display ";; Applying phase \"")
	      (display phase-name)
	      (display "\" to ")
	      (write datum)
	      (newline)))
	(sequence-phases (phase-body datum) rest))))

(define (compiler-front-end-phases expr)
  (sequence-phases
   expr
   `(("macro-expansion"				,sc-expand)
;;     ("dump"					,(lambda (x) (pretty-print x) (newline) x))
     ("toplevel begin-flattening"		,flatten-toplevel-begins)
     ("toplevel-rewriting"			,rewrite-toplevel-defines)
     ("parsing"					,(lambda (x) (make-lambda '() (parse '() x))))
     ("optimisation(1) removing head thunks"	,remove-thunk-in-head-position)
     ("cps-transform"				,cps-transform)
     ("optimisation(2) removing noop begins"	,remove-begin-head-noops)
     ("annotation"				,annotate-tree))))
