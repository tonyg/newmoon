;; (require (lib "pretty.ss"))

(define all-languages
  `(
    (scheme
     #t)

    (assembly (%list-of (backend-asm (name ,symbol?) (code #t))))

    (arginfo
     (arginfo (name ,symbol?) (captured ,boolean?) (mutated ,boolean?) (is-rest ,boolean?)))

    (core-scheme
     (%or
      (lit (value #t))
      (var (name ,symbol?))
      (lambda (formals #t) (varargs ,boolean?) (body (%list-of core-scheme)))
      (apply (rator core-scheme) (rands (%list-of core-scheme)))
      (begin (exprs (%list-of core-scheme)))
      (if (test core-scheme) (true core-scheme) (false core-scheme))
      (set (name ,symbol?) (expr core-scheme))
      (define (name ,symbol?) (expr core-scheme))
      (asm (formals (%list-of ,symbol?)) (actuals (%list-of core-scheme)) (code assembly))
      ))

    (ds
     (%or
      (ds-lit (value #t))
      (ds-var (name ,symbol?))
      (ds-lambda (formals (%list-of arginfo)) (varargs ,boolean?) (expr ds))
      (ds-apply (rator ds) (rands (%list-of ds)))
      (ds-begin (head ds) (tail ds))
      (ds-if (test ds) (true ds) (false ds))
      (ds-set (name ,symbol?) (expr ds))
      (ds-asm (formals (%list-of ,symbol?)) (actuals (%list-of ds)) (code assembly))
      ))

    (cps-value
     (%or
      (cps-lit (value #t))
      (cps-var (name ,symbol?))
      (cps-lambda (formals (%list-of arginfo)) (varargs ,boolean?) (expr cps-exp))
      (cps-asm (formals (%list-of ,symbol?)) (actuals (%list-of cps-value)) (code assembly))
      (cps-set (name ,symbol?) (expr cps-value))

      (cps-begin (head cps-value) (tail cps-value))
      (cps-if (test cps-value) (true cps-value) (false cps-value))
      ))

    (cps-exp
     (%or
      (cps-apply (rator cps-value) (rands (%list-of cps-value)))
      (cps-begin (head cps-value) (tail cps-exp))
      (cps-if (test cps-value) (true cps-exp) (false cps-exp))
      ))

    (local-location
     (%or
      (loc-argument (index ,number?))
      (loc-environment (index ,number?))))

    (capture-record
     (capture (arginfo arginfo) (old-location local-location) (new-location local-location)))

    (cps2-value
     (%or
      (cps-lit (value #t))
      (cps-local-get (name ,symbol?) (arginfo arginfo) (location local-location))
      (cps-global-get (name ,symbol?))
      (cps-lambda (formals (%list-of arginfo))
		  (varargs ,boolean?)
		  (captures (%list-of capture-record))
		  (globals (%list-of ,symbol?))
		  (expr cps2-exp))
      (cps-asm (formals (%list-of ,symbol?)) (actuals (%list-of cps2-value)) (code assembly))
      (cps-local-set (name ,symbol?) (arginfo arginfo) (location local-location) (expr cps2-value))
      (cps-global-set (name ,symbol?) (expr cps2-value))

      (cps-begin (head cps2-value) (tail cps2-value))
      (cps-if (test cps2-value) (true cps2-value) (false cps2-value))
      ))

    (cps2-exp
     (%or
      (cps-apply (rator cps2-value) (rands (%list-of cps2-value)))
      (cps-begin (head cps2-value) (tail cps2-exp))
      (cps-if (test cps2-value) (true cps2-exp) (false cps2-exp))
      ))
    ))

(define (core-scheme-child-attrs node)
  (case (node-kind node)
    ((lit var) '())
    ((lambda) 'body)
    ((apply) '(rator . rands))
    ((begin) 'exprs)
    ((if) '(test true false))
    ((set define) '(expr))
    ((asm) 'actuals)
    (else (error "unknown node kind in core-scheme-child-attrs:" node))))

(define (ds-child-attrs node)
  (case (node-kind node)
    ((ds-lit ds-var) '())
    ((ds-lambda) '(expr))
    ((ds-apply) '(rator . rands))
    ((ds-begin) '(head tail))
    ((ds-if) '(test true false))
    ((ds-set) '(expr))
    ((ds-asm) 'actuals)
    (else (error "unknown node kind in ds-child-attrs:" node))))

(define (cps-child-attrs node)
  (case (node-kind node)
    ((cps-lit cps-var) '())
    ((cps-lambda) '(expr))
    ((cps-asm) 'actuals)
    ((cps-set) '(expr))
    ((cps-apply) '(rator . rands))
    ((cps-begin) '(head tail))
    ((cps-if) '(test true false))
    (else (error "unknown node kind in cps-child-attrs:" node))))

(define (cps2-child-attrs node)
  (case (node-kind node)
    ((cps-lit cps-local-get cps-global-get) '())
    ((cps-lambda) '(expr))
    ((cps-asm) 'actuals)
    ((cps-local-set cps-global-set) '(expr))
    ((cps-apply) '(rator . rands))
    ((cps-begin) '(head tail))
    ((cps-if) '(test true false))
    (else (error "unknown node kind in cps2-child-attrs:" node))))

(define (sequence-phases datum phase-alist)
  (if (null? phase-alist)
      (begin
	(if (main$debug)
	    (begin
	      (display ";; Final phase result is ")
	      (write (node->list datum))
	      (newline)))
	datum)
      (let* ((entry (car phase-alist))
	     (phase-name (car entry))
	     (phase-prelanguage (cadr entry))
	     (phase-body (caddr entry))
	     (phase-postlanguage (cadddr entry))
	     (rest (cdr phase-alist)))
	(if (main$debug)
	    (begin
	      (display ";;--------------------------------------------------")
	      (newline)
	      (display ";; Applying phase \"")
	      (display phase-name)
	      (display "\" to ")
	      (write (node->list datum))
	      (newline)))
	(if (not (check-language datum phase-prelanguage all-languages type-error))
	    (error (string-append "Failed precondition for phase \"" phase-name "\"")
		   datum))
	(let ((new-datum (phase-body datum)))
	  (if (not (check-language new-datum phase-postlanguage all-languages type-error))
	      (error (string-append "Failed postcondition for phase \"" phase-name "\"")
		     new-datum))
	  (sequence-phases new-datum rest)))))

(define (compiler-front-end-phases expr)
  (sequence-phases
   expr
   `(("macro-expansion"				scheme ,macro-expand core-scheme)
     ;;("dump"					#t ,(lambda (x) (pretty-print x) (newline) x) #t)
     ("toplevel begin-flattening"		core-scheme ,flatten-begins core-scheme)
     ("toplevel-rewriting"			core-scheme ,rewrite-toplevel-defines core-scheme)
     ("internal-definition-rewriting"		core-scheme
						,(lambda (x)
						   (make-node 'ds-lambda
						     'formals '()
						     'varargs #f
						     'expr (rewrite-internal-definitions x)))
						ds)
     ("optimisation(1) removing head thunks"	ds ,remove-thunk-in-head-position ds)
     ("cps-transform"				ds ,cps-transform cps-value)
     ("optimisation(2) removing noop begins"	cps-value ,remove-begin-head-noops cps-value)
     ("annotation"				cps-value ,annotate-tree cps2-value))))
