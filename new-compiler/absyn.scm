;; Node type definitions

(define (valid-literal? x)
  (not (procedure? x)))

(define-language %assembly
  (@backend-asm (name symbol?) (code %any)))

(define %assembly (%list-of @backend-asm))

(define-node-type @arginfo
  (name symbol?)
  (cont boolean?)
  (is-rest boolean?))

(define (make-arginfo is-cont name)
  (make-node @arginfo name is-cont #f))

(define (make-normal-arginfo n)
  (make-arginfo #f n))

(define-language %core-scheme
  (@core-lit (value valid-literal?))
  (@core-void)
  (@core-var (name symbol?))
  (@core-lambda (formals %any) (varargs boolean?) (body (%list-of %core-scheme)))
  (@core-apply (rator %core-scheme) (rands (%list-of %core-scheme)))
  (@core-begin (exprs (%list-of %core-scheme)))
  (@core-if (test %core-scheme) (true %core-scheme) (false %core-scheme))
  (@core-set (name symbol?) (expr %core-scheme))
  (@core-define (name symbol?) (expr %core-scheme))
  (@core-asm (formals (%list-of symbol?)) (actuals (%list-of %core-scheme)) (code %assembly))
  (@core-backend (backend-name symbol?) (arguments (%list-of %any))))

(define-language %ds
  (@ds-lit (value valid-literal?))
  (@ds-void)
  (@ds-var (name symbol?))
  (@ds-lambda (formals (%list-of @arginfo)) (varargs boolean?) (expr %ds))
  (@ds-apply (rator %ds) (rands (%list-of %ds)))
  (@ds-begin (head %ds) (tail %ds))
  (@ds-if (test %ds) (true %ds) (false %ds))
  (@ds-set (name symbol?) (expr %ds))
  (@ds-asm (formals (%list-of symbol?)) (actuals (%list-of %ds)) (code %assembly))
  (@ds-backend (backend-name symbol?) (arguments (%list-of %any))))

(define (ds-child-attrs node)
  (case (node-type-name (node-type node))
    ((@ds-lambda) '(expr))
    ((@ds-apply) '(rator . rands))
    ((@ds-begin) '(head tail))
    ((@ds-if) '(test true false))
    ((@ds-set) '(expr))
    ((@ds-asm) 'actuals)
    (else '())))

(define-language %cps-value
  (@cps-lit (value valid-literal?))
  (@cps-void)
  (@cps-var (name symbol?))
  (@cps-lambda (cont (%optional @arginfo))
	       (formals (%list-of @arginfo))
	       (varargs boolean?)
	       (expr %cps-exp)
	       (expr-references (%set-of symbol?))
	       (expr-updates (%set-of symbol?)))
  (@cps-asm (formals (%list-of symbol?)) (actuals (%list-of %cps-value)) (code %assembly))
  (@cps-backend (backend-name symbol?) (arguments (%list-of %any)))
  (@cps-set (name symbol?) (expr %cps-value))
  (@cps-value-begin (head %cps-value) (tail %cps-value))
  (@cps-value-if (test %cps-value) (true %cps-value) (false %cps-value)))

(define-language %cps-exp
  (@cps-apply (cont boolean?) (rator %cps-value) (rands (%list-of %cps-value)))
  (@cps-exp-begin (head %cps-value) (tail %cps-exp))
  (@cps-exp-if (test %cps-value) (true %cps-exp) (false %cps-exp)))

(define (cps-child-attrs node)
  ;; Icky: treats %cps-value and %cps-exp together
  (case (node-type-name (node-type node))
    ((@cps-lambda) '(expr))
    ((@cps-asm) 'actuals)
    ((@cps-set) '(expr))
    ((@cps-value-begin) '(head tail))
    ((@cps-value-if) '(test true false))
    ((@cps-apply) '(rator . rands))
    ((@cps-exp-begin) '(head tail))
    ((@cps-exp-if) '(test true false))
    (else '())))

(define-language %location
  (@loc-local (source (lambda (v) (memq v '(argument environment))))
	      (arginfo @arginfo)
	      (captured boolean?)
	      (mutable boolean?)
	      (index number?))
  (@loc-global))

(define (@loc-local-boxed? loc)
  (and (@loc-local-captured loc)
       (@loc-local-mutable loc)
       (begin
	 (compiler-assert continuation-arginfo-never-boxed
			  (not (@arginfo-cont (@loc-local-arginfo loc))))
	 #t)))

(define-node-type @capture (old-location %location) (new-location %location))

(define (@capture-arginfo c)
  (@loc-local-arginfo (@capture-new-location c)))

(define-language %cps2-value
  (@cps2-lit (value valid-literal?))
  (@cps2-void)
  (@cps2-get (name symbol?) (location %location))
  (@cps2-lambda (is-continuation boolean?)
		(formals (%list-of %location))
		(varargs boolean?)
		(expr %cps2-exp)
		(captures (%list-of @capture))
		(globals (%set-of symbol?)))
  (@cps2-asm (formals (%list-of symbol?)) (actuals (%list-of %cps2-value)) (code %assembly))
  (@cps2-backend (backend-name symbol?) (arguments (%list-of %any)))
  (@cps2-set (name symbol?) (location %location) (expr %cps2-value))
  (@cps2-value-begin (head %cps2-value) (tail %cps2-value))
  (@cps2-value-if (test %cps2-value) (true %cps2-value) (false %cps2-value)))

(define-language %cps2-exp
  (@cps2-apply (cont boolean?) (rator %cps2-value) (rands (%list-of %cps2-value)))
  (@cps2-exp-begin (head %cps2-value) (tail %cps2-exp))
  (@cps2-exp-if (test %cps2-value) (true %cps2-exp) (false %cps2-exp)))
