(define eval
  (let ()
    (define undefined
      (lambda (rte) 17))

    (define (lookup-variable v cte k . fk)
      (let lookup-rib ((rib 0) (ribs cte))
	(if (null? ribs)
	    (if (null? fk)
		(error "r5rs-eval:unbound-variable" "Unbound variable" v)
		((car fk)))
	    (let* ((l (cdar ribs))
		   (global (caar ribs)))
	      (let lookup-i ((i 0) (l l))
		(cond
		 ((null? l) (lookup-rib (+ rib 1) (cdr ribs)))
		 ((eq? (car l) v) (k rib i global))
		 (else (lookup-i (+ i 1) (cdr l)))))))))

    (define (global? x cte)
      (lookup-variable x cte
		       (lambda (rib i global) global)
		       (lambda () #t)))

    (define (compile-combination c cte)
      (let ((rator (compile (car c) cte))
	    (rands (map (lambda (x) (compile x cte)) (cdr c))))
	(lambda (rte)
	  (apply (rator rte)
		 (map (lambda (x) (x rte)) rands)))))

    (define (analyze-formals formals k)
      (let loop ((formals formals) (acc '()))
	(cond
	 ((null? formals) (k (reverse acc) #f))
	 ((pair? formals) (loop (cdr formals) (cons (car formals) acc)))
	 (else (k (reverse (cons formals acc)) #t)))))

    (define (compile-recursive-definitions defs xs cte)
      (let* ((cte-rib (map car defs))
	     (new-cte (cons (cons #f cte-rib) cte))
	     (actual-thunks (map (lambda (def) (compile (cdr def) new-cte)) defs))
	     (body-thunk (compile `(begin ,@xs) new-cte))
	     (num-defines (length cte-rib)))
	(lambda (rte)
	  (let* ((rte-rib (make-vector num-defines 'undefined-internal-definition))
		 (new-rte (cons rte-rib rte)))
	    (do ((i 0 (+ i 1))
		 (actual-thunks actual-thunks (cdr actual-thunks)))
		((= i num-defines)
		 (body-thunk new-rte))
	      (vector-set! rte-rib i ((car actual-thunks) new-rte)))))))

    (define (compile-body xs cte)
      (let collect-defs ((defs '())
			 (xs xs))
	(if (or (null? xs)
		(not (and (pair? (car xs))
			  (symbol? (caar xs))
			  (global? (caar xs) cte))))
	    ;; We've run out of specials and defines.
	    (compile-recursive-definitions defs xs cte)
	    (case (caar xs)
	      ((begin) (collect-defs defs (append (cdar xs) (cdr xs))))
	      ((define) (let ((def (car xs))
			      (rest (cdr xs)))
			  (if (pair? (cadr def))
			      (let ((fnname (caadr def))
				    (fnargs (cdadr def))
				    (fnbody (cddr def)))
				(collect-defs (alist-cons fnname `(lambda ,fnargs ,@fnbody) defs)
					      rest))
			      (collect-defs (alist-cons (cadr def) (caddr def) defs)
					    rest))))
	      (else (compile-recursive-definitions defs xs cte))))))

    (define (compile-lambda x cte)
      (let ((formals (cadr x))
	    (body-exps (cddr x)))
	(analyze-formals
	 formals
	 (lambda (newrib isrest)
	   (let* ((body (compile-body body-exps (cons (cons #f newrib) cte)))
		  (argc (length newrib)))
	     (case argc
	       ((0) (if isrest
			(error "internal-error" "Should never have nullary rest procedure")
			(lambda (rte) (lambda () (body (cons (vector) rte))))))
	       ((1) (if isrest
			(lambda (rte) (lambda a (body (cons (vector a) rte))))
			(lambda (rte) (lambda (a) (body (cons (vector a) rte))))))
	       ((2) (if isrest
			(lambda (rte) (lambda (a . b) (body (cons (vector a b) rte))))
			(lambda (rte) (lambda (a b) (body (cons (vector a b) rte))))))
	       ((3) (if isrest
			(lambda (rte) (lambda (a b . c) (body (cons (vector a b c) rte))))
			(lambda (rte) (lambda (a b c) (body (cons (vector a b c) rte))))))
	       ((4) (if isrest
			(lambda (rte) (lambda (a b c . d) (body (cons (vector a b c d) rte))))
			(lambda (rte) (lambda (a b c d) (body (cons (vector a b c d) rte))))))
	       (else
		(if isrest
		    (let ((fixed-argc (- argc 1)))
		      (lambda (rte)
			(lambda r
			  (let ((v (make-vector argc)))
			    (do ((r r (cdr r))
				 (i 0 (+ i 1)))
				((= i fixed-argc) (vector-set! v i r))
			      (vector-set! v i (car r)))
			    (body (cons v rte))))))
		    (lambda (rte)
		      (lambda r
			(body (cons (list->vector r) rte))))))))))))

    (define (compile x cte)
      (cond
       ((symbol? x) (lookup-variable x cte
				     (lambda (rib i global)
				       (lambda (rte)
					 (vector-ref (list-ref rte rib) i)))
				     (lambda ()
				       (lambda (rte)
					 (let-primitive ((resolve-binding
							  (specific-method "Newmoon.Environment"
									   "ResolveBindingCell"
									   symbol
									   symbol))
							 (binding-value
							  (specific-method "Newmoon.Binding"
									   "getValue")))
					   (let* ((e (current-environment))
						  (c (resolve-binding e x 'global))
						  (v (binding-value c)))
					     v))))))
       ((not (pair? x)) (lambda (rte) x))
       ((not (and (symbol? (car x))
		  (global? (car x) cte)))
	(compile-combination x cte))
       (else
	(case (car x)
	  ((quote) (let ((v (cadr x))) (lambda (rte) v)))
	  ((define) (error "syntax-error"
			   "(eval) Internal definition in invalid position"
			   (and (not (null? (cdr x)))
				(cadr x))))
	  ((lambda) (compile-lambda x cte))
	  ((begin) (cond
		    ((null? (cdr x)) undefined)
		    ((null? (cddr x)) (compile (cadr x) cte))
		    (else (let ((head (compile (cadr x) cte))
				(tail (compile `(begin ,@(cddr x)) cte)))
			    (lambda (rte) (head rte) (tail rte))))))
	  ((if) (let ((test (compile (cadr x) cte))
		      (tbr (compile (caddr x) cte))
		      (fbr (if (null? (cdddr x)) undefined (compile (cadddr x) cte))))
		  (lambda (rte)
		    (if (test rte) (tbr rte) (fbr rte)))))
	  ((set!) (lookup-variable (cadr x) cte
				   (let ((val (compile (caddr x) cte)))
				     (lambda (rib i global)
				       (if global
					   (error "set-forbidden"
						  "May not set global variables"
						  (cadr x))
					   (lambda (rte)
					     (vector-set! (list-ref rte rib) i (val rte))))))))
	  ((#%extern-apply) (error "extern-apply-not-supported"
				   "(eval) doesn't support extern-apply"
				   x))
	  (else (compile-combination x cte))))))

    (lambda (x . env)
      (write x) (newline)
      (let* ((env (if (pair? env)
		      (car env)
		      (scheme-report-environment 5)))
	     (cte (car env))
	     (rte (cdr env))
	     (x (if (and (pair? x)
			 (string? (car x))
			 (string=? (car x) "noexpand"))
		    (cadr x)
		    (sc-expand x)))
	     (thunk (compile x cte)))
	(thunk rte)))))

(define null-environment
  (let ((e '(() . ())))
    (lambda (version)
      (if (= version 5)
          e
	  (error "unsupported-environment" "eval null-environment only supports R5RS")))))

(define scheme-report-environment
  (let-syntax ((make-rib (syntax-rules ()
			   ((_ prev name ...)
			    (let ((cte (car prev))
				  (rte (cdr prev)))
			      (cons (cons (list #t 'name ...) cte)
				    (cons (vector name ...) rte)))))))
    (let ((e (make-rib (null-environment 5)
		       eqv? eq? equal?
		       number? complex? real? rational? integer? exact? inexact?
		       = < > <= >= zero? positive? negative? odd? even?
		       max min + * - /
		       abs quotient remainder modulo gcd lcm numerator denominator
		       floor ceiling truncate round rationalize
		       exp log sin cos tan asin acos atan sqrt expt
		       make-rectangular make-polar real-part imag-part magnitude angle
		       exact->inexact inexact->exact
		       number->string string->number
		       not boolean?
		       pair? cons car cdr set-car! set-cdr! caar cadr cdar cddr
		       caaar caadr cadar caddr cdaar cdadr cddar cdddr
		       caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
		       cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
		       null? list? list length append reverse list-tail list-ref
		       memq memv member assq assv assoc
		       symbol? symbol->string string->symbol
		       char? char=? char<? char>? char<=? char>=?
		       char-ci=? char-ci<? char-ci>? char-ci<=? char-ci>=?
		       char-alphabetic? char-numeric? char-whitespace?
		       char-upper-case? char-lower-case?
		       char->integer integer->char char-upcase char-downcase
		       string? make-string string string-length string-ref string-set!
		       string=? string-ci=? string<? string>? string<=? string>=?
		       string-ci<? string-ci>? string-ci<=? string-ci>=?
		       substring string-append string->list list->string
		       string-copy string-fill!
		       vector? make-vector vector vector-length vector-ref vector-set!
		       vector->list list->vector vector-fill!
		       procedure? apply map for-each force
		       call-with-current-continuation
		       values call-with-values dynamic-wind
		       eval null-environment ;; scheme-report-environment
		       call-with-input-file call-with-output-file
		       input-port? output-port? current-input-port current-output-port
		       with-input-from-file with-output-to-file
		       open-input-file open-output-file close-input-port close-output-port
		       read read-char peek-char eof-object? char-ready?
		       write display newline write-char)))
      (letrec ((scheme-report-environment
		(lambda (version)
		  (if (= version 5)
		      (make-rib e scheme-report-environment)
		      (error "unsupported-environment"
			     "eval scheme-report-environment only supports R5RS")))))
	scheme-report-environment))))
