; Basic syntax of core scheme.
;---------------------------------------------------------------------------

; There are a few essential procedures required before we can set up
; the basic macros.

; Note that sys$install-binding is inserted by the compiler as a
; primitive!

(sys$install-binding '%define-global-variable 'global
		     (lambda (name value)
		       (sys$install-binding name 'global value)))

(sys$install-binding '%define-macro-transformer 'global
		     (lambda (name kind transformer)
		       (sys$install-binding name 'macro (cons kind transformer))))

(define (null? x)
  (%jvm-assemble (x) (x)
    ($ x)
    (getstatic "sisc.data.EmptyList" "EMPTYLIST" "sisc.data.EmptyList")
    (obj-cond eq ldtrue)
    (getstatic "sisc.data.SchemeBoolean" "FALSE" "sisc.data.SchemeBoolean")
    (goto done)
    ldtrue
    (getstatic "sisc.data.SchemeBoolean" "TRUE" "sisc.data.SchemeBoolean")
    done))

(define (pair? x)
  (%jvm-assemble (x) (x)
    ($ x)
    (instanceof "sisc.data.Pair")
    (int-cond !=0 ldtrue)
    (getstatic "sisc.data.SchemeBoolean" "FALSE" "sisc.data.SchemeBoolean")
    (goto done)
    ldtrue
    (getstatic "sisc.data.SchemeBoolean" "TRUE" "sisc.data.SchemeBoolean")
    done))

(define (cons a d)
  (%jvm-assemble (a d) (a d)
    (new "sisc.data.Pair")
    (dup 1)
    ($ a)
    ($ d)
    (invoke "sisc.data.Pair" "<init>" void (object object) special)))

(define (car x)
  (%jvm-assemble (x) (x)
    ($ x)
    (check-cast "sisc.data.Pair")
    (getfield "sisc.data.Pair" "car" object)))
  
(define (cdr x)
  (%jvm-assemble (x) (x)
    ($ x)
    (check-cast "sisc.data.Pair")
    (getfield "sisc.data.Pair" "cdr" object)))

(define (vector? x)
  (%jvm-assemble (x) (x)
    ($ x)
    (instanceof "sisc.data.SchemeVector")
    (int-cond !=0 ldtrue)
    (getstatic "sisc.data.SchemeBoolean" "FALSE" "sisc.data.SchemeBoolean")
    (goto done)
    ldtrue
    (getstatic "sisc.data.SchemeBoolean" "TRUE" "sisc.data.SchemeBoolean")
    done))

(define (symbol? x)
  (%jvm-assemble (x) (x)
    ($ x)
    (instanceof "sisc.data.SchemeString")
    (int-cond !=0 ldtrue)
    (getstatic "sisc.data.SchemeBoolean" "FALSE" "sisc.data.SchemeBoolean")
    (goto done)
    ldtrue
    (getstatic "sisc.data.SchemeBoolean" "TRUE" "sisc.data.SchemeBoolean")
    done))

(define (eq? x y)
  (%jvm-assemble (x y) (x y)
    ($ x)
    ($ y)
    (obj-cond eq eq-true)
    (getstatic "sisc.data.SchemeBoolean" "FALSE" "sisc.data.SchemeBoolean")
    (goto eq-done)
    eq-true
    (getstatic "sisc.data.SchemeBoolean" "TRUE" "sisc.data.SchemeBoolean")
    eq-done))

(define (= x y)
  (%jvm-assemble (x y) (x y)
    ($ x)
    (invoke "sisc.util.Util" "num" "sisc.data.Quantity" ("sisc.data.Value") static)
    ($ y)
    (invoke "sisc.util.Util" "num" "sisc.data.Quantity" ("sisc.data.Value") static)
    (const 0)
    (invoke "sisc.data.Quantity" "comp" boolean ("sisc.data.Quantity" int) virtual)
    (invoke "sisc.util.Util" "truth" "sisc.data.SchemeBoolean" (boolean) static)))

(define (> x y)
  (%jvm-assemble (x y) (x y)
    ($ x)
    (invoke "sisc.util.Util" "num" "sisc.data.Quantity" ("sisc.data.Value") static)
    ($ y)
    (invoke "sisc.util.Util" "num" "sisc.data.Quantity" ("sisc.data.Value") static)
    (const 1)
    (invoke "sisc.data.Quantity" "comp" boolean ("sisc.data.Quantity" int) virtual)
    (invoke "sisc.util.Util" "truth" "sisc.data.SchemeBoolean" (boolean) static)))

(define (+ x y)
  (%jvm-assemble (x y) (x y)
    ($ x)
    (invoke "sisc.util.Util" "num" "sisc.data.Quantity" ("sisc.data.Value") static)
    ($ y)
    (invoke "sisc.util.Util" "num" "sisc.data.Quantity" ("sisc.data.Value") static)
    (invoke "sisc.data.Quantity" "add" "sisc.data.Quantity" ("sisc.data.Quantity") virtual)))

(define (length x)
  (let loop ((x x) (count 0))
    (if (null? x)
	count
	(loop (cdr x) (+ count 1)))))

(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))
(define (caaar x) (car (car (car x))))
(define (caadr x) (car (car (cdr x))))
(define (cadar x) (car (cdr (car x))))
(define (caddr x) (car (cdr (cdr x))))
(define (cdaar x) (cdr (car (car x))))
(define (cdadr x) (cdr (car (cdr x))))
(define (cddar x) (cdr (cdr (car x))))
(define (cdddr x) (cdr (cdr (cdr x))))
(define (caaaar x) (car (car (car (car x)))))
(define (caaadr x) (car (car (car (cdr x)))))
(define (caadar x) (car (car (cdr (car x)))))
(define (caaddr x) (car (car (cdr (cdr x)))))
(define (cadaar x) (car (cdr (car (car x)))))
(define (cadadr x) (car (cdr (car (cdr x)))))
(define (caddar x) (car (cdr (cdr (car x)))))
(define (cadddr x) (car (cdr (cdr (cdr x)))))
(define (cdaaar x) (cdr (car (car (car x)))))
(define (cdaadr x) (cdr (car (car (cdr x)))))
(define (cdadar x) (cdr (car (cdr (car x)))))
(define (cdaddr x) (cdr (car (cdr (cdr x)))))
(define (cddaar x) (cdr (cdr (car (car x)))))
(define (cddadr x) (cdr (cdr (car (cdr x)))))
(define (cdddar x) (cdr (cdr (cdr (car x)))))
(define (cddddr x) (cdr (cdr (cdr (cdr x)))))

(define (list . args) args)

(define (fold1-right kons knil lis1)
  (((lambda (recur)
      (set! recur (lambda (lis)
		    (if (null? lis)
			knil
			((lambda (head)
			   (kons head (recur (cdr lis))))
			 (car lis)))))
      recur)
    #f)
   lis1))

(define (append . ls)
  (fold1-right (lambda (l acc)
		 (fold1-right cons acc l))
	       '()
	       ls))

(begin-for-syntax
 (define (map1 f lst)
   (if (null? lst)
       '()
       (cons (f (car lst)) (map1 f (cdr lst))))))

(define (for-each1 f lst)
  (if (null? lst)
      #t
      (begin
	(f (car lst))
	(for-each1 f (cdr lst)))))

;; These are the core macros.

(begin-for-syntax
 (define (sys$process-quasiquote exp depth)
   (if (vector? exp)
       (list 'list->vector (sys$process-quasiquote (vector->list exp) depth))
       (if (pair? exp)
	   (if (eq? (car exp) 'quasiquote)
	       (list 'list ''quasiquote (sys$process-quasiquote (cadr exp) (+ depth 1)))
	       (if (eq? (car exp) 'unquote)
		   (if (= depth 1)
		       (cadr exp)
		       (list 'list ''unquote (sys$process-quasiquote (cadr exp) (- depth 1))))
		   (if (if (pair? (car exp))
			   (eq? (caar exp) 'unquote-splicing)
			   #f)
		       (if (= depth 1)
			   (list 'append (cadar exp) (sys$process-quasiquote (cdr exp) depth))
			   (list 'cons
				 (sys$process-quasiquote (car exp) (- depth 1))
				 (sys$process-quasiquote (cdr exp) depth)))
		       (list 'cons
			     (sys$process-quasiquote (car exp) depth)
			     (sys$process-quasiquote (cdr exp) depth)))))
	   (list 'quote exp)))))

(defmacro quasiquote (quoted-list)
  (sys$process-quasiquote quoted-list 1))

(begin-for-syntax
 (define (sys$let-transformer . args)
   (if (symbol? (car args))
       ((lambda (loopname names inits body)
	  `((let ((,loopname #f))
	      (set! ,loopname (lambda ,names ,@body))
	      ,loopname)
	    ,@inits))
	(car args)
	(map1 car (cadr args))
	(map1 cadr (cadr args))
	(cddr args))
       ((lambda (names inits body)
	  `((lambda ,names ,@body) ,@inits))
	(map1 car (car args))
	(map1 cadr (car args))
	(cdr args)))))

(defmacro let args
  (apply sys$let-transformer args))

(defmacro let* (bindings . body)
  (if (null? bindings)
      `(let () ,@body)
      `(let (,(car bindings)) (let* ,(cdr bindings) ,@body))))

(defmacro letrec (bindings . body)
  (if (null? bindings)
      `(let () ,@body)
      `(let ()
	 ,@(map1 (lambda (binding)
		   `(define ,(car binding) ,(cadr binding)))
		 bindings)
	 (let () ,@body))))

(defmacro and exprs
  (if (null? exprs)
      #t
      (if (null? (cdr exprs))
	  (car exprs)
	  `(if ,(car exprs)
	       ,(cons 'and (cdr exprs))
	       #f))))

(defmacro or exprs
  (if (null? exprs)
      #f
      (if (null? (cdr exprs))
	  (car exprs)
	  (let ((valuesym (gensym "or-gensym")))
	    `(let ((,valuesym ,(car exprs)))
	       (if ,valuesym
		   ,valuesym
		   ,(cons 'or (cdr exprs))))))))

(defmacro cond clauses
  (if (null? clauses)
      `(begin)
      (let ((clause (car clauses))
	    (rest (cdr clauses)))
	(if (eq? (car clause) 'else)
	    `(begin ,@(cdr clause))
	    (if (and (> (length clause) 2)
		     (eq? (cadr clause) '=>)) ;; unhygienic :-(
		(let ((test-expr (car clause))
		      (func-expr (caddr clause))
		      (temp-id (gensym "cond-gensym")))
		  `(let ((,temp-id ,test-expr))
		     (if ,temp-id
			 (,func-expr ,temp-id)
			 (cond ,@rest))))
		(if (null? (cdr clause))
		    `(or ,(car clause)
			 (cond ,@rest))
		    `(if ,(car clause)
			 (begin ,@(cdr clause))
			 (cond ,@rest))))))))

(defmacro do (vardefs terminator . body)
  (let ((loopsym (gensym "do-gensym")))
    `(let ,loopsym ,(map1 (lambda (x) (drop-right x 1)) vardefs)
	  (cond ,terminator
		(else ,@body
		      (,loopsym ,@(map1 last vardefs)))))))

(defmacro case (test-expr . clauses)
  (let ((test-expr-id (gensym "case-gensym")))
    `(let ((,test-expr-id ,test-expr))
       (cond
	,@(map (lambda (clause)
		 (if (eq? (car clause) 'else)
		     clause
		     `((memv ,test-expr-id ',(car clause))
		       ,@(cdr clause))))
	       clauses)))))

(define (memq key lst)
  (cond
   ((null? lst) #f)
   ((eq? (car lst) key) lst)
   (else
    (member key (cdr lst)))))

(define (reverse lst)
  (let loop ((acc '()) (lst lst))
    (if (null? lst)
	acc
	(loop (cons (car lst) acc) (cdr lst)))))

(define (not x) (if x #f #t))

(define (make-parameter value)
  (lambda x
    (if (null? x)
	value
	(let ((old value))
	  (set! value (car x))
	  old))))

(define (dynamic-wind w thunk u) ;; %%% completely bogus
  (begin
    (w)
    (call-with-values thunk
      (lambda results
	(u)
	(apply values results)))))

(define-syntax parameterize
  (lambda (x)
    (syntax-case x ()
      ((_ ((param newvalexp) ...) body ...)
       (with-syntax (((oldval ...) (generate-temporaries (syntax (param ...))))
		     ((newval ...) (generate-temporaries (syntax (param ...)))))
	 (syntax (let ((oldval #f) ...)
		   (let ((newval newvalexp) ...)
		     (dynamic-wind
			 (lambda ()
			   (begin (set! oldval (param)) ...)
			   (begin (param newval) ...))
			 (lambda ()
			   body ...)
			 (lambda ()
			   (begin (set! newval (param)) ...)
			   (begin (param oldval) ...)))))))))))

(define-primitive %%symbol-split (specific-method symbol "Split" "System.Char[]"))
(define-primitive %%string-char-array (specific-method string "GetCharArray"))
(define (string-split s charstr)
  (map (lambda (x) (symbol->string x))
       (vector->list (%%symbol-split (string->symbol s) (%%string-char-array charstr)))))

(define-primitive %%getenv (specific-static "System.Environment" "GetEnvironmentVariable" symbol))

(define-primitive %%load-void (assembler "LdnullAssembler"))
(define (void-guard v g)
  (if (eq? (%%load-void) v)
      g
      v))

(define library-search-path
  (make-parameter
   (let* ((var (%%getenv (string->symbol "NEWMOON_LIBPATH")))
	  (strs (string-split (symbol->string (void-guard var
							  (string->symbol
							   "/Users/tonyg/src/newmoon/lib")))
			      ":")))
     strs)))

(define-primitive %%array-create-instance
  (specific-static "System.Array" "CreateInstance" "System.Type" int))
(define (specialise-vector t v)
  (let* ((len (vector-length v))
	 (v2 (%%array-create-instance t len)))
    (do ((i 0 (+ i 1)))
	((fx= i len) v2)
      (vector-set! v2 i (vector-ref v i)))))
(define (list->specialised-vector t l)
  (let* ((len (length l))
	 (v2 (%%array-create-instance t len)))
    (do ((i 0 (+ i 1))
	 (l l (cdr l)))
	((fx= i len) v2)
      (vector-set! v2 i (car l)))))

(define-primitive %%lookup-type (specific-static "System.Type" "GetType" symbol))
(define (lookup-type str)
  (%%lookup-type (->symbol str)))

(define-primitive string-join (specific-static string "Join" list string))

(define (string-append . strings)
  (string-join strings ""))

(define-primitive %%file-exists? (specific-static "System.IO.File" "Exists" symbol))
(define (file-exists? x)
  (%%file-exists? (->symbol x)))

(define (resolve-library-path filename collection-path)
  (let* ((cpath (if (null? collection-path)
		    "newmoon-lib/"
		    (string-append (string-join collection-path "/") "/")))
	 (frag (string-append cpath filename)))
    (let loop ((p (library-search-path)))
      (if (null? p)
	  (error "Could not resolve-library-path" filename collection-path)
	  (let* ((path (car p))
		 (rest (cdr p))
		 (f (string-append path "/" frag)))
	    ;;(display (string-append ";; newmoon resolve-library-path trying "f" ..."))
	    ;;(newline)
	    (if (file-exists? f)
		f
		(loop rest)))))))

(define-syntax receive
  (syntax-rules ()
    ((_ (var ...) expr body ...)
     (call-with-values expr
       (lambda (var ...)
	 body ...)))))

(define (check-arg pred val caller)
  (let lp ((val val))
    (if (pred val) val (lp (error "Bad argument" val pred caller)))))

(define-syntax :optional
  (syntax-rules ()
    ((_ restvar def pred)
     (if (null? restvar)
	 def
	 (check-arg pred (car rest-var) ':optional)))
    ((_ restvar def)
     (if (null? restvar)
	 def
	 (car restvar)))))

(define-syntax let-optionals
  (syntax-rules ()
    ((_ restvar () body ...)
     (let ()
       body ...))

    ((_ restvar ((var def)) body ...)
     (let ((var (:optional restvar def)))
       body ...))

    ((_ restvar ((var def) more ...) body ...)
     (let ((var (:optional restvar def))
	   (newrest (if (null? restvar) restvar (cdr restvar))))
       (let-optionals newrest (more ...) body ...)))))

(define-syntax let-optionals*
  (syntax-rules ()
    ((_ restvar () body ...)
     (let ()
       body ...))

    ((_ restvar (((var ...) producer)) body ...)
     (receive (var ...) (producer restvar)
       body ...))

    ((_ restvar ((var def pred ...)) body ...)
     (let ((var (:optional restvar def (lambda (var) pred) ...)))
       body ...))

    ((_ restvar (newrest) body ...)
     (let ((newrest restvar))
       body ...))

    ((_ restvar ((var def pred ...) more ...) body ...)
     (let ((var (:optional restvar def (lambda (var) pred) ...))
	   (newrest (if (null? restvar) restvar (cdr restvar))))
       (let-optionals* newrest (more ...) body ...)))))

(define (make-promise thunk)
  (let ((result-ready? #f)
	(result #f))
    (lambda ()
      (if result-ready?
	  result
	  (let ((x (thunk)))
	    (if result-ready?
		result
		(begin
		  (set! result-ready? #t)
		  (set! result x)
		  result)))))))

(define (force promise)
  (promise))

(define-syntax delay
  (syntax-rules ()
    ((delay expression)
     (make-promise (lambda () expression)))))

(require (lib "r5rs-misc.scm"))
(require (lib "r5rs-ports.scm"))
(require (lib "r5rs-eval.scm"))
