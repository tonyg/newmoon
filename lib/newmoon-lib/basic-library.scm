; Basic syntax of core scheme.
;---------------------------------------------------------------------------

; There are a few essential procedures required before we can set up
; the basic macros.

((lambda ()
   (define (definer name kind value)
     (%assemble (name kind value) (name kind value)
       (scheme (%define-global-variable name value))
       (dotnet (ldarg module)
	       ($ name)
	       ($ kind)
	       (castclass "string")
	       ($ val)
	       (call "void class [Newmoon]Newmoon.Environment::InstallBinding(class [Newmoon]Newmoon.Module, object, string, object)"))))
   (definer 'sys$install-binding 'global definer)))

(sys$install-binding '%define-global-variable 'global
		     (lambda (name value)
		       (sys$install-binding name 'global value)))

(sys$install-binding '%define-macro-transformer 'global
		     (lambda (name kind transformer)
		       (sys$install-binding name 'macro (cons kind transformer))))

(define (null? x)
  (%assemble (x nil t f) (x '() #t #f)
    (scheme (null? x))
    (dotnet ($ x)
	    ($ nil)
	    (beq ldtrue)
	    ($ f)
	    (br done)
	    ldtrue
	    ($ t)
	    done)
    (jvm ($ x)
	 (getstatic "sisc.data.EmptyList" "EMPTYLIST" "sisc.data.EmptyList")
	 (obj-cond eq ldtrue)
	 (getstatic "sisc.data.SchemeBoolean" "FALSE" "sisc.data.SchemeBoolean")
	 (goto done)
	 ldtrue
	 (getstatic "sisc.data.SchemeBoolean" "TRUE" "sisc.data.SchemeBoolean")
	 done)))

(define (pair? x)
  (%assemble (x t f) (x #t #f)
    (scheme (pair? x))
    (dotnet ($ x)
	    (isinst "class [Newmoon]Newmoon.Pair")
	    (brinst ldtrue)
	    ($ f)
	    (br done)
	    ldtrue
	    ($ t)
	    done)
    (jvm ($ x)
	 (instanceof "sisc.data.Pair")
	 (int-cond !=0 ldtrue)
	 (getstatic "sisc.data.SchemeBoolean" "FALSE" "sisc.data.SchemeBoolean")
	 (goto done)
	 ldtrue
	 (getstatic "sisc.data.SchemeBoolean" "TRUE" "sisc.data.SchemeBoolean")
	 done)))

(define (cons a d)
  (%assemble (a d) (a d)
    (scheme (cons a d))
    (dotnet ($ a)
	    ($ d)
	    (newobj "instance void class [Newmoon]Newmoon.MutablePair::.ctor(object, object)"))
    (jvm (new "sisc.data.Pair")
	 (dup 1)
	 ($ a)
	 ($ d)
	 (invoke "sisc.data.Pair" "<init>" void (object object) special))))

(define (car x)
  (%assemble (x) (x)
    (scheme (car x))
    (dotnet ($ x)
	    (castclass "class [Newmoon]Newmoon.Pair")
	    (call "instance object [Newmoon]Newmoon.Pair::get_Car()"))
    (jvm ($ x)
	 (check-cast "sisc.data.Pair")
	 (getfield "sisc.data.Pair" "car" object))))
  
(define (cdr x)
  (%assemble (x) (x)
    (scheme (cdr x))
    (dotnet ($ x)
	    (castclass "class [Newmoon]Newmoon.Pair")
	    (call "instance object [Newmoon]Newmoon.Pair::get_Cdr()"))
    (jvm ($ x)
	 (check-cast "sisc.data.Pair")
	 (getfield "sisc.data.Pair" "cdr" object))))

(define (vector? x)
  (%assemble (x t f) (x #t #f)
    (scheme (vector? x))
    (dotnet ($ x)
	    (isinst "object[]")
	    (brinst ldtrue)
	    ($ f)
	    (br done)
	    ldtrue
	    ($ t)
	    done)
    (jvm ($ x)
	 (instanceof "sisc.data.SchemeVector")
	 (int-cond !=0 ldtrue)
	 (getstatic "sisc.data.SchemeBoolean" "FALSE" "sisc.data.SchemeBoolean")
	 (goto done)
	 ldtrue
	 (getstatic "sisc.data.SchemeBoolean" "TRUE" "sisc.data.SchemeBoolean")
	 done)))

(define (symbol? x)
  (%assemble (x t f) (x #t #f)
    (scheme (symbol? x))
    (dotnet ($ x)
	    (isinst "string")
	    (brinst ldtrue)
	    ($ f)
	    (br done)
	    ldtrue
	    ($ t)
	    done)
    (jvm ($ x)
	 (instanceof "sisc.data.SchemeString")
	 (int-cond !=0 ldtrue)
	 (getstatic "sisc.data.SchemeBoolean" "FALSE" "sisc.data.SchemeBoolean")
	 (goto done)
	 ldtrue
	 (getstatic "sisc.data.SchemeBoolean" "TRUE" "sisc.data.SchemeBoolean")
	 done)))

(define (eq? x y)
  (%assemble (x y t f) (x y #t #f)
    (scheme (eq? x y))
    (dotnet ($ x)
	    ($ y)
	    (beq ldtrue)
	    ($ f)
	    (br done)
	    ldtrue
	    ($ t)
	    done)
    (jvm ($ x)
	 ($ y)
	 (obj-cond eq eq-true)
	 (getstatic "sisc.data.SchemeBoolean" "FALSE" "sisc.data.SchemeBoolean")
	 (goto eq-done)
	 eq-true
	 (getstatic "sisc.data.SchemeBoolean" "TRUE" "sisc.data.SchemeBoolean")
	 eq-done)))

(define (= x y)
  (%assemble (x y) (x y)
    (scheme (= x y))
    (dotnet ($ x)
	    ($ y)
	    (call "bool class [Newmoon]Newmoon.Number::Equal(object, object)")
	    (box "bool"))
    (jvm ($ x)
	 (invoke "sisc.util.Util" "num" "sisc.data.Quantity" ("sisc.data.Value") static)
	 ($ y)
	 (invoke "sisc.util.Util" "num" "sisc.data.Quantity" ("sisc.data.Value") static)
	 (const 0)
	 (invoke "sisc.data.Quantity" "comp" boolean ("sisc.data.Quantity" int) virtual)
	 (invoke "sisc.util.Util" "truth" "sisc.data.SchemeBoolean" (boolean) static))))

(define (> x y)
  (%assemble (x y) (x y)
    (scheme (> x y))
    (dotnet ($ x)
	    ($ y)
	    (call "bool class [Newmoon]Newmoon.Number::Greater(object, object)")
	    (box "bool"))
    (jvm ($ x)
	 (invoke "sisc.util.Util" "num" "sisc.data.Quantity" ("sisc.data.Value") static)
	 ($ y)
	 (invoke "sisc.util.Util" "num" "sisc.data.Quantity" ("sisc.data.Value") static)
	 (const 1)
	 (invoke "sisc.data.Quantity" "comp" boolean ("sisc.data.Quantity" int) virtual)
	 (invoke "sisc.util.Util" "truth" "sisc.data.SchemeBoolean" (boolean) static))))

;; HERE

(define (+ x y)
  (%assemble (x y) (x y)
    (scheme (+ x y))
    (dotnet ($ x)
	    ($ y)
	    (call "object class [Newmoon]Newmoon.Number::Add(object, object)"))
    (jvm ($ x)
	 (invoke "sisc.util.Util" "num" "sisc.data.Quantity" ("sisc.data.Value") static)
	 ($ y)
	 (invoke "sisc.util.Util" "num" "sisc.data.Quantity" ("sisc.data.Value") static)
	 (invoke "sisc.data.Quantity" "add" "sisc.data.Quantity" ("sisc.data.Quantity") virtual))))

(define (- x y)
  (%assemble (x y) (x y)
    (scheme (- x y))
    (dotnet ($ x)
	    ($ y)
	    (call "object class [Newmoon]Newmoon.Number::Sub(object, object)"))
    (jvm ($ x)
	 (invoke "sisc.util.Util" "num" "sisc.data.Quantity" ("sisc.data.Value") static)
	 ($ y)
	 (invoke "sisc.util.Util" "num" "sisc.data.Quantity" ("sisc.data.Value") static)
	 (invoke "sisc.data.Quantity" "sub" "sisc.data.Quantity" ("sisc.data.Quantity") virtual))))

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
  (define (recur lis)
    (if (null? lis)
	knil
	((lambda (head)
	   (kons head (recur (cdr lis))))
	 (car lis))))
  (recur lis1))

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

;; From SRFI-1
(define (drop lis k)
  (define (iter lis k)
    (if (zero? k) lis (iter (cdr lis) (- k 1))))
  (iter lis k))

;; From SRFI-1
(define (drop-right lis k)
  (define (recur lag lead)
    (if (pair? lead)
	(cons (car lag) (recur (cdr lag) (cdr lead)))
	'()))
  (recur lis (drop lis k)))

(define (gensym . maybe-prefix)
  (if (null? maybe-prefix)
      (%assemble () ()
	(scheme (gensym))
	(dotnet (call "string class [Newmoon]Newmoon.Primitive::Gensym()")))
      (%assemble (prefix) ((car maybe-prefix))
	(scheme (gensym prefix))
	(dotnet ($ prefix)
		(callvirt "instance string class [mscorlib]System.Object::ToString()")
		(call "string class [Newmoon]Newmoon.Primitive::Gensym(string)")))))

(define apply
  (%assemble () ()
    (scheme (lambda (k proc . args)
	      (pretty-print `(scheme-apply-basic-library ,k ,proc ,args))
	      (apply proc k args)))
    (dotnet (newobj "instance void class [Newmoon]Newmoon.ApplyTailClosure::.ctor()"))))

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
	,@(map1 (lambda (clause)
		  (if (eq? (car clause) 'else)
		      clause
		      `((memv ,test-expr-id ',(car clause))
			,@(cdr clause))))
		clauses)))))

(defmacro when (test-expr . body)
  `(if ,test-expr
       (begin ,@body)))

(define (memq key lst)
  (cond
   ((null? lst) #f)
   ((eq? (car lst) key) lst)
   (else
    (memq key (cdr lst)))))

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

(define (->symbol x)
  (%assemble (x) (x)
    (dotnet ($ x)
	    (callvirt "instance string [mscorlib]System.Object::ToString()"))))

(define (string->symbol x)
  (%assemble (x) (x)
    (dotnet ($ x)
	    (castclass "class [Newmoon]Newmoon.SchemeString")
	    (call "instance string [Newmoon]Newmoon.SchemeString::ToString()"))))

(define (symbol->string x)
  (%assemble (x) (x)
    (dotnet ($ x)
	    (castclass "string")
	    (newobj "instance void class [Newmoon]Newmoon.SchemeString::.ctor(string)"))))

(define (vector->list x)
  (%assemble (x) (x)
    (dotnet ($ x)
	    (castclass "object[]")
	    (call "class [Newmoon]Newmoon.List class [Newmoon]Newmoon.List::FromVector(object[])"))))

(define (string-split s charstr)
  (map1 symbol->string
	(vector->list
	 (%assemble (sym charstr) ((string->symbol x) charstr)
	   ($ sym)
	   ($ charstr)
	   (castclass "class [Newmoon]Newmoon.SchemeString")
	   (call "instance char[] [Newmoon]Newmoon.SchemeString::GetCharArray()")
	   (call "instance string [mscorlib]System.String::Split(char[])")))))

(define (getenv name)
  (%assemble (name f) (name #f)
    (dotnet ($ name)
	    (callvirt "instance string [mscorlib]System.Object::ToString()")
	    (call "string [mscorlib]System.Environment::GetEnvironmentVariable(string)")
	    (dup)
	    (brnull ldfalse)
	    (newobj "instance void class [Newmoon]Newmoon.SchemeString::.ctor(string)")
	    (br done)
	    ldfalse
	    ($ f)
	    done)))

(define-primitive %%load-void (assembler "LdnullAssembler"))
(define (void-guard v g)
  (%assemble (v g) (v g)
    (dotnet ($ v)
	    (brnull ld-g)
	    ($ v)
	    (br done)
	    ld-g
	    ($ g)
	    done)))

(define library-search-path
  (make-parameter
   (let* ((var (getenv "NEWMOON_LIBPATH"))
	  (strs (string-split (or var "/Users/tonyg/src/newmoon/lib") ":")))
     strs)))

(define (vector-length v)
  (%assemble (v) (v)
    (scheme (vector-length v))
    (dotnet ($ v)
	    (castclass "object[]")
	    (ldlen)
	    (box "int32"))))

(define (array-create-instance type len)
  (%assemble (type len) (type len)
    (dotnet ($ type)
	    (castclass "class [mscorlib]System.Type")
	    ($ len)
	    (unbox "int32")
	    (ldind.i4)
	    (call "class [mscorlib]System.Array::CreateInstance(class [mscorlib]System.Type, int32)"))))

(define (fx= x y)
  (%assemble (x y t f) (x y #t #f)
    (scheme (= x y))
    (dotnet ($ x)
	    (unbox "int32")
	    (ldind.i4)
	    ($ y)
	    (unbox "int32")
	    (ldind.i4)
	    (bne ldfalse)
	    ($ t)
	    (br done)
	    ldfalse
	    ($ f)
	    done)))

(define (vector-ref v i)
  (%assemble (v i) (v i)
    (scheme (vector-ref v i))
    (dotnet ($ v)
	    (castclass "object[]")
	    ($ i)
	    (unbox "int32")
	    (ldind.i4)
	    (ldelem.ref))))

(define (vector-set! v i val)
  (%assemble (v i val) (v i val)
    (scheme (vector-ref v i))
    (dotnet ($ v)
	    (castclass "object[]")
	    ($ i)
	    (unbox "int32")
	    (ldind.i4)
	    ($ val)
	    (stelem.ref))))

(define (specialise-vector t v)
  (let* ((len (vector-length v))
	 (v2 (array-create-instance t len)))
    (do ((i 0 (+ i 1)))
	((fx= i len) v2)
      (vector-set! v2 i (vector-ref v i)))))

(define (list->specialised-vector t l)
  (let* ((len (length l))
	 (v2 (array-create-instance t len)))
    (do ((i 0 (+ i 1))
	 (l l (cdr l)))
	((fx= i len) v2)
      (vector-set! v2 i (car l)))))

(define (lookup-type str)
  (%assemble (sym) ((string->symbol str))
    (dotnet ($ sym)
	    (castclass "string")
	    (call "class [mscorlib]System.Type class [mscorlib]System.Type::GetType(string)"))))

(define (string-join strs sepstr)
  (%assemble (strs sepstr) (strs sepstr)
    (dotnet ($ strs)
	    (castclass "class [Newmoon]Newmoon.List")
	    ($ sepstr)
	    (castclass "class [Newmoon]Newmoon.SchemeString")
	    (call "class [Newmoon]Newmoon.SchemeString class [Newmoon]Newmoon.SchemeString::Join(class [Newmoon]Newmoon.List, class [Newmoon]Newmoon.SchemeString)"))))

(define (string-append . strings)
  (string-join strings ""))

(define (file-exists? x)
  (%assemble (x) (x)
    (dotnet ($ x)
	    (callvirt "instance string [mscorlib]System.Object::ToString()")
	    (call "bool class [mscorlib]System.IO.File::Exists(string)")
	    (box "bool"))))

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

(defmacro receive (vars expr . body)
  `(call-with-values (lambda () ,expr)
     (lambda ,vars ,@body)))

(define (check-arg pred val caller)
  (let lp ((val val))
    (if (pred val) val (lp (error "Bad argument" val pred caller)))))

(defmacro :optional (restvar def . maybe-pred)
  (if (null? maybe-pred)
      `(if (null? ,restvar)
	   ,def
	   (car ,restvar))
      `(if (null? ,restvar)
	   ,def
	   (check-arg ,pred (car ,rest-var) ':optional))))

(defmacro let-optionals (restvar vardefs . body)
  (if (null? vardefs)
      `(let () ,@body)
      (let ((newrest (gensym "letoptnewrest")))
	`(let ((,(caar vardefs) (:optional ,restvar ,(cadar vardefs)))
	       (,newrest (if (null? ,restvar) ,restvar (cdr ,restvar))))
	   (let-optionals ,newrest ,(cdr vardefs) ,@body)))))

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
