; Basic syntax of core scheme.
;---------------------------------------------------------------------------

; There are a few essential procedures required before we can set up
; the basic macros.

; Note that sys$install-binding is inserted by the compiler as a
; primitive!

(sys$install-binding '%define-global-variable 'global
		     (lambda (name val)
		       (sys$install-binding name 'global val)))

(sys$install-binding '$sc-put-cte 'global
		     (lambda args
		       '$sc-put-cte-ignored))

(define-syntax require
  (lambda (x)
    (syntax-case x (lib)
      ((_)
       #'(quote empty-require-spec-list))

      ((_ filename more ...)
       (string? (syntax-object->datum #'filename))
       (begin
	 (visit (syntax-object->datum #'filename))
	 #'(begin
	     (load filename)
	     (require more ...))))

      ((_ (lib filename collect ...) more ...)
       (andmap string? (syntax-object->datum #'(filename collect ...)))
       (begin
	 (visit (resolve-library-path (syntax-object->datum #'filename)
				      (syntax-object->datum #'(collect ...))))
	 #'(begin
	     (load (resolve-library-path filename '(collect ...)))
	     (require more ...)))))))

(define-syntax require-for-syntax
  (lambda (x)
    (syntax-case x (lib)
      ((_)
       #'(quote empty-require-for-syntax-spec-list))

      ((_ filename more ...)
       (string? (syntax-object->datum #'filename))
       (begin
	 (visit (syntax-object->datum #'filename))
	 (load (syntax-object->datum #'filename))
	 #'(begin
	     (quote (required-for-syntax filename))
	     (require-for-syntax more ...))))

      ((_ (lib filename collect ...) more ...)
       (andmap string? (syntax-object->datum #'(filename collect ...)))
       (let ((path (resolve-library-path (syntax-object->datum #'filename)
					 (syntax-object->datum #'(collect ...)))))
	 (visit path)
	 (load path)
	 #'(begin
	     (quote (required-for-syntax (lib filename collect ...)))
	     (require-for-syntax more ...)))))))

;;
;; (primitive-syntax name (kind detail ...) ...)
;;
(define-syntax primitive-syntax
  (lambda (x)
    (define (build-clause name clause)
      (with-syntax ((name name)
		    (length (datum->syntax-object (syntax here) 'length)))
	(syntax-case clause (type-predicate
			     specific-static generic-static varargs-static
			     specific-constructor generic-constructor closure-constructor
			     specific-property-reader specific-property-writer
			     generic-property-reader generic-property-writer
			     specific-method overloaded-method generic-method
			     assembler
			     static-variable)
	  ((type-predicate typename)
	   (syntax
	    ((_ actual)
	     (syntax (%extern-apply 'name
				     '(type-predicate () name (typename))
				     actual)))))

	  ((specific-static typename methodname argtype ...)
	   (with-syntax (((actual ...) (generate-temporaries (syntax (argtype ...)))))
	     (syntax
	      ((_ actual ...)
	       (syntax (%extern-apply 'name
				       '(specific-static () name (typename methodname argtype ...))
				       actual ...))))))

	  ((generic-static typename methodname)
	   (syntax
	    ((_ actual (... ...))
	     (syntax (%extern-apply 'name
				     '(generic-static () name (typename methodname))
				     actual (... ...))))))

	  ((varargs-static typename methodname)
	   (syntax
	    ((_ actual (... ...))
	     (syntax (%extern-apply 'name
				     '(varargs-static () name (typename methodname))
				     actual (... ...))))))

	  ((specific-constructor typename argtype ...)
	   (with-syntax (((actual ...) (generate-temporaries (syntax (argtype ...)))))
	     (syntax
	      ((_ actual ...)
	       (syntax (%extern-apply 'name
				       '(specific-constructor () name (typename argtype ...))
				       actual ...))))))

	  ((generic-constructor typename)
	   (syntax
	    ((_ actual (... ...))
	     (syntax (%extern-apply 'name
				     '(generic-constructor () name (typename))
				     actual (... ...))))))

	  ((closure-constructor typename argtype ...)
	   (with-syntax (((actual ...) (generate-temporaries (syntax (argtype ...)))))
	     (syntax
	      ((_ actual ...)
	       (syntax (%extern-apply 'name
				       '(closure-constructor () name (typename argtype ...))
				       actual ...))))))

	  ((specific-method typename methodname argtype ...)
	   (with-syntax (((actual ...) (generate-temporaries (syntax (receiver argtype ...)))))
	     (syntax
	      ((_ actual ...)
	       (syntax (%extern-apply 'name
				       '(specific-method () name (typename methodname argtype ...))
				       actual ...))))))

	  ((overloaded-method typename methodname)
	   (syntax
	    ((_ receiver actual (... ...))
	     (syntax (%extern-apply 'name
				     '(overloaded-method () name (typename methodname))
				     receiver actual (... ...))))))

	  ((generic-method methodname)
	   (syntax
	    ((_ receiver actual (... ...))
	     (syntax (%extern-apply 'name
				     '(generic-method () name (methodname))
				     receiver actual (... ...))))))

	  ((assembler typename)
	   (syntax
	    ((_ actual (... ...))
	     (syntax (%extern-apply 'name
				     '(assembler () name (typename))
				     actual (... ...))))))

	  ((specific-property-reader typename propname argtype ...)
	   (with-syntax (((actual ...) (generate-temporaries (syntax (receiver argtype ...)))))
	     (syntax
	      ((_ actual ...)
	       (syntax (%extern-apply 'name
				       '(specific-property (reader) name
							   (typename propname argtype ...))
				       actual ...))))))

	  ((specific-property-writer typename propname argtype ...)
	   (with-syntax (((actual ...)
			  ;; Note that newval here is in the wrong position -
			  ;; it should be last. It has been moved because generate-temporaries
			  ;; needs a proper list and doesn't support the psyntax extension
			  ;; of being able to say (x ... y).
			  (generate-temporaries (syntax (receiver newval argtype ...)))))
	     (syntax
	      ((_ actual ...)
	       (syntax (%extern-apply 'name
				       '(specific-property (writer) name
							   (typename propname argtype ...))
				       actual ...))))))

	  ((generic-property-reader typename propname)
	   (syntax
	    ((_ receiver actual (... ...))
	     (syntax (%extern-apply 'name
				     '(generic-property (reader) name (typename propname))
				     receiver actual (... ...))))))

	  ((generic-property-writer typename propname)
	   (syntax
	    ((_ receiver actual (... ...) newval)
	     (syntax (%extern-apply 'name
				     '(generic-property (writer) name (typename propname))
				     receiver actual (... ...) newval)))))

	  ((static-variable typename varname)
	   (syntax
	    ((_)
	     (syntax (%extern-apply 'name
				     '(static-variable () name (typename varname)))))))

	  )))

    (syntax-case x ()
      ((_ name (kind arg ...) ...)
       (with-syntax (((clauses ...) (map (lambda (clause)
					   (build-clause (syntax name) clause))
					 (syntax ((kind arg ...) ...)))))
	 (syntax
	  (lambda (x)
	    (syntax-case x ()
	      clauses ...))))))))

(define-syntax define-primitive
  (lambda (x)
    (syntax-case x (specific-property generic-property)
      ((_ (rname wname) (specific-property detail ...))
       (and (identifier? (syntax rname))
	    (identifier? (syntax wname)))
       (syntax
	(begin
	  (define-syntax rname (primitive-syntax rname (specific-property-reader detail ...)))
	  (define-syntax wname (primitive-syntax wname (specific-property-writer detail ...))))))

      ((_ (rname wname) (generic-property detail ...))
       (and (identifier? (syntax rname))
	    (identifier? (syntax wname)))
       (syntax
	(begin
	  (define-syntax rname (primitive-syntax rname (generic-property-reader detail ...)))
	  (define-syntax wname (primitive-syntax wname (generic-property-writer detail ...))))))

      ((_ name detail ...)
       (identifier? (syntax name))
       (syntax
	(define-syntax name (primitive-syntax name detail ...)))))))

(define-syntax define-primitive/lambda
  (lambda (x)
    (syntax-case x (varargs-static)
      ((_ name (varargs-static typename methodname))
       (syntax
	(define-syntax name
	  (lambda (x)
	    (syntax-case x ()
	      ((id x (... ...))
	       (syntax
		(%extern-apply 'name
				'(varargs-static () name (typename methodname))
				x (... ...))))
	      (id
	       (identifier? (syntax id))
	       (syntax
		(lambda arglist
		  (let ((vec (list->vector arglist)))
		    (%extern-apply 'name
				    '(specific-static () name (typename methodname vector))
				    vec))))))))))

      ((_ (name arg ...) (kind detail ...))
       (syntax
	(define-syntax name
	  (lambda (x)
	    (define helper (primitive-syntax name (kind detail ...)))
	    ;; No 'set!' since it requires nasty hooking into psyntax.
	    (syntax-case x ()
	      ((id x (... ...)) (helper (syntax (id x (... ...)))))
	      (id
	       (identifier? (syntax id))
	       (with-syntax ((application (helper (syntax (id arg ...)))))
		 (syntax (lambda (arg ...)
			   application))))))))))))

(define-syntax let-primitive
  (lambda (x)
    (syntax-case x ()
      ((_ ((name detail ...) ...) body ...)
       (syntax
	(let-syntax ((name (primitive-syntax name detail ...)) ...)
	  body ...))))))

(define-primitive/lambda (null? x) (type-predicate "Newmoon.Null"))

(define-primitive/lambda (pair? x) (type-predicate "Newmoon.Pair"))

(define-primitive/lambda (cons a d)
  (specific-constructor "Newmoon.MutablePair" object object))

(define-primitive/lambda (car x) (specific-property-reader "Newmoon.Pair" "Car"))
(define-primitive/lambda (cdr x) (specific-property-reader "Newmoon.Pair" "Cdr"))
(define-primitive/lambda (set-car! x v) (specific-property-writer "Newmoon.Pair" "Car"))
(define-primitive/lambda (set-cdr! x v) (specific-property-writer "Newmoon.Pair" "Cdr"))

(define-primitive/lambda (boolean? x) (type-predicate bool))
(define-primitive/lambda (char? x) (type-predicate char))
(define-primitive/lambda (vector? x) (type-predicate vector))
(define-primitive/lambda (symbol? x) (type-predicate symbol))
(define-primitive/lambda (eq? a b) (assembler "EqAssembler"))
(define-primitive/lambda (length x) (specific-method "Newmoon.List" "ListLength"))
(define-primitive/lambda (%%get-type x) (specific-method object "GetType"))

(define (void)
  (let-primitive ((void (specific-static "Newmoon.Undefined" "GetUndefined")))
    (void)))

(define-primitive fx= (assembler "IntEqAssembler"))
(define-primitive fx+ (assembler "IntPlusAssembler"))
(define-primitive fx- (assembler "IntMinusAssembler"))
(define-primitive fx< (assembler "IntLtAssembler"))
(define-primitive fx> (assembler "IntGtAssembler"))

(define (= a b . rest)
  (and (fx= a b)
       (let loop ((rest rest))
	 (or (null? rest)
	     (and (fx= a (car rest))
		  (loop (cdr rest)))))))

(define (< a b) (fx< a b)) ;; %%%
(define (> a b) (fx> a b)) ;; %%%
(define (fx<= a b) (if (fx> a b) #f #t)) ; %%% inline not
(define (fx>= a b) (if (fx< a b) #f #t)) ; %%% inline not
(define <= fx<=)
(define >= fx>=)

(define-primitive bitwise-and (specific-static "Newmoon.Number" "And" int int))

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
  (let recur ((lis lis1))
    (if (null? lis)
	knil
	(let ((head (car lis)))
	  (kons head (recur (cdr lis)))))))

(define (append . ls)
  (fold1-right (lambda (l acc)
		 (fold1-right cons acc l))
	       '()
	       ls))

(define (map1 f lst)
  (if (null? lst)
      '()
      (cons (f (car lst)) (map1 f (cdr lst)))))

(define (for-each1 f lst)
  (if (null? lst)
      #t
      (begin
	(f (car lst))
	(for-each1 f (cdr lst)))))

(define (memq key lst)
  (cond
   ((null? lst) #f)
   ((eq? (car lst) key) lst)
   (else
    (memq key (cdr lst)))))

;---------------------------------------------------------------------------

(define-primitive/lambda + (varargs-static "Newmoon.Number" "Add"))
(define-primitive/lambda - (varargs-static "Newmoon.Number" "Sub"))
(define-primitive/lambda * (varargs-static "Newmoon.Number" "Mul"))
(define-primitive/lambda / (varargs-static "Newmoon.Number" "Div"))
(define-primitive/lambda (quotient n1 n2) (generic-static "Newmoon.Number" "Quotient"))
(define-primitive/lambda (remainder n1 n2) (generic-static "Newmoon.Number" "Remainder"))

(define-primitive/lambda (list->vector x) (specific-method "Newmoon.List" "ToVector"))
(define-primitive/lambda (zero? x) (specific-static "Newmoon.Number" "IsZero" object))
(define-primitive/lambda (->symbol x) (specific-method object "ToString"))
(define-primitive/lambda (string->symbol x) (specific-method string "ToString"))
(define-primitive/lambda (symbol->string x) (specific-constructor string symbol))

;; SRFI-23 error.
(let-primitive ((scheme-error (specific-static "Newmoon.Primitive" "SchemeError" vector)))
  (define (error msg . args)
    (scheme-error (list->vector (cons msg args)))))

(define-syntax %%string-comparison
  (syntax-rules ()
    ((_ pred ci)
     (lambda (a b)
       (let-primitive ((compare (specific-static symbol "Compare" symbol symbol bool)))
	 (pred (compare (string->symbol a) (string->symbol b) ci)))))))

(define string=? (%%string-comparison zero? #f))
(define string-ci=? (%%string-comparison zero? #t))

(define string<? (%%string-comparison negative? #f))
(define string-ci<? (%%string-comparison negative? #t))

(define string>? (%%string-comparison positive? #f))
(define string-ci>? (%%string-comparison positive? #t))

(define (string<=? a b) (not (string>? a b)))
(define (string-ci<=? a b) (not (string-ci>? a b)))

(define (string>=? a b) (not (string<? a b)))
(define (string-ci>=? a b) (not (string-ci<? a b)))

(define (string-downcase a)
  (let-primitive ((downcase (specific-method symbol "ToLower")))
    (symbol->string (downcase (string->symbol a)))))

(define (symbol-downcase a)
  (let-primitive ((intern (specific-static symbol "Intern" symbol))
		  (downcase (specific-method symbol "ToLower")))
    (intern (downcase a))))

(define-primitive %%make-call/cc (closure-constructor "Newmoon.CallWithCurrentContinuation"))
(define-primitive %%make-call/values (closure-constructor "Newmoon.CallWithValues"))
(define-primitive %%make-values (closure-constructor "Newmoon.Values"))
(define call-with-current-continuation (%%make-call/cc))
(define call-with-values (%%make-call/values))
(define values (%%make-values))

(define (gensym . p)
  (let-primitive ((g (specific-static "Newmoon.Primitive" "Gensym" symbol)))
    (g (if (null? p)
	   'g
	   (->symbol (car p))))))

(define-primitive/lambda vector (varargs-static "Newmoon.Primitive" "Vector"))
(define-primitive/lambda (vector-length v) (assembler "VectorLengthAssembler"))
(define-primitive/lambda (vector-ref v k) (assembler "VectorRefAssembler"))
(define-primitive/lambda (vector-set! v k o) (assembler "VectorSetAssembler"))
(define-primitive/lambda (vector->list v) (specific-static "Newmoon.List" "FromVector" vector))
(define (vector-fill! v o)
  (do ((i 0 (+ i 1)))
      ((fx= i (vector-length v)))
    (vector-set! v i o)))

(let-primitive ((%%make-vector (assembler "Newmoon.MakeVectorAssembler")))
  (define (make-vector count . o)
    (if (pair? o)
	(%%make-vector count (car o))
	(%%make-vector count (void)))))

(define-primitive/lambda (string? x) (type-predicate string))

(let-primitive ((make-string2 (specific-constructor string int char))
		(make-string1 (specific-constructor string int)))
  (define (make-string count . ch)
    (if (pair? ch)
	(make-string2 count (car ch))
	(make-string1 count))))

(define-primitive/lambda (list->string x) (specific-static string "FromList" list))
(define (string . args) (list->string args))

(define-primitive/lambda (string->list x) (specific-method string "ToList"))
(define-primitive/lambda (string-copy x) (specific-method string "Clone"))
(define-primitive/lambda (string-fill! x c) (specific-method string "Fill" char))

(define-primitive/lambda (string-length x) (specific-property-reader string "Length"))
(define-primitive/lambda (string-ref x k) (specific-property-reader string "Item" int))
(define-primitive/lambda (string-set! x k c) (specific-property-writer string "Item" int))
(define-primitive/lambda (substring x s e) (specific-method string "SubString" int int))

(define-primitive/lambda (char-numeric? x) (specific-static char "IsNumber" char))
(define-primitive/lambda (char-whitespace? x) (specific-static char "IsWhiteSpace" char))
(define-primitive/lambda (char-alphabetic? x) (specific-static char "IsLetter" char))
(define-primitive/lambda (char-upper-case? x) (specific-static char "IsUpper" char))
(define-primitive/lambda (char-lower-case? x) (specific-static char "IsLower" char))
(define-primitive/lambda (char-downcase x) (specific-static char "ToLower" char))
(define-primitive/lambda (char-upcase x) (specific-static char "ToUpper" char))
(define-primitive/lambda (char->integer x) (specific-static "System.Convert" "ToInt32" char))
(define-primitive/lambda (integer->char x) (specific-static "System.Convert" "ToChar" int))

(define-primitive/lambda (%char->latin1 x) (specific-static "System.Convert" "ToInt32" char))
(define-primitive/lambda (%latin1->char x) (specific-static "System.Convert" "ToChar" int))

(define char-cased?
  (let ((big-a (char->integer #\A))
	(big-z (char->integer #\Z))
	(little-a (char->integer #\a))
	(little-z (char->integer #\z)))
    (lambda (c)
      (let ((i (char->integer c)))
	(or (and (>= i big-a) (<= i big-z))
	    (and (>= i little-a) (<= i little-z)))))))

(define char-titlecase char-upcase)

(define (char=? a b)
  (fx= (char->integer a)
       (char->integer b)))

(define (char-ci=? a b)
  (char=? (char-downcase a)
	  (char-downcase b)))

(define (char<? a b) (fx< (char->integer a) (char->integer b)))
(define (char>? a b) (fx> (char->integer a) (char->integer b)))
(define (char<=? a b) (fx<= (char->integer a) (char->integer b)))
(define (char>=? a b) (fx>= (char->integer a) (char->integer b)))

(define (char-ci<? a b) (char<? (char-downcase a) (char-downcase b)))
(define (char-ci>? a b) (char>? (char-downcase a) (char-downcase b)))
(define (char-ci<=? a b) (char<=? (char-downcase a) (char-downcase b)))
(define (char-ci>=? a b) (char>=? (char-downcase a) (char-downcase b)))

(define-primitive make-hash-table (specific-constructor "System.Collections.Hashtable"))
(define-primitive/lambda (hash-table? x) (type-predicate "System.Collections.Hashtable"))
(define-primitive (%%hash-table-get hash-table-put!)
  (specific-property "System.Collections.Hashtable" "Item" object))
(define-primitive %%hash-table-contains-key?
  (specific-method "System.Collections.Hashtable" "Contains" object))
(define-primitive hash-table-remove!
  (specific-method "System.Collections.Hashtable" "Remove" object))
(define (hash-table-get ht key fail-thunk)
  (if (%%hash-table-contains-key? ht key)
      (%%hash-table-get ht key)
      (fail-thunk)))

(define-primitive %%exit (specific-static "System.Environment" "Exit" int))
(define (exit . n)
  (if (pair? n)
      (%%exit (car n))
      (%%exit 0)))

(define-primitive %%InvokeModule (specific-method "Newmoon.Environment" "InvokeModule" symbol))
(define-primitive %%ClosureModule (specific-property-reader "Newmoon.Closure" "Module"))
(define-primitive %%ModuleEnvironment (specific-property-reader "Newmoon.Module" "Env"))

(define-syntax current-environment
  (syntax-rules ()
    ((_)
     (%%ModuleEnvironment
      (%%ClosureModule
       (lambda () #f))))))

(define (load f)
  (let* ((e (current-environment))
	 (c (%%InvokeModule e (string->symbol f))))
    (c)))

(define-primitive %%make-apply (closure-constructor "Newmoon.ApplyClosure"))
(define apply (%%make-apply))

(define-primitive/lambda (eqv? x y)
  (specific-static "Newmoon.Primitive" "SchemeEqv" object object))
(define-primitive/lambda (equal? x y)
  (specific-static "Newmoon.Primitive" "SchemeEqual" object object))

(define-primitive/lambda (negative? x) (specific-static "Newmoon.Primitive" "NegativeP" object))
(define-primitive/lambda (positive? x) (specific-static "Newmoon.Primitive" "PositiveP" object))
(define-primitive/lambda (number? x) (type-predicate int))
(define-primitive/lambda (complex? x) (type-predicate int))
(define-primitive/lambda (real? x) (type-predicate int))
(define-primitive/lambda (rational? x) (type-predicate int))
(define-primitive/lambda (integer? x) (type-predicate int))

(define-primitive/lambda (exact? x) (type-predicate int))
(define-primitive/lambda (inexact? x) (type-predicate int))

(define (min arg . args)
  (let lp ((result arg) (args args))
    (cond
     ((null? args) result)
     ((< (car args) result) (lp (car args) (cdr args)))
     (else (lp result (cdr args))))))

(define (max arg . args)
  (let lp ((result arg) (args args))
    (cond
     ((null? args) result)
     ((> (car args) result) (lp (car args) (cdr args)))
     (else (lp result (cdr args))))))

(define-primitive namespace-defined? (assembler "Newmoon.NamespaceDefinedAssembler"))

(define-primitive/lambda (procedure? x) (type-predicate closure))

(define (last lis) (car (last-pair lis)))

(define (last-pair lis)
  (let lp ((lis lis))
    (let ((tail (cdr lis)))
      (if (pair? tail) (lp tail) lis))))

(define (drop-right lis k)
  (let recur ((lag lis) (lead (drop lis k)))
    (if (pair? lead)
	(cons (car lag) (recur (cdr lag) (cdr lead)))
	'())))

(define (list-tail lis k)
  (let iter ((lis lis) (k k))
    (if (zero? k) lis (iter (cdr lis) (- k 1)))))

(define (list-ref lis k)
  (car (list-tail lis k)))

(define (list? x)
  (let lp ((x x) (lag x))
    (if (pair? x)
	(let ((x (cdr x)))
	  (if (pair? x)
	      (let ((x   (cdr x))
		    (lag (cdr lag)))
		(and (not (eq? x lag)) (lp x lag)))
	      (null? x)))
	(null? x))))

; (map list '(1 2 3) '(a b c d) '(p q r s t))
;
(define (map f lst0 . lsts)
  (if (null? lsts)
      (map1 f lst0)
      (let inner-map ((f f) (lsts (cons lst0 lsts)))
	(call-with-current-continuation
	 (lambda (return)
	   (for-each1 (lambda (x) (if (null? x) (return '()))) lsts)
	   (let ((args (map1 car lsts)))
	     (cons (apply f args) (inner-map f (map1 cdr lsts)))))))))

; (for-each (lambda (x y) (display x) (newline) (display y) (newline) (newline))
; 	  '(1 2 3)
; 	  '(a b c d))
;
(define (for-each f lst0 . lsts)
  (if (null? lsts)
      (for-each1 f lst0)
      (let inner-for-each ((f f) (lsts (cons lst0 lsts)))
	(call-with-current-continuation
	 (lambda (return)
	   (for-each1 (lambda (x) (if (null? x) (return 'for-each-unspecified))) lsts)
	   (let ((args (map1 car lsts)))
	     (apply f args)
	     (inner-for-each f (map1 cdr lsts))))))))

(define (assq key alst)
  (cond
   ((null? alst) #f)
   ((eq? (caar alst) key) (car alst))
   (else
    (assq key (cdr alst)))))

(define (assv key alst)
  (cond
   ((null? alst) #f)
   ((eqv? (caar alst) key) (car alst))
   (else
    (assv key (cdr alst)))))

(define (assoc key alst)
  (cond
   ((null? alst) #f)
   ((equal? (caar alst) key) (car alst))
   (else
    (assoc key (cdr alst)))))

(define (memv key lst)
  (cond
   ((null? lst) #f)
   ((eqv? (car lst) key) lst)
   (else
    (memv key (cdr lst)))))

(define (member key lst)
  (cond
   ((null? lst) #f)
   ((equal? (car lst) key) lst)
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
