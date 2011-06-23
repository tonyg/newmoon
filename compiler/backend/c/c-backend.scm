;; CPS C backend, Cheney-on-the-MTA style.

(define %cid string?)

(define (%ctype v)
  (or (string? v)
      (and (pair? v)
	   (pair? (cdr v))
	   (eq? (car v) '*)
	   (%ctype (cadr v)))))

(define-node-type @structdef
  (name %cid)
  (fields (%list-of @vardef)))

(define-node-type @functiondef
  (name %cid)
  (modifiers (%list-of string?))
  (rettype %ctype)
  (variadic? boolean?)
  (formals (%list-of @vardef))
  (locals (%box-of (%list-of @vardef)))
  (body (%box-of (%list-of %cinstr))))

(define-node-type @vardef
  (name %cid)
  (type %ctype))

(define (%cinstr instr)
  (or (symbol? instr)
      (string? instr)
      (number? instr)
      (and (pair? instr)
	   (symbol? (car instr))
	   (every %cinstr (cdr instr)))))

(define-node-type @constant-closure
  (lambdaname %cid))

(define *noreturn* "__attribute__((noreturn))")

(define (make-vardef* def)
  (if (and (node? def) (node-kind? def @vardef))
      def
      (make-vardef (car def) (cadr def))))

(define (make-vardef name type)
  (make-node @vardef name type))

(define (make-structdef name fields)
  (make-node @structdef name (map make-vardef* fields)))

(define (make-functiondef name modifiers rettype is-variadic formals)
  (make-node @functiondef name modifiers rettype is-variadic (map make-vardef* formals)
	     (box '())
	     (box '())))

(define (push-box! b val)
  (set-box! b (cons val (unbox b))))

(define (add-local! functiondef localname localtype localinit)
  (push-box! (@functiondef-locals functiondef) (list localname localtype localinit)))

(define (add-instr! functiondef instr)
  (push-box! (@functiondef-body functiondef) instr))

(define (add-instrs! functiondef instrs)
  (for-each (lambda (i) (add-instr! functiondef i)) instrs))

(define (make-counter receiver)
  (let ((counter 0))
    (lambda ()
      (let ((c counter))
	(set! counter (+ counter 1))
	(receiver c)))))

(define (mangle-id sym-or-str)
  (let ((str (if (symbol? sym-or-str) (symbol->string sym-or-str) sym-or-str)))
    (list->string
     (cons* #\N #\_
	    (fold-right (lambda (char acc)
			  (if (or (char-alphabetic? char)
				  (char-numeric? char))
			      (cons char acc)
			      (case char
				((#\-) (cons* #\_ #\_ acc))
				(else (cons* #\_ #\x (append (string->list
							      (number->string
							       (char->integer char) 16))
							     (cons #\_ acc)))))))
			'()
			(string->list str))))))

(define (string-or-symbol->id strsym)
  (cond
   ((string? strsym) strsym)
   ((symbol? strsym) (symbol->string strsym))
   (else (error `(non-string-or-symbol string-or-symbol->id ,strsym)))))

(define (emit port . items)
  (emit* port items))

(define (emit* port items)
  (for-each (lambda (item) (display item port)) items))

(define (c-file-prologue port assemblyname)
  (emit port
	"/* Generated by Newmoon */\n"
	"#include \"newmoon-code.h\"\n"
	"\n"))

(define (emit-separated-list port items emitter oparen sep cparen)
  (emit port oparen)
  (let loop ((items items)
	     (need-sep #f))
    (if (null? items)
	(emit port cparen)
	(begin
	  (when need-sep (emit port sep))
	  (emitter (car items))
	  (loop (cdr items) #t)))))

(define (emit-terminated-list port items emitter oparen term cparen)
  (emit port oparen)
  (let loop ((items items))
    (if (null? items)
	(emit port cparen)
	(begin
	  (emitter (car items))
	  (emit port term)
	  (loop (cdr items))))))

(define (emit-type port t)
  (if (pair? t)
      (case (car t)
	((*) (emit port "")
	     (emit-type port (cadr t))
	     (emit port " *"))
	(else (error `(invalid-c-type ,t))))
      (emit port t)))

(define (emit-vardef port indent)
  (let ((pad (make-string indent #\space)))
    (lambda (vardef)
      (emit port pad)
      (emit-type port (@vardef-type vardef))
      (emit port " "(@vardef-name vardef)))))

(define (emit-structdef port)
  (lambda (structdef)
    (emit port "typedef struct S_"(@structdef-name structdef))
    (emit-terminated-list port
			  (@structdef-fields structdef)
			  (emit-vardef port 2) " {\n" ";\n" "} ")
    (emit port (@structdef-name structdef)";\n\n")))

(define (emit-function-header port functiondef)
  (for-each (lambda (modifier) (emit port modifier" "))
	    (@functiondef-modifiers functiondef))
  (emit-type port (@functiondef-rettype functiondef))
  (emit port " "(@functiondef-name functiondef))
  (emit-separated-list port
		       (@functiondef-formals functiondef)
		       (emit-vardef port 0)
		       "(" ", " "")
  (if (@functiondef-variadic? functiondef)
      (emit port ", ...)")
      (emit port ")")))

(define (emit-functionproto port)
  (lambda (functiondef)
    (emit-function-header port functiondef)
    (emit port ";\n")))

(define (emit-functiondef port)
  (lambda (functiondef)
    (emit-function-header port functiondef)
    (emit port " {\n")
    (for-each (lambda (localdef)
		(emit port "  ")
		(emit-type port (cadr localdef))
		(emit port " "(car localdef)" = "(caddr localdef)";\n"))
	      (unbox (@functiondef-locals functiondef)))
    (let ((extra-scope-count (fold (emit-instr port) 0
				   (reverse (unbox (@functiondef-body functiondef))))))
      (emit port (make-string extra-scope-count #\})"\n"))
    (emit port "}\n\n")))

(define (emit-instr port)
  (letrec ((emitter (lambda (instr)
		      (if (pair? instr)
			  (begin
			    (emit port (car instr))
			    (emit-separated-list port (cdr instr) emitter "(" ", " ")"))
			  (emit port instr)))))
    (lambda (outer-instr old-scope-count)
      (emit port "  ")
      (cond
       ((and (pair? outer-instr) (eq? (car outer-instr) 'push-temporary-scope))
	(emit port "{\n")
	(+ old-scope-count 1))
       ((pair? outer-instr)
	(case (car outer-instr)
	  ((begin-literal-c)
	   (let ((tempname (cadr outer-instr)))
	     (emit port
		   "\n#define return(x) ({ "tempname" = (x); goto return__"tempname"; })\n")))
	  ((literal-c)
	   (let ((pieces (cdr outer-instr)))
	     (for-each (lambda (piece) (emit port piece"\n  ")) pieces)))
	  ((end-literal-c)
	   (let ((tempname (cadr outer-instr)))
	     (emit port "\n#undef return\n  return__"tempname":\n")))
	  (else
	   (emitter outer-instr)))
	(emit port ";\n")
	old-scope-count)
       (else
	(emitter outer-instr)
	(emit port ";\n")
	old-scope-count)))))

(define (arginfo->id arginfo)
  (mangle-id (@arginfo-name arginfo)))

(define (arginfo->ctype arginfo)
  (if (@arginfo-cont arginfo)
      "continuation"
      "oop"))

(define (capture-index capture)
  (@loc-local-index (@capture-new-location capture)))

(define (capture-fields captures)
  (map (lambda (capture)
	 (let ((arginfo (@capture-arginfo capture))
	       (index (capture-index capture)))
	   (cons index (make-vardef (string-append "env_"(number->string index)"_"
						   (arginfo->id arginfo))
				    (capture-type capture)))))
       captures))

(define (capture-index->name index capture-map)
  (cdr (assq index capture-map)))

(define (capture-type capture)
  (arginfo->ctype (@capture-arginfo capture)))

(define (capture-source-arg-for-index index)
  (string-append "cap_"(number->string index)))

(define (capture-source-args captures)
  (map (lambda (capture)
	 (list (capture-source-arg-for-index (capture-index capture))
	       (capture-type capture)))
       captures))

(define (zero-pad-left str width)
  (let ((delta (- width (string-length str))))
  (cond
   ((negative? delta) (error `(numeric-string-too-wide zero-pad-left ,str ,width)))
   ((zero? delta) str)
   (else (string-append (make-string delta #\0) str)))))

(define (escape-string str)
  (list->string
   (cons #\"
	 (fold-right (lambda (ch acc)
		       (let ((i (char->integer ch)))
			 (if (or (< i 32) (= i 92) (>= i 128)) ;; assume ASCII %%%
			     (append '(#\\)
				     (string->list (zero-pad-left (number->string i 8) 3))
				     acc)
			     (if (char=? ch #\")
				 (cons* #\\ #\" acc)
				 (cons ch acc)))))
		     '(#\")
		     (string->list str)))))

(define (compiler-back-end-phases input-filename frontend-result)
  (let* ((next-lambda-name (make-counter (lambda (c) (string-append "Fn" (number->string c)))))
	 (next-envdef-name (make-counter (lambda (c) (string-append "Env" (number->string c)))))
	 (next-label (make-counter (lambda (c) (string-append "L" (number->string c)))))
	 (next-temp (make-counter (lambda (c) (string-append "T" (number->string c)))))
	 (next-literal (make-counter (lambda (c) (string-append "Lit" (number->string c)))))
	 (assembly-name (if (compiler$target-namespace)
			    (compiler$target-namespace)
			    (replace-filename-extension input-filename "")))
	 (output-filename (replace-filename-extension input-filename ".c"))
	 (literal-table '())
	 (global-table (make-hash-table))
	 (all-structures '())
	 (all-functions '())
	 (all-literal-c-stanzas '()))

    (define (record-structure! structdef)
      (set! all-structures (cons structdef all-structures))
      structdef)

    (define (record-function! functiondef)
      (set! all-functions (cons functiondef all-functions))
      functiondef)

    (define (record-global! globalname)
      (hash-table-put! global-table globalname globalname)
      globalname)

    (define (record-literal! key literalvalue-thunk)
      (cond
       ((and key (assoc key literal-table)) => cadr)
       (else
	(let ((name (next-literal)))
	  (set! literal-table (cons (list key name (literalvalue-thunk)) literal-table))
	  name))))

    (define (record-literal-c-stanza! lines)
      (set! all-literal-c-stanzas (cons lines all-literal-c-stanzas)))

    (define (new-function! name modifiers rettype is-variadic formals)
      (let ((fd (make-functiondef name modifiers rettype is-variadic formals)))
	(record-function! fd)
	(add-instr! fd `(newmoontrace ,(escape-string assembly-name)
				      ,(escape-string name)))
	fd))

    (define (temp fn instr)
      (let ((name (next-temp)))
	(add-instr! fn `(deftemp ,name ,instr))
	name))

    (define (alloctemp fn type instr)
      (let ((name (next-temp)))
	(add-instr! fn `(defstorage ,name ,type ,instr))
	(temp fn `(addressof ,name))))

    (define (gen-literal-initialiser fn value)
      (cond
       ((symbol? value) `(intern ,(escape-string (symbol->string value))
				 ,(string-length (symbol->string value))))
       ((string? value)
	(let ((name (next-temp)))
	  (add-instr! fn `(defbinary ,name ,(string-length value) ,(escape-string value)))
	  name))
       ((pair? value) (alloctemp fn 'pair `(mkpair ,(car value) ,(cdr value))))
       ((vector? value)
	(let ((len (vector-length value)))
	  (let ((vec (alloctemp fn 'vector `(mkvec ,len))))
	    (do ((i 0 (+ i 1)))
		((= i len))
	      (add-instr! fn `(vecset ,vec ,i ,(vector-ref value i))))
	    vec)))
       ((node? value)
	(node-match value
	  ((@constant-closure lambdaname)
	   (let ((name (next-temp)))
	     (add-instr! fn `(allocenv constant_closure_env 0 ,lambdaname ,name))
	     name))))
       (else
	(error `(unimplemented-literal-kind gen-literal-initialiser ,value)))))

    (define (build-closure parent-fn capture-instrs node receive-closure)
      (let* ((is-continuation (@cps2-lambda-is-continuation node))
	     (formals (@cps2-lambda-formals node))
	     (captures (@cps2-lambda-captures node))
	     (num-captures (length captures))
	     (varargs (@cps2-lambda-varargs node))
	     (expr (@cps2-lambda-expr node))
	     (arity (let ((fl (length formals))) (if varargs (- fl 1) fl)))
	     (lambdaname (next-lambda-name))
	     (varlambdaname (next-lambda-name))
	     (envdefname (next-envdef-name))
	     (newcloname (next-temp))
	     (capture-map (capture-fields captures)))

	(define (formals->argdefs formals)
	  (filter-map (lambda (loc)
			(let ((ai (@loc-local-arginfo loc)))
			  (and (not (@arginfo-is-rest ai))
			       `(,(arginfo->id ai) ,(arginfo->ctype ai)))))
		      formals))

	(record-structure! (make-structdef envdefname `(("header" "object_header")
							("code" "newmoon_code")
							,@(map cdr capture-map))))

	;; Construct closure instance
	(if (null? captures)
	    (set! newcloname
		  (record-literal! #f (lambda () (make-node @constant-closure lambdaname))))
	    (add-instr! parent-fn `(allocenv ,envdefname
					     ,(length capture-map)
					     ,lambdaname
					     ,newcloname)))
	(for-each (lambda (capture capture-instr)
		    (let* ((cap-cont (@arginfo-cont (@capture-arginfo capture)))
			   (index (capture-index capture))
			   (v (capture-index->name index capture-map)))
			(add-instr! parent-fn `(storeenv ,newcloname
							 ,(@vardef-name v)
							 ,capture-instr))))
		  captures capture-instrs)

	(let ((code (new-function! lambdaname
				   `("static" ,*noreturn*)
				   "void"
				   varargs
				   `(("self" (* ,envdefname))
				     ("argc" "int")
				     ,@(formals->argdefs formals))))
	      (first-vardef-name (arginfo->id (@loc-local-arginfo (car formals))))
	      (varlambdacode (new-function! varlambdaname
					    `("static" ,*noreturn*)
					    "void"
					    #f
					    `(("self" (* ,envdefname))
					      ("actuals" "oop")))))
	  (add-instrs! varlambdacode
		       (map (lambda (loc) `(deftemp ,(arginfo->id (@loc-local-arginfo loc)) NULL))
			    formals))
	  (add-instr! varlambdacode `(deftemp original_actuals actuals))
	  (add-instrs! varlambdacode
		       (map (lambda (loc)
			      (let ((ai (@loc-local-arginfo loc)))
				`(,(if (@arginfo-is-rest ai)
				       'extractrestactual
				       'extractsingleactual)
				  ,(if varargs
				       'wrong_variable_argc_apply
				       'wrong_fixed_argc_apply)
				  ,arity
				  ,(arginfo->id ai))))
			    formals))
	  (add-instr! varlambdacode `(,lambdaname self -2
						  ,@(map (lambda (loc)
							   (arginfo->id (@loc-local-arginfo loc)))
							 formals)))
	  (if varargs
	      (let ((rest-arginfo (car (filter @arginfo-is-rest
					       (map @loc-local-arginfo formals))))
		    (penultimate-vardef (last (@functiondef-formals code))))
		(add-instrs! code `((deftemp ,(arginfo->id rest-arginfo) (mknull))
				    (extractvarargs ,(arginfo->id rest-arginfo)
						    ,arity
						    ,first-vardef-name
						    ,varlambdaname
						    ,(@vardef-name penultimate-vardef)))))
	      (add-instr! code `(checkfixedarity ,arity ,first-vardef-name ,varlambdaname)))
	  (for-each (lambda (loc)
		      (when (@loc-local-boxed? loc)
			(add-instr! code `(installbox
					   ,(arginfo->id (@loc-local-arginfo loc))))))
		    formals)
	  (add-instr! code (gen-node code capture-map expr)))

	(receive-closure newcloname
			 lambdaname)))

    (define (gen-node fn capture-map node)
      (define (gen-literal value)
	(cond
	 ((pair? value)
	  (record-literal! value (lambda ()
				   (cons (gen-literal (car value))
					 (gen-literal (cdr value))))))
	 ((vector? value)
	  (record-literal! value (lambda ()
				   (list->vector
				    (map gen-literal (vector->list value))))))
	 ((number? value) (if (and (exact? value)
				   (integer? value))
			      `(litint ,value)
			      (alloctemp fn 'floatholder `(mkfloatholder ,value))))
	 ((boolean? value) (if value `(mktrue) `(mkfalse)))
	 ((char? value) `(mkchar ,(char->integer value)))
	 ((null? value) `(mknull))
	 (else
	  (record-literal! value (lambda () value)))))

      (define (gen-local-load loc)
	(case (@loc-local-source loc)
	  ((argument) (arginfo->id (@loc-local-arginfo loc)))
	  ((environment) (let ((v (capture-index->name (@loc-local-index loc) capture-map)))
			   `(envref self ,(@vardef-name v))))))

      (define (gen-local-store loc instr)
	(case (@loc-local-source loc)
	  ((argument) `(settemp ,(arginfo->id (@loc-local-arginfo loc)) ,instr))
	  ((environment) `(setenv env
				  ,(capture-index->name (@loc-local-index loc) capture-map)
				  ,instr))))

      (define (gen-get name loc)
	(node-match loc
	  ((@loc-local)
	   (if (@loc-local-boxed? loc)
	       `(getbox ,(gen-local-load loc))
	       (gen-local-load loc)))
	  ((@loc-global)
	   `(globalget ,(mangle-id (record-global! name))))))

      (define (gen-closure-instantiation node receive-closure)
	(let* ((captures (@cps2-lambda-captures node))
	       (capture-instrs (map (lambda (capture)
				      (gen-local-load (@capture-old-location capture)))
				    captures)))
	  (build-closure fn capture-instrs node receive-closure)))

      (define (gen-asm formals actuals code)
	(let* ((c-code (cond
			((find (lambda (clause) (eq? 'c (@backend-asm-name clause))) code)
			 => @backend-asm-code)
			(else (error `(cps2-asm missing-c-clause ,(node->list node))))))
	       (tempname (next-temp)))
	  (add-local! fn tempname 'oop "NULL")
	  (add-instr! fn '(push-temporary-scope))
	  (add-instr! fn `(begin-literal-c ,tempname))
	  (let* ((env0 (map cons formals actuals))
		 (env1 (filter (lambda (entry) (memq (car entry) c-code)) env0))
		 (env (map (lambda (entry) (cons (car entry) (temp fn (gen (cdr entry)))))
			   env1)))
	    (add-instr! fn `(literal-c ,@(map (lambda (instr)
						(cond
						 ((symbol? instr)
						  (cdr (or (assq instr env)
							   (error `(cps2-asm unknown-formal
									     ,instr ,node)))))
						 ((string? instr)
						  instr)
						 (else (error `(cps2-asm invalid-c-syntax
									 ,instr ,node)))))
					      c-code))))
	  (add-instr! fn `(end-literal-c ,tempname))
	  tempname))

      (define (gen-backend backend-name arguments)
	(when (eq? backend-name 'c)
	  (record-literal-c-stanza! arguments))
	`(mkvoid))

      (define (gen-set name loc expr)
	(node-match loc
	  ((@loc-local)
	   (if (@loc-local-boxed? loc)
	       `(setbox ,(gen-local-load loc) ,(gen expr))
	       (gen-local-store loc (gen expr))))
	  ((@loc-global)
	   `(globalset ,(mangle-id (record-global! name)) ,(gen expr)))))

      (define (gen-apply cont rator rands)
	(let ((arity (length rands)))
	  (if (node-kind? rator @cps2-lambda)
	      (gen-closure-instantiation rator
					 (lambda (varname lambdaname)
					   `(directcall ,lambdaname
							,varname
							,arity
							,@(map gen rands))))
	      `(,(if cont 'callfun 'checkedcallfun)
		,(gen rator)
		,arity
		,@(map gen rands)))))

      (define (gen-begin head tail)
	(add-instr! fn (gen head))
	(gen tail))

      (define (gen-if test true false)
	(let ((tlabel (next-temp))
	      (flabel (next-temp)))
	  (add-instrs! fn `((conditional ,(gen test) ,tlabel ,flabel)
			    (deflabel ,tlabel)))
	  (add-instrs! fn `(,(gen true)
			    (deflabel ,flabel)))
	  (add-instrs! fn `(,(gen false)))
	  `(emptystmt)))

      (define (gen node)
	(node-match node
	  ((@cps2-lit value) (gen-literal value))
	  ((@cps2-void) `(mkvoid))
	  ((@cps2-get name location) (gen-get name location))
	  ((@cps2-lambda) (gen-closure-instantiation node (lambda (varname lambdaname) varname)))
	  ((@cps2-asm formals actuals code) (gen-asm formals actuals code))
	  ((@cps2-backend backend-name arguments) (gen-backend backend-name arguments))
	  ((@cps2-set name location expr) (gen-set name location expr))
	  ((@cps2-value-begin head tail) (gen-begin head tail))
	  ((@cps2-value-if test true false) (gen-if test true false))
	  ((@cps2-apply cont rator rands) (gen-apply cont rator rands))
	  ((@cps2-exp-begin head tail) (gen-begin head tail))
	  ((@cps2-exp-if test true false) (gen-if test true false))))

      (gen node))

    (define (gen-module-entry-point node)
      ;; We're relying here on being handed a cps-lambda node, that is
      ;; a real lambda-proc, not a lambda-cont (or lambda-jump, once
      ;; we implement those).
      (compiler-assert module-entry-point-is-lambda-proc
		       (and (node-kind? node @cps2-lambda)
			    (not (@cps2-lambda-is-continuation node))))
      ;; It also has to have no captures.
      (compiler-assert module-entry-point-has-no-captures
		       (null? (@cps2-lambda-captures node)))
      (build-closure 'invalid-parent-fn '() node (lambda (varname lambdaname) varname)))

    (define (gen-program-entry-point)
      (let ((entry (new-function! "main" '() '"int" #f `(("argc" "int") ("argv" (* "char"))))))
	(add-instrs! entry `((return (newmoon_main argc argv |InitGlobals| |Startup|))))))

    ;;---------------------------------------------------------------------------
    (for-each display (list ";; GCC backend compiling to "assembly-name"\n"))
    (let ((first-function (gen-module-entry-point frontend-result)))

      (when (compiler$make-program)
	(gen-program-entry-point))

      (for-each display (list ";; GCC backend generating "output-filename"\n"))
      (delete-file-if-exists output-filename)
      (call-with-output-file output-filename
	(lambda (o)
	  (c-file-prologue o (mangle-id assembly-name))
	  (for-each (lambda (lines)
		      (for-each (lambda (line) (emit o line"\n")) lines))
		    (reverse all-literal-c-stanzas))
	  (emit o "\n")
	  (hash-table-for-each global-table
			       (lambda (globalname dummy)
				 (emit o "defglobal("(mangle-id globalname)");\n")))
	  (emit o "\n")
	  (let* ((global-initialiser (new-function! "InitGlobals" '() "void" #f `()))
		 (mangled-global-ids
		  (hash-table-map global-table
				  (lambda (globalname dummy)
				    (let ((mid (mangle-id globalname)))
				      (add-instr! global-initialiser
						  `(initglobal ,mid
							       ,(escape-string
								 (symbol->string globalname))))
				      mid)))))
	    (add-instr! global-initialiser
			`(registerroots ,(length mangled-global-ids)
					,@(map (lambda (mid)
						 `(globalbox ,mid))
					       mangled-global-ids))))
	  (for-each (lambda (entry)
		      (let ((literalname (cadr entry)))
			(emit o "defliteral("literalname");\n")))
		    literal-table)
	  (emit o "\n")
	  (let ((literal-initialiser (new-function! "Startup" `(,*noreturn*)
						    "void" #f
						    `(("self" "oop")
						      ("argc" "int")
						      ("k" "continuation")))))
	    (for-each (lambda (entry)
			(let ((literalname (cadr entry))
			      (literalvalue (caddr entry)))
			  (add-instr! literal-initialiser
				      `(settemp ,literalname
						,(gen-literal-initialiser literal-initialiser
									  literalvalue)))))
		      (reverse literal-table))
	    (add-instr! literal-initialiser `(callfun ,first-function 1 k)))
	  (for-each (emit-structdef o) all-structures)
	  (for-each (emit-functionproto o) all-functions)
	  (emit o "\n")
	  (for-each (emit-functiondef o) all-functions)
	  ))

      (for-each display (list ";; GCC backend compiling "output-filename
			      (if (compiler$make-program)
				  " to executable\n"
				  " to library\n")))

      (let ((backend-path (string-append (path->string (current-load-relative-directory))
					 "/backend/c"))
	    (so (case (system-type)
		  ;; ((macosx) ".dylib")
		  (else ".so")))
	    (dummy-arg "-DNEWMOON_DUMMY_DEFINITION_BECAUSE_GCC_HATES_BLANK_SPACES"))
	(when (not (apply call-external-program
			  (or (getenv "NEWMOON_GCC")
			      (find-executable-path "glibtool")
			      (find-executable-path "libtool")
			      (error (string-append
				      "The environment variable NEWMOON_GCC"
				      " is not set, and neither 'glibtool'"
				      " nor 'libtool' was found on the $PATH.")))
			  "--tag=CC"
			  "--mode=link"
			  "gcc"
			  "-g"
			  "-fomit-frame-pointer"
			  "-foptimize-sibling-calls"
			  (if (compiler$make-program)
			      dummy-arg
			      "-dynamiclib")
			  (string-append "-I"backend-path)
			  (string-append "-L"backend-path)
			  "-o" (if (compiler$make-program)
				   (replace-filename-extension output-filename "")
				   (replace-filename-extension output-filename so))
			  output-filename
			  "-lnewmoon"
			  (compiler$extra-backend-args)
			  ))
	  (error "Call to external compiler failed - is $NEWMOON_GCC correct?"))
	))))
