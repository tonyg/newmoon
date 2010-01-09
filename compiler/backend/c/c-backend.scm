;; CPS C backend, Cheney-on-the-MTA style.

(define c-languages
  `(
    (id ,string?)
    (type ,string?)

    (structdef (structdef (name id)
			  (fields (%list-of vardef))))

    (functiondef (functiondef (name id)
			      (modifiers (%list-of ,string?))
			      (rettype type)
			      (variadic? ,boolean?)
			      (formals (%list-of vardef))
			      (locals (%list-of vardef))
			      (body (%list-of instr))))

    (vardef (vardef (name id)
		    (type type)))

    (instr ,(letrec ((instr? (lambda (instr)
			       (or (symbol? instr)
				   (string? instr)
				   (number? instr)
				   (and (pair? instr)
					(symbol? (car instr))
					(every instr? (cdr instr)))))))
	      instr?))
    ))

(define *noreturn* "__attribute__((noreturn))")

(define (make-vardef* def)
  (if (and (node? def) (node-kind? def 'vardef))
      def
      (make-vardef (car def) (cadr def))))

(define (make-vardef name type)
  (make-node 'vardef
	     'name name
	     'type type))

(define (make-structdef name fields)
  (make-node 'structdef
	     'name name
	     'fields (map make-vardef* fields)))

(define (make-functiondef name modifiers rettype is-variadic formals)
  (make-node 'functiondef
	     'name name
	     'modifiers modifiers
	     'rettype rettype
	     'variadic? is-variadic
	     'formals (map make-vardef* formals)
	     'locals '()
	     'body '()))

(define (add-local! functiondef localname localtype localinit)
  (node-push! functiondef 'functiondef 'locals (list localname localtype localinit)))

(define (add-instr! functiondef instr)
  (node-push! functiondef 'functiondef 'body instr))

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
   (else (error "Non-string-or-symbol in string-or-symbol->id" strsym))))

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
	(else (error "Invalid C type" t)))
      (emit port t)))

(define (emit-vardef port indent)
  (let ((pad (make-string indent #\space)))
    (lambda (vardef)
      (emit port pad)
      (emit-type port (node-get vardef 'vardef 'type))
      (emit port " "(node-get vardef 'vardef 'name)))))

(define (emit-structdef port)
  (lambda (structdef)
    (emit port "typedef struct S_"(node-get structdef 'structdef 'name))
    (emit-terminated-list port
			  (node-get structdef 'structdef 'fields)
			  (emit-vardef port 2) " {\n" ";\n" "} ")
    (emit port (node-get structdef 'structdef 'name)";\n\n")))

(define (emit-function-header port functiondef)
  (for-each (lambda (modifier) (emit port modifier" "))
	    (node-get functiondef 'functiondef 'modifiers))
  (emit-type port (node-get functiondef 'functiondef 'rettype))
  (emit port " "(node-get functiondef 'functiondef 'name))
  (emit-separated-list port
		       (node-get functiondef 'functiondef 'formals)
		       (emit-vardef port 0)
		       "(" ", " "")
  (if (node-get functiondef 'functiondef 'variadic?)
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
	      (node-get functiondef 'functiondef 'locals))
    (let ((extra-scope-count (fold (emit-instr port) 0
				   (reverse (node-get functiondef 'functiondef 'body)))))
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
  (mangle-id (node-get arginfo 'arginfo 'name)))

(define (capture-index capture)
  (node-get (node-get capture 'capture 'new-location) 'loc-environment 'index))

(define (capture-fields captures)
  (map (lambda (capture)
	 (let* ((arginfo (node-get capture 'capture 'arginfo)))
	   (let* ((index (capture-index capture)))
	     (cons index (make-vardef (string-append "env_"(number->string index)"_"
						     (arginfo->id arginfo))
				      (capture-type capture))))))
       captures))

(define (capture-index->name index capture-map)
  (cdr (assq index capture-map)))

(define (capture-type capture)
  'oop)

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
   ((negative? delta) (error "Numeric string too wide in zero-pad-left" (list str width)))
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
      (record-function! (make-functiondef name modifiers rettype is-variadic formals)))

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
       ((symbol? value) `(intern ,(escape-string (symbol->string value))))
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
		    ((constant-closure lambdaname)
		     (let ((name (next-temp)))
		       (add-instr! fn `(allocenv constant_closure_env 0 ,lambdaname ,name))
		       name))))
       (else
	(error "gen-literal-initialiser: unimplemented literal kind" value))))

    (define (build-closure parent-fn capture-instrs node receive-closure)
      (let* ((cont (node-get node 'cps-lambda 'cont))
	     (is-continuation (not cont)) ;; no continuation arg --> we *are* a continuation
	     (formals (node-get node 'cps-lambda 'formals))
	     (captures (node-get node 'cps-lambda 'captures))
	     (num-captures (length captures))
	     (varargs (node-get node 'cps-lambda 'varargs))
	     (expr (node-get node 'cps-lambda 'expr))
	     (arity (let ((fl (length formals))) (if varargs (- fl 1) fl)))
	     (lambdaname (next-lambda-name))
	     (envdefname (next-envdef-name))
	     (newcloname (next-temp))
	     (capture-map (capture-fields captures)))

	(define (formals->argdefs formals)
	  (cond
	   ((null? formals) '())
	   ((node-get (car formals) 'arginfo 'is-rest) (formals->argdefs (cdr formals)))
	   (else (cons `(,(arginfo->id (car formals)) oop) (formals->argdefs (cdr formals))))))

	(record-structure! (make-structdef envdefname `((header object_header)
							(code newmoon_code)
							,@(map cdr capture-map))))

	;; Construct closure instance
	(if (null? captures)
	    (set! newcloname (record-literal! #f
					      (lambda () (make-node 'constant-closure
								    'lambdaname lambdaname))))
	    (add-instr! parent-fn `(allocenv ,envdefname
					     ,(length capture-map)
					     ,lambdaname
					     ,newcloname)))
	(for-each (lambda (capture capture-instr)
		    (let* ((cap-cont (node-get (node-get capture 'capture 'arginfo)
					       'arginfo 'cont))
			   (index (capture-index capture))
			   (v (capture-index->name index capture-map)))
			(add-instr! parent-fn `(storeenv ,newcloname
							 ,(node-get v 'vardef 'name)
							 ,capture-instr))))
		  captures capture-instrs)

	(let ((code (new-function! lambdaname
				   `("static" ,*noreturn*)
				   'void
				   varargs
				   `((self (* ,envdefname))
				     (argc int)
				     ,@(if is-continuation
					   '()
					   `((k continuation)))
				     ,@(formals->argdefs formals)))))
	  (when varargs
	    (let ((rest-arginfo (car (filter (lambda (f) (node-get f 'arginfo 'is-rest))
					     formals)))
		  (penultimate-vardef (last (node-get code 'functiondef 'formals))))
	      (add-instrs! code `((deftemp ,(arginfo->id rest-arginfo) (mknull))
				  (extractvarargs ,(arginfo->id rest-arginfo)
						  ,arity
						  ,(node-get penultimate-vardef
							     'vardef 'name))))))
	  (for-each (lambda (arginfo)
		      (when (arginfo-boxed? arginfo)
			(begin
			  (compiler-assert continuation-arginfo-never-boxed
					   (not (node-get arginfo 'arginfo 'cont)))
			  (add-instr! code `(installbox ,(arginfo->id arginfo))))))
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

      (define (gen-local-load arginfo location)
	(node-match location
		    ((loc-continuation) 'k)
		    ((loc-argument index) (arginfo->id arginfo))
		    ((loc-environment index)
		     (let ((v (capture-index->name index capture-map)))
		       `(envref self ,(node-get v 'vardef 'name))))))

      (define (gen-local-store arginfo location instr)
	(node-match location
		    ((loc-continuation) `(settemp k ,instr))
		    ((loc-argument index) `(settemp ,(arginfo->id arginfo) ,instr))
		    ((loc-environment index) `(setenv env
						      ,(capture-index->name index capture-map)
						      ,instr))))

      (define (gen-local-get arginfo location)
	(if (arginfo-boxed? arginfo)
	    `(getbox ,(gen-local-load arginfo location))
	    (gen-local-load arginfo location)))

      (define (gen-global-get name)
	`(globalget ,(mangle-id (record-global! name))))

      (define (gen-closure-instantiation node receive-closure)
	(let* ((captures (node-get node 'cps-lambda 'captures))
	       (capture-instrs (map (lambda (capture)
				      (let* ((old-loc (node-get capture 'capture 'old-location))
					     (arginfo (node-get capture 'capture 'arginfo)))
					(gen-local-load arginfo old-loc)))
				    captures)))
	  (build-closure fn capture-instrs node receive-closure)))

      (define (gen-asm formals actuals code)
	(let* ((c-code (cond
			((find (lambda (clause)
				 (eq? 'c (node-get clause 'backend-asm 'name)))
			       code)
			 => (lambda (clause) (node-get clause 'backend-asm 'code)))
			(else (error "cps-asm missing c clause" node))))
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
							   (error "cps-asm: unknown formal"
								  (list instr node)))))
						 ((string? instr)
						  instr)
						 (else (error "cps-asm: invalid C syntax"
							      (list instr node)))))
					      c-code))))
	  (add-instr! fn `(end-literal-c ,tempname))
	  tempname))

      (define (gen-backend backend-name arguments)
	(when (eq? backend-name 'c)
	  (record-literal-c-stanza! arguments))
	`(mkvoid))

      (define (gen-local-set arginfo location expr)
	(if (arginfo-boxed? arginfo)
	    `(setbox ,(gen-local-load arginfo location)
		     ,(gen expr))
	    (gen-local-store arginfo location (gen expr))))

      (define (gen-global-set name expr)
	`(globalset ,(mangle-id (record-global! name)) ,(gen expr)))

      (define (gen-apply cont rator rands)
	(let ((arity (length rands)))
	  (if (node-kind? rator 'cps-lambda)
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
		    ((cps-lit value) (gen-literal value))
		    ((cps-void) `(mkvoid))
		    ((cps-local-get name arginfo location) (gen-local-get arginfo location))
		    ((cps-global-get name) (gen-global-get name))
		    ((cps-lambda cont formals varargs captures globals expr)
		     (gen-closure-instantiation node (lambda (varname lambdaname) varname)))
		    ((cps-asm formals actuals code) (gen-asm formals actuals code))
		    ((cps-backend backend-name arguments) (gen-backend backend-name arguments))
		    ((cps-local-set name arginfo location expr)
		     (gen-local-set arginfo location expr))
		    ((cps-global-set name expr) (gen-global-set name expr))
		    ((cps-apply cont rator rands) (gen-apply cont rator rands))
		    ((cps-begin head tail) (gen-begin head tail))
		    ((cps-if test true false) (gen-if test true false))))
      (gen node))

    (define (gen-module-entry-point node)
      ;; We're relying here on being handed a cps-lambda node, that is
      ;; a real lambda-proc, not a lambda-cont (or lambda-jump, once
      ;; we implement those).
      (compiler-assert module-entry-point-is-lambda-proc
		       (and (node-kind? node 'cps-lambda)
			    (node-get node 'cps-lambda 'cont)))
      ;; It also has to have no captures.
      (compiler-assert module-entry-point-has-no-captures
		       (null? (node-get node 'cps-lambda 'captures)))
      (build-closure 'invalid-parent-fn '() node (lambda (varname lambdaname) varname)))

    (define (gen-program-entry-point)
      (let ((entry (new-function! "main" '() 'int #f `((argc int) (argv (* char))))))
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
	  (let* ((global-initialiser (new-function! "InitGlobals" '() 'void #f `()))
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
	  (let ((literal-initialiser (new-function! "Startup" `() 'void #f
						    `((k continuation)))))
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
	    (dummy-arg "-DNEWMOON_DUMMY_DEFINITION_BECAUSE_GCC_HATES_BLANK_SPACES"))
	(when
	    (not (call-external-program (or (getenv "NEWMOON_GCC")
					    (find-executable-path "glibtool")
					    (find-executable-path "libtool")
					    (error (string-append
						    "The environment variable NEWMOON_GCC"
						    " is not set, and neither 'glibtool'"
						    " nor 'libtool' was found on the $PATH.")))
					"--mode=link"
					"gcc"
					"-g"
					"-O3"
					"-fomit-frame-pointer"
					"-foptimize-sibling-calls"
					"-fno-pic"
					(string-append "-I"backend-path)
					(string-append "-L"backend-path)
					(if (compiler$make-program)
					    dummy-arg
					    "-c")
					"-o" (if (compiler$make-program)
						 (replace-filename-extension output-filename "")
						 (replace-filename-extension output-filename ".so"))
					output-filename
					(if (compiler$make-program)
					    "-lnewmoon"
					    dummy-arg)
					))
	  (error "Call to external compiler failed - is $NEWMOON_GCC correct?"))
	))))
