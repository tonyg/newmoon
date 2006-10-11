(define dotnet-languages
  `(
    (id ,string?)
    (type ,string?)

    (classdef (classdef (name id)
			(extends type)
			(fields (%list-of vardef))
			(methods (%list-of methdef))))

    (vardef (vardef (name id)
		    (type type)))

    (methdef (methdef (name id)
		      (flags (%list-of ,symbol?))
		      (rettype type)
		      (formals (%list-of vardef))
		      (locals (%list-of vardef))
		      (body (%list-of instr))))

    (instr ,(lambda (instr)
	      (or (symbol? instr)
		  (string? instr)
		  (and (pair? instr)
		       (or (symbol? (car instr))
			   (every string? instr))))))
    ))

(define *max-non-varargs-arity* 4)

(define *il-cont-type* "class [Newmoon]Newmoon.Continuation")
(define *il-closure-type* "class [Newmoon]Newmoon.Closure")
(define *il-module-type* "class [Newmoon]Newmoon.Module")
(define *il-environment-type* "class [Newmoon]Newmoon.Environment")
(define *il-cell-type* "class [Newmoon]Newmoon.Cell")
(define *il-binding-type* "class [Newmoon]Newmoon.Binding")
(define *il-list-type* "class [Newmoon]Newmoon.List")
(define *il-objarray-type* "object[]")

(define *il-wrongargc-ctor* "instance void class [Newmoon]Newmoon.WrongArgCount::.ctor(int32, int32, bool, bool)")
(define *il-schemestring-ctor* "instance void class [Newmoon]Newmoon.SchemeString::.ctor(string, bool)")
(define *il-pair-ctor* "instance void class [Newmoon]Newmoon.Pair::.ctor(object, object)")
(define *il-cell-ctor* "instance void class [Newmoon]Newmoon.Cell::.ctor(object)")
(define *il-module-ctor* "instance void class [Newmoon]Newmoon.Module::.ctor(string, class [Newmoon]Newmoon.Environment)")

(define *il-undefined-field* "class [Newmoon]Newmoon.Undefined [Newmoon]Newmoon.Undefined::UNDEFINED")
(define *il-null-field* "class [Newmoon]Newmoon.Null [Newmoon]Newmoon.Null::NULL")
(define *il-cell-value-field* "object [Newmoon]Newmoon.Cell::'value'")
(define *il-binding-value-field* "object [Newmoon]Newmoon.Binding::'value'")
(define *il-closure-module-field* "class [Newmoon]Newmoon.Module [Newmoon]Newmoon.Closure::module")

(define *il-reply-varargs-method* "instance object class [Newmoon]Newmoon.Continuation::ReplyVarargs(object[])")
(define *il-apply-varargs-method* "instance object class [Newmoon]Newmoon.Closure::ApplyVarargs(class [Newmoon]Newmoon.Continuation, object[])")
(define *il-resolve-binding-cell-method* "instance class [Newmoon]Newmoon.Binding class [Newmoon]Newmoon.Module::ResolveBindingCell(string, string)")

(define *il-get-type-from-handle-method* "class [mscorlib]System.Type class [mscorlib]System.Type::GetTypeFromHandle(valuetype [mscorlib]System.RuntimeTypeHandle)")
(define *il-program-entry-point-method* "void [Newmoon]Newmoon.Driver::ProgramEntryPoint(string[], class [mscorlib]System.Type)")
(define *il-writeline-method* "void class [mscorlib]System.Console::WriteLine(string)")

(define (make-counter receiver)
  (let ((counter 0))
    (lambda ()
      (let ((c counter))
	(set! counter (+ counter 1))
	(receiver c)))))

;; Must match Newmoon.Environment.Mangle(string) in Environment.cs
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

(define (make-classdef name extends)
  (make-node 'classdef
	     'name (string-or-symbol->id name)
	     'extends extends
	     'fields '()
	     'methods '()))

(define (classdef-add-field! classdef name type)
  (let ((fielddef (make-node 'vardef
			     'name (string-or-symbol->id name)
			     'type type)))
    (node-push! classdef 'classdef 'fields fielddef)
    fielddef))

(define (classdef-add-method! classdef name flags rettype formals)
  (let ((methdef (make-node 'methdef
			    'name (string-or-symbol->id name)
			    'flags flags
			    'rettype rettype
			    'formals (map (lambda (entry)
					    (make-node 'vardef
						       'name (string-or-symbol->id (car entry))
						       'type (cadr entry)))
					  formals)
			    'locals '()
			    'body '())))
    (node-push! classdef 'classdef 'methods methdef)
    methdef))

(define (add-local! methdef name type)
  (node-push! methdef 'methdef 'locals (make-node 'vardef
						  'name (string-or-symbol->id name)
						  'type type))
  name)

(define (add-instr! methdef instr)
  (node-push! methdef 'methdef 'body instr))

(define (add-instrs! methdef instrs)
  (for-each (lambda (i) (add-instr! methdef i)) instrs))

(define (emit port . items)
  (emit* port items))

(define (emit* port items)
  (for-each (lambda (item) (display item port)) items))

(define (il-file-prologue port assemblyname)
  (emit port
	"// Generated by Newmoon\n"
	".assembly '"assemblyname"' {}\n"
	".assembly extern mscorlib {}\n"
	".assembly extern Newmoon {}\n"
	"\n"))

(define (for-each-reverse fn lst)
  (for-each fn (reverse lst)))

(define (emit-classdef port)
  (lambda (classdef)
    (check-language classdef 'classdef dotnet-languages type-error)
    (emit port
	  ".class public auto ansi beforefieldinit "(node-get classdef 'classdef 'name)"\n"
	  #\tab"extends "(node-get classdef 'classdef 'extends)"\n"
	  "{\n")
    (for-each-reverse (emit-fielddef port) (node-get classdef 'classdef 'fields))
    (for-each-reverse (emit-methdef port) (node-get classdef 'classdef 'methods))
    (emit port
	  "}\n\n")))

(define (emit-fielddef port)
  (lambda (fielddef)
    (emit port
	  #\tab".field public "(node-get fielddef 'vardef 'type)" "
	  (node-get fielddef 'vardef 'name)"\n")))

(define (emit-methdef port)
  (lambda (methdef)
    (let ((flags (node-get methdef 'methdef 'flags)))
      (emit port "\n")
      (if (memq 'static flags)
	  (emit port #\tab".method public static hidebysig default\n")
	  (begin
	    (emit port #\tab".method public")
	    (if (memq 'virtual flags) (emit port " virtual"))
	    (if (memq 'newslot flags) (emit port " newslot"))
	    (emit port " hidebysig "
		  (if (string=? (node-get methdef 'methdef 'name) ".ctor")
		      "specialname rtspecialname "
		      "")
		  "instance default\n")))
      (emit port
	    #\tab (node-get methdef 'methdef 'rettype)
	    " "(node-get methdef 'methdef 'name))
      (emit-vardefs port (node-get methdef 'methdef 'formals))
      (emit port "\n"#\tab"cil managed\n"#\tab"{\n")
      (if (memq 'entrypoint flags)
	  (emit port #\tab #\tab ".entrypoint\n"))
      (let ((locals (node-get methdef 'methdef 'locals)))
	(if (not (null? locals))
	    (begin
	      (emit port #\tab #\tab ".locals init ")
	      (emit-vardefs port (node-get methdef 'methdef 'locals))
	      (emit port "\n"))))
      (emit-instructions port methdef (node-get methdef 'methdef 'body))
      (emit port #\tab"}\n"))))

(define (emit-vardefs port formals)
  (emit port "(")
  (let loop ((formals formals)
	     (need-comma #f))
    (if (null? formals)
	(emit port ")")
	(let* ((formal (car formals))
	       (type (node-get formal 'vardef 'type))
	       (name (node-get formal 'vardef 'name)))
	  (if need-comma (emit port ", "))
	  (emit port type" "name)
	  (loop (cdr formals) #t)))))

(define (lookup-methdef-formal-index methdef formalname)
  (let ((formalname (string-or-symbol->id formalname)))
    (let loop ((index (if (memq 'static (node-get methdef 'methdef 'flags)) 0 1))
	       (formals (node-get methdef 'methdef 'formals)))
      (if (null? formals)
	  (error "Internal error in assembler: Formal argument not found" formalname)
	  (let* ((candidate (car formals))
		 (name (node-get candidate 'vardef 'name)))
	    (if (string=? name formalname)
		index
		(loop (+ index 1) (cdr formals))))))))

(define (emit-instructions port methdef instrs)
  (define (emit-instruction instr)
    (cond
     ((or (symbol? instr) (string? instr))
      (emit port #\tab instr":\n"))
     ((and (memq (car instr) '(ldarg starg))
	   (or (symbol? (cadr instr))
	       (string? (cadr instr))))
      (let* ((name (cadr instr))
	     (index (lookup-methdef-formal-index methdef name)))
	(emit-instruction `(,(car instr) ,index ,@(cddr instr)
			    ,(string-append "// "(string-or-symbol->id name))))))
     (else
      (emit port #\tab)
      (for-each (lambda (part) (emit port #\tab part)) instr)
      (emit port "\n"))))
  (for-each-reverse emit-instruction instrs))

(define (format-types typelist)
  (string-concatenate (list "("
			    (reduce-right (lambda (a b) (string-append a", "b)) "" typelist)
			    ")")))

(define (arginfo->id arginfo)
  (mangle-id (node-get arginfo 'arginfo 'name)))

(define (formals->argdefs formals)
  (map (lambda (arginfo)
	 (list (arginfo->id arginfo)
	       (if (node-get arginfo 'arginfo 'is-rest)
		   *il-list-type*
		   "object")))
       formals))

(define (body-argdefs-from is-continuation argdefs)
  (if is-continuation
      argdefs
      (cons `(k ,*il-cont-type*) argdefs)))

(define (capture-index capture)
  (node-get (node-get capture 'capture 'new-location) 'loc-environment 'index))

(define (gen-capture-fields! classdef captures)
  (map (lambda (capture)
	 (let* ((arginfo (node-get capture 'capture 'arginfo)))
	   (let* ((index (capture-index capture))
		  (field (classdef-add-field! classdef
					      (string-append "env_"(number->string index)"_"
							     (arginfo->id arginfo))
					      (capture-type capture))))
	     (cons index (node-get field 'vardef 'name)))))
       captures))

(define (global->fieldname global)
  (string-append "global_"(mangle-id global)))

(define (capture-index->name index capture-map)
  (cdr (assq index capture-map)))

(define (capture-cont? capture)
  (node-get (node-get capture 'capture 'arginfo) 'arginfo 'cont))

(define (capture-type capture)
  (if (capture-cont? capture) *il-cont-type* "object"))

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
			 (if (or (< i 32) (>= i 128)) ;; assume ASCII %%%
			     (append '(#\\)
				     (string->list (zero-pad-left (number->string i 8) 3))
				     acc)
			     (if (char=? ch #\")
				 (cons* #\\ #\" acc)
				 (cons ch acc)))))
		     '(#\")
		     (string->list str)))))

(define (debug-code . strs)
  (if #f
      `((ldstr ,(escape-string (string-concatenate strs)))
	(call ,*il-writeline-method*))
      '()))

(define (compiler-back-end-phases input-filename frontend-result)
  (let* ((next-lambda-name (make-counter (lambda (c) (string-append "Lambda_" (number->string c)))))
	 (next-label (make-counter (lambda (c) (string-append "L" (number->string c)))))
	 (assembly-name (if (compiler$target-namespace)
			    (compiler$target-namespace)
			    (replace-filename-extension input-filename "")))
	 (fq-namespace (string-append "Newmoon.CompiledModules."(mangle-id assembly-name)))
	 (output-filename (replace-filename-extension input-filename ".il"))
	 (statics-classdef (make-classdef "Statics" "[Newmoon]Newmoon.Module"))
	 (statics-classname (string-append "class "fq-namespace".Statics"))
	 (literal-table (make-hash-table 'equal))
	 (global-table (make-hash-table))
	 (all-classdefs (list statics-classdef)))

    (define (qualify id)
      (string-append fq-namespace"."id))

    (define (qualify-class id)
      (string-append "class "fq-namespace"."id))

    (define (record-generated-class! classdef)
      (set! all-classdefs (cons classdef all-classdefs)))

    (define (build-closure node)
      (let* ((cont (node-get node 'cps-lambda 'cont))
	     (is-continuation (not cont)) ;; no continuation arg --> we *are* a continuation
	     (formals (node-get node 'cps-lambda 'formals))
	     (captures (node-get node 'cps-lambda 'captures))
	     (num-captures (length captures))
	     (varargs (node-get node 'cps-lambda 'varargs))
	     (globals (node-get node 'cps-lambda 'globals))
	     (expr (node-get node 'cps-lambda 'expr))
	     (arity (let ((fl (length formals))) (if varargs (- fl 1) fl)))
	     (supertype (if is-continuation
			    "[Newmoon]Newmoon.TailContinuation"
			    "[Newmoon]Newmoon.TailClosure"))
	     (lambdaname (next-lambda-name))
	     (lambdatype (qualify-class lambdaname))
	     (classdef (make-classdef lambdaname supertype))
	     (capture-map (gen-capture-fields! classdef captures))
	     (ctor-formals (cons `(module ,*il-module-type*) (capture-source-args captures))))

	(define (gen-boxing-actions methdef)
	  (for-each (lambda (arginfo)
		      (if (arginfo-boxed? arginfo)
			  (begin
			    (compiler-assert continuation-arginfo-never-boxed
					     (not (node-get arginfo 'arginfo 'cont)))
			    (add-instrs! methdef `((ldarg ,(arginfo->id arginfo))
						   (newobj ,*il-cell-ctor*)
						   (starg ,(arginfo->id arginfo)))))))
		    formals))

	(define (gen-fixed-arity)
	  (let ((applyv (classdef-add-method! classdef
					      (if is-continuation "ReplyVarargs" "ApplyVarargs")
					      '(virtual) "object"
					      (body-argdefs-from is-continuation
								 `((args ,*il-objarray-type*))))))
	    (add-instrs! applyv
			 `(,@(debug-code lambdatype" fixvar")
			   (ldarg args)
			   (ldlen)
			   (ldc.i4 ,arity)
			   (bne.un wrongArity)
			   (ldarg.0)
			   ,@(if is-continuation
				 '()
				 '((ldarg k)))))
	    (do ((i 0 (+ i 1)))
		((= i arity))
	      (add-instrs! applyv
			   `((ldarg args)
			     (ldc.i4 ,i)
			     (ldelem.ref))))
	    (add-instrs! applyv
			 `((tail.)
			   ,(if is-continuation
				`(call ,(string-append
					 "instance object "lambdatype"::Reply"
					 (format-types (make-list arity "object"))))
				`(call ,(string-append
					 "instance object "lambdatype"::Apply"
					 (format-types (cons *il-cont-type*
							     (make-list arity "object"))))))
			   (ret)
			   wrongArity
			   (ldc.i4 ,arity)
			   (ldarg args)
			   (ldlen)
			   (ldc.i4 0)
			   (ldc.i4 ,(if is-continuation 1 0))
			   (newobj ,*il-wrongargc-ctor*)
			   (throw))))
	  (let ((applyn (classdef-add-method! classdef
					      (if is-continuation "Reply" "Apply")
					      '(virtual) "object"
					      (body-argdefs-from is-continuation
								 (formals->argdefs formals)))))
	    (add-instrs! applyn (debug-code lambdatype" reply/apply"))
	    (gen-boxing-actions applyn)
	    (gen-node applyn lambdatype capture-map expr)))

	(define (gen-variable-arity)
	  (let ((applyv (classdef-add-method! classdef
					      (if is-continuation "ReplyVarargs" "ApplyVarargs")
					      '(virtual) "object"
					      (body-argdefs-from is-continuation
								 `((args ,*il-objarray-type*))))))
	    (add-instrs! applyv (debug-code lambdatype" varvar"))
	    (add-local! applyv 'restarg *il-list-type*)
	    (add-local! applyv 'counter "int32")
	    (add-instrs! applyv
			 `((ldarg args)
			   (ldlen)
			   (ldc.i4 ,arity)
			   (blt.un tooFewArguments)

			   (ldsfld ,*il-null-field*)
			   (stloc restarg)
			   (ldarg args)
			   (ldlen)
			   (ldc.i4 1)
			   (sub)
			   (stloc counter)

			   consLoopTop
			   (ldloc counter)
			   (ldc.i4 ,arity)
			   (blt.un consLoopDone)

			   (ldarg args)
			   (ldloc counter)
			   (ldelem.ref)
			   (ldloc restarg)
			   (newobj ,*il-pair-ctor*)
			   (stloc restarg)

			   (ldloc counter)
			   (ldc.i4 1)
			   (sub)
			   (stloc counter)
			   (br consLoopTop)

			   consLoopDone
			   (ldarg.0)
			   ,@(if is-continuation
				 '()
				 '((ldarg k)))))
	    (do ((i 0 (+ i 1)))
		((= i arity))
	      (add-instrs! applyv
			   `((ldarg args)
			     (ldc.i4 ,i)
			     (ldelem.ref))))
	    (add-instrs! applyv
			 `((ldloc restarg)
			   (tail.)
			   (call ,(string-append
				   "instance object "lambdatype"::Body"
				   (format-types `(,@(if is-continuation '() (list *il-cont-type*))
						   ,@(make-list arity "object")
						   ,*il-list-type*))))
			   (ret)

			   tooFewArguments
			   (ldc.i4 ,arity)
			   (ldarg args)
			   (ldlen)
			   (ldc.i4 1)
			   (ldc.i4 ,(if is-continuation 1 0))
			   (newobj ,*il-wrongargc-ctor*)
			   (throw))))
	  (let ((applyn (classdef-add-method! classdef
					      "Body"
					      '() "object"
					      (body-argdefs-from is-continuation
								 (formals->argdefs formals)))))
	    (add-instrs! applyn (debug-code lambdatype" body"))
	    (gen-boxing-actions applyn)
	    (gen-node applyn lambdatype capture-map expr)))

	(let ((ctor (classdef-add-method! classdef ".ctor" '() "void" ctor-formals)))
	  (add-instrs! ctor `((ldarg.0)
			      (ldarg module)
			      (call ,(string-append "instance void "supertype
						    "::.ctor("*il-module-type*")"))
			      ,@(debug-code lambdatype" ctor")))
	  (for-each (lambda (global)
		      (let ((fieldname (global->fieldname global)))
			(classdef-add-field! classdef 
					     (global->fieldname global)
					     *il-binding-type*)
			(add-instrs! ctor `((ldarg.0)
					    (ldarg module)
					    (ldstr ,(escape-string (symbol->string global)))
					    (ldstr ,(escape-string "global"))
					    (call ,*il-resolve-binding-cell-method*)
					    (stfld ,(string-append *il-binding-type*" "lambdatype
								   "::"fieldname))))))
		    globals)
	  (for-each (lambda (capture)
		      (let* ((cap-cont (node-get (node-get capture 'capture 'arginfo)
						 'arginfo 'cont))
			     (index (capture-index capture)))
			(add-instrs! ctor `((ldarg.0)
					    (ldarg ,(capture-source-arg-for-index index))
					    (stfld ,(string-append
						     (if cap-cont *il-cont-type* "object")
						     " "lambdatype"::"
						     (capture-index->name index capture-map)))))))
		    captures)
	  (add-instr! ctor '(ret)))

	(if varargs
	    (gen-variable-arity)
	    (gen-fixed-arity))

	(record-generated-class! classdef)
	(string-append lambdatype"::.ctor"(format-types (map cadr ctor-formals)))))

    (define (gen-literal methdef value)
      (cond
       ((number? value) (if (and (exact? value)
				 (integer? value))
			    (add-instrs! methdef `((ldc.i4 ,value)
						   (box "int32")))
			    (add-instrs! methdef `((ldc.r8 ,value)
						   (box "float64")))))
       ((symbol? value) (add-instr! methdef `(ldstr ,(escape-string (symbol->string value)))))
       ((string? value) (add-instrs! methdef `((ldstr ,(escape-string value))
					       (ldc.i4 0)
					       (newobj ,*il-schemestring-ctor*))))
       ((boolean? value) (add-instrs! methdef `((ldc.i4 ,(if value 1 0))
						(box "bool"))))
       ((char? value) (add-instrs! methdef `((ldc.i4 ,(char->integer value))
					     (box "char"))))
       ((null? value) (add-instr! methdef
				  `(ldsfld ,*il-null-field*)))
       ((pair? value)
	(gen-literal methdef (car value))
	(gen-literal methdef (cdr value))
	(add-instr! methdef `(newobj ,*il-pair-ctor*)))
       ((vector? value)
	(let ((len (vector-length value)))
	  (add-instrs! methdef `((ldc.i4 ,len)
				 (newarr "object")))
	  (do ((i 0 (+ i 1)))
	      ((= i len))
	    (add-instrs! methdef `((dup)
				   (ldc.i4 ,i)))
	    (gen-literal methdef (vector-ref value i))
	    (add-instr! methdef '(stelem.ref)))))
       (else
	(error "gen-literal: unimplemented literal kind" value))))

    (define (gen-node methdef lambdatype capture-map node)
      ;; Perhaps need a headposition/tailposition indicator, so that
      ;; we don't generate redundant computations?
      (define (gen-void want-value)
	(gen-linkage/no-value want-value))

      (define (gen-linkage/value want-value)
	(if want-value
	    #t ;; do nothing - have a value and want a value
	    ;; Otherwise, pop a value as what we have is unwanted
	    (add-instr! methdef `(pop))))

      (define (gen-linkage/no-value want-value)
	(if want-value
	    (add-instr! methdef `(ldsfld ,*il-undefined-field*))
	    #t))

      (define (gen-local-load arginfo location)
	(node-match location
		    ((loc-continuation)
		     (add-instr! methdef `(ldarg k)))
		    ((loc-argument index)
		     (add-instr! methdef `(ldarg ,(arginfo->id arginfo))))
		    ((loc-environment index)
		     (add-instrs! methdef `((ldarg.0)
					    (ldfld ,(string-append "object "lambdatype"::"
								   (capture-index->name
								    index capture-map))))))))

      (define (gen-local-store arginfo location expr-thunk)
	(node-match location
		    ((loc-continuation)
		     (add-instr! methdef `(starg k)))
		    ((loc-argument index)
		     (add-instr! methdef `(starg ,(arginfo->id arginfo))))
		    ((loc-environment index)
		     (add-instr! methdef `(ldarg.0))
		     (expr-thunk)
		     (add-instr! methdef `(stfld ,(string-append "object "lambdatype"::"
								 (capture-index->name
								  index capture-map)))))))

      (define (gen-local-get want-value arginfo location)
	(when want-value
	  (gen-local-load arginfo location)
	  (if (arginfo-boxed? arginfo)
	      (add-instrs! methdef
			   `((castclass ,*il-cell-type*)
			     (ldfld ,*il-cell-value-field*))))
	  (gen-linkage/value want-value)))

      (define (gen-global-get want-value name)
	(when want-value
	  (add-instrs! methdef `((ldarg.0)
				 (ldfld ,(string-append *il-binding-type*" "lambdatype"::"
							(global->fieldname name)))
				 (ldfld ,*il-binding-value-field*)))
	  (gen-linkage/value want-value)))

      (define (gen-closure-instantiation want-value node captures)
	(when want-value
	  (let ((ctor-token (build-closure node)))
	    (add-instrs! methdef `((ldarg.0)
				   (ldfld ,*il-closure-module-field*)))
	    (for-each (lambda (capture)
			(let* ((old-loc (node-get capture 'capture 'old-location))
			       (arginfo (node-get capture 'capture 'arginfo)))
			  (gen-local-load arginfo old-loc)))
		      captures)
	    (add-instr! methdef `(newobj "instance void" ,ctor-token))
	    (gen-linkage/value want-value))))

      (define (gen-asm want-value formals actuals code)
	(let* ((dotnet-code (cond
			     ((find (lambda (clause)
				      (eq? 'dotnet (node-get clause 'backend-asm 'name)))
				    code)
			      => (lambda (clause) (node-get clause 'backend-asm 'code)))
			     (else (error "cps-asm missing dotnet clause" node))))
	       (env (map cons formals actuals))
	       (labels (map (lambda (orig)
			      (cons orig (string-append (next-label) (mangle-id orig))))
			    (filter symbol? dotnet-code))))
	  (for-each (lambda (instr)
		      (cond
		       ((symbol? instr) (add-instr! methdef (cdr (assq instr labels))))
		       ((eq? (car instr) '$)
			(gen #t (cdr (or (assq (cadr instr) env)
					 (error "cps-asm: unknown formal" (list instr node))))))
		       (else
			(let ((alpha-converted-instr
			       (map (lambda (x) (if (symbol? x)
						    (let ((cell (assq x labels)))
						      (if cell
							  (cdr cell)
							  x))
						    x))
				    instr)))
			  (add-instr! methdef alpha-converted-instr)))))
		    dotnet-code)
	  (gen-linkage/value want-value)))

      (define (gen-local-set want-value arginfo location expr)
	(if (arginfo-boxed? arginfo)
	    (begin
	      (gen-local-load arginfo location)
	      (add-instr! methdef `(castclass ,*il-cell-type*))
	      (gen #t expr)
	      (add-instr! methdef `(stfld ,*il-cell-value-field*)))
	    (gen-local-store arginfo location (lambda () (gen #t expr))))
	(gen-linkage/no-value want-value))

      (define (gen-global-set want-value name expr)
	(add-instrs! methdef `((ldarg.0)
			       (ldfld ,(string-append *il-binding-type*" "lambdatype"::"
						      (global->fieldname name)))))
	(gen #t expr)
	(add-instr! methdef `(stfld ,*il-binding-value-field*))
	(gen-linkage/no-value want-value))

      (define (build-argvec arity rands)
	(add-instrs! methdef `((ldc.i4 ,arity)
			       (newarr "object")))
	(do ((i 0 (+ i 1))
	     (rands rands (cdr rands)))
	    ((= i arity))
	  (add-instrs! methdef `((dup)
				 (ldc.i4 ,i)))
	  (gen #t (car rands))
	  (add-instr! methdef '(stelem.ref))))

      (define (gen-apply want-value cont rator rands)
	(compiler-assert gen-apply-called-in-wanted-value-context
			 want-value)
	(let ((arity (length rands)))
	  (gen #t rator)
	  (if (> arity *max-non-varargs-arity*)
	      (if cont
		  (begin
		    (build-argvec arity rands)
		    (add-instrs! methdef `((tail.)
					   (callvirt ,*il-reply-varargs-method*))))
		  (begin
		    (gen #t (car rands))
		    (build-argvec (- arity 1) (cdr rands))
		    (add-instrs! methdef `((tail.)
					   (callvirt ,*il-apply-varargs-method*)))))
	      (if cont
		  (begin
		    (for-each gen/value rands)
		    (add-instrs! methdef
				 `((tail.)
				   (callvirt ,(string-append
					       "instance object "*il-cont-type*"::Reply"
					       (format-types (make-list arity "object")))))))
		  (begin
		    (for-each gen/value rands)
		    (add-instrs! methdef
				 `((tail.)
				   (callvirt ,(string-append
					       "instance object "*il-closure-type*"::Apply"
					       (format-types (cons *il-cont-type*
								   (make-list (- arity 1)
									      "object"))))))))))
	  (add-instr! methdef '(ret))))

      (define (gen-begin want-value head tail)
	(gen #f head)
	(gen want-value tail))

      (define (gen-if want-value test true false)
	(let ((label-true/pop (next-label))
	      (label-true (next-label))
	      (label-done (next-label)))
	  (gen #t test)
	  (add-instrs! methdef `((dup)
				 (isinst "bool")
				 (brfalse ,label-true/pop)
				 (unbox "bool")
				 (ldind.i1)
				 (brtrue ,label-true)))
	  (gen want-value false)
	  (add-instrs! methdef `((br ,label-done)
				 ,label-true/pop
				 (pop)
				 ,label-true))
	  (gen want-value true)
	  (add-instrs! methdef `(,label-done))))

      (define (gen/value node)
	(gen #t node))

      (define (gen want-value node)
	(node-match node
		    ((cps-lit value) (if want-value (gen-literal methdef value)))
		    ((cps-void) (gen-void want-value))
		    ((cps-local-get name arginfo location)
		     (gen-local-get want-value arginfo location))
		    ((cps-global-get name) (gen-global-get want-value name))
		    ((cps-lambda cont formals varargs captures globals expr)
		     (gen-closure-instantiation want-value node captures))
		    ((cps-asm formals actuals code) (gen-asm want-value formals actuals code))
		    ((cps-local-set name arginfo location expr)
		     (gen-local-set want-value arginfo location expr))
		    ((cps-global-set name expr) (gen-global-set want-value name expr))
		    ((cps-apply cont rator rands) (gen-apply want-value cont rator rands))
		    ((cps-begin head tail) (gen-begin want-value head tail))
		    ((cps-if test true false) (gen-if want-value test true false))))

      (gen #t node))

    (define (gen-module-constructor)
      (let ((ctor (classdef-add-method! statics-classdef ".ctor" '() "void"
					`((env ,*il-environment-type*)))))
	(add-instrs! ctor `((ldarg.0)
			    (ldstr ,(escape-string assembly-name))
			    (ldarg env)
			    (call ,*il-module-ctor*)
			    ,@(debug-code "modctor")
			    (ret)))))

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
      (let ((entry (classdef-add-method! statics-classdef "GetEntryPoint"
					 '(virtual) *il-closure-type*
					 '()))
	    (ctor-token (build-closure node)))
	(add-instrs! entry `(,@(debug-code "GetEntryPoint")
			     (ldarg.0) ;; this == module
			     (newobj "instance void" ,ctor-token)
			     (ret)))))

    (define (gen-program-entry-point)
      (let ((entry (classdef-add-method! statics-classdef "Main" '(static entrypoint) "void"
					 `((argv "string[]")))))
	(add-instrs! entry `((ldarg argv)
			     (ldtoken ,statics-classname)
			     (call ,*il-get-type-from-handle-method*)
			     (call ,*il-program-entry-point-method*)
			     (ret)))))

    ;;---------------------------------------------------------------------------
    (for-each display (list ";; dotnet backend compiling to namespace "fq-namespace"\n"))
    (gen-module-constructor)
    (gen-module-entry-point frontend-result)

    (if (compiler$make-program)
	(gen-program-entry-point))

    (for-each display (list ";; dotnet backend generating "output-filename"\n"))
    (delete-file-if-exists output-filename)
    (call-with-output-file output-filename
      (lambda (o)
	(il-file-prologue o (mangle-id assembly-name))
	(emit o ".namespace "fq-namespace"\n{\n\n")
	(for-each (emit-classdef o) all-classdefs)
	(emit o "\n} // namespace "fq-namespace"\n")))

    (for-each display (list ";; dotnet backend assembling "output-filename
			    (if (compiler$make-program)
				" to executable\n"
				" to library\n")))
    (if (not (call-external-program (or (getenv "NEWMOON_ILASM")
					(find-executable-path "ilasm")
					(error (string-append
						"The environment variable NEWMOON_ILASM"
						" is not set, and 'ilasm' was not found"
						" on the $PATH.")))
				    (if (compiler$make-program) "/exe" "/dll")
				    output-filename))
	(error "Call to external assembler failed - is $NEWMOON_ILASM correct?"))
    ))
