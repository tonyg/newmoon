(define-primitive %%lookup-field
  (specific-method "System.Type" "GetField" symbol "System.Reflection.BindingFlags"))
(define-primitive %%get-all-fields (specific-method "System.Type" "GetFields"))
(define-primitive %%parseEnum (specific-static "System.Enum" "Parse" "System.Type" symbol bool))
(define-primitive ->int32 (generic-static "System.Convert" "ToInt32"))
(define-primitive %%fi-type (specific-property-reader "System.Reflection.FieldInfo" "FieldType"))
(define-primitive %%fi-value (specific-method "System.Reflection.FieldInfo" "GetValue" object))
(define-primitive %%opcode-name (specific-property-reader "System.Reflection.Emit.OpCode" "Name"))
(define-primitive %%emit (generic-method "Emit"))

(define *opcodes*
  (let* ((<opcodes> (lookup-type "System.Reflection.Emit.OpCodes"))
	 (opcodes (map (lambda (fi) (%%fi-value fi 'dummy))
		       (vector->list (%%get-all-fields <opcodes>)))))
    (map (lambda (o)
	   (list (symbol-downcase (%%opcode-name o))
		 o))
	 opcodes)))

(define-primitive %%rebox (assembler "ReboxAssembler"))

(define (symbol->enum enumtype sym)
  (%%parseEnum enumtype sym #t))

(require (lib "1.scm" "srfi"))

(define-syntax combine-flags
  (syntax-rules ()
    ((_ enumname expr ...)
     (let ((type (lookup-type enumname)))
       (%%rebox "System.Int32" enumname
		(fold (lambda (a b) (fx+ a b)) 0
		      (map (lambda (x) (->int32 (symbol->enum type x)))
			   (list expr ...))))))))

(define (lookup-opcode name)
  (cond
   ((assq name *opcodes*) => cadr)
   (else
    (error "No opcode for name" name))))

(define (emit ilg op . maybe-arg)
  (let ((o (lookup-opcode op)))
    (if (null? maybe-arg)
	(%%emit ilg o)
	(%%emit ilg o (car maybe-arg)))))

(define <AssemblyBuilderAccess> (lookup-type "System.Reflection.Emit.AssemblyBuilderAccess"))
(define <MethodAttributes> (lookup-type "System.Reflection.MethodAttributes"))
(define <TypeAttributes> (lookup-type "System.Reflection.TypeAttributes"))

(define-primitive %%currentDomain (specific-static "System.AppDomain" "get_CurrentDomain"))
(define-primitive %%defineDynamicAssembly (generic-method "DefineDynamicAssembly"))
(define-primitive %%defineDynamicModule (generic-method "DefineDynamicModule"))
(define-primitive %%defineType (generic-method "DefineType"))
(define-primitive %%defineMethod
  (specific-method "System.Reflection.Emit.TypeBuilder" "DefineMethod"
		   symbol
		   "System.Reflection.MethodAttributes"
		   "System.Type"
		   "System.Type[]"))
(define-primitive %%getILGenerator (generic-method "GetILGenerator"))
(define-primitive %%createType (generic-method "CreateType"))
(define-primitive %%make-AssemblyName (specific-constructor "System.Reflection.AssemblyName"))
(define-primitive (%%aName %%aName!) (specific-property "System.Reflection.AssemblyName" "Name"))

(define (make-AssemblyName n)
  (let ((x (%%make-AssemblyName)))
    (%%aName! x (->symbol n))
    x))

(define ab (%%defineDynamicAssembly (%%currentDomain)
				    (make-AssemblyName "tmp-asm")
				    (symbol->enum <AssemblyBuilderAccess> 'run)))

(define modb (%%defineDynamicModule ab 'tmp-asm))

(define tb (%%defineType modb
			 'tmp-asm.tmp
			 (symbol->enum <TypeAttributes> 'public)
			 (lookup-type "System.Object")))

(define mb (%%defineMethod tb
			   'meth
			   (combine-flags "System.Reflection.MethodAttributes"
					  'public 'static 'hidebysig)
			   (lookup-type "System.Int32")
			   (specialise-vector
			    (lookup-type "System.Type")
			    (vector (lookup-type "System.Int32")))))

(define ilg (%%getILGenerator mb))

(emit ilg 'ldarg.0)
(emit ilg 'ldc.i4 1)
(emit ilg 'add)
(emit ilg 'ldc.i4.2)
(emit ilg 'mul)
(emit ilg 'ret)

(define tmp-type (%%createType tb))

(define-primitive %%getMethod (specific-method "System.Type" "GetMethod" symbol))
(define-primitive %%invoke (specific-method "System.Reflection.MethodBase" "Invoke" object vector))
(define (invoke-static-method meth . args)
  (%%invoke meth (%%load-void) (list->vector args)))

(define mmm (%%getMethod tmp-type 'meth))
(display (invoke-static-method mmm 123))
(newline)
