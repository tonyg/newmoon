"true";exec env CLASSPATH=bcel-5.1.jar sisc "$0" "$@"

(require-library "sisc/libs/srfi")

(module bcel (bcel-gen-class
	      bcel-type-for
	      bcel-alpha-convert
	      bcel-gen-field!
	      bcel-gen-method!
	      bcel-dump-to-file)

  (import hashtable)
  (import logicops)

  (import srfi-1)
  (import srfi-9)
  (import srfi-13)

  (import s2j)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; I prefer the MzScheme bit-operation names.
  (define bitwise-and logand)
  (define bitwise-ior logor)
  (define bitwise-xor logxor)
  (define bitwise-not lognot)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define-java-classes
    (<jstring> |java.lang.String|)
    (<Integer> |java.lang.Integer|)
    (<Double> |java.lang.Double|)

    (<Constants> |org.apache.bcel.Constants|)
    (<InstructionFactory> |org.apache.bcel.generic.InstructionFactory|)
    (<InstructionList> |org.apache.bcel.generic.InstructionList|)
    (<InstructionHandle> |org.apache.bcel.generic.InstructionHandle|)
    (<ConstantPoolGen> |org.apache.bcel.generic.ConstantPoolGen|)
    (<ClassGen> |org.apache.bcel.generic.ClassGen|)
    (<FieldGen> |org.apache.bcel.generic.FieldGen|)
    (<MethodGen> |org.apache.bcel.generic.MethodGen|)
    (<Type> |org.apache.bcel.generic.Type|)
    (<ObjectType> |org.apache.bcel.generic.ObjectType|)
    (<ArrayType> |org.apache.bcel.generic.ArrayType|))

  (define-generic-java-methods
    get-class-name
    get-constant-pool
    get-java-class
    add-field
    add-method
    dump

    get-field

    add-exception-handler
    get-method
    set-max-stack
    set-max-locals

    set-target

    dispose)

  (define (with-exception->false thunk)
    (with/fc
	(lambda (m e) #f)
      thunk))

  (define (memoize-function arg-transformer comparison body)
    (let ((table (make-hashtable comparison))
	  (notfound "*notfound*"))
      (lambda (arg)
	(let ((arg (if arg-transformer (arg-transformer arg) arg)))
	  (hashtable/get! table arg (lambda () (body arg)))))))

  (define (->sym x)
    (if (symbol? x)
	x
	(string->symbol x)))

  (define bcel-gen-class #f)
  (define bcel-type-for #f)
  (define bcel-alpha-convert #f)
  (define bcel-gen-field! #f)
  (define bcel-gen-method! #f)

  (define (bcel-dump-to-file classgen filename)
    (dump (get-java-class classgen) (->jstring filename)))

  (let () ;; here because the module system needs the module body to use internal-style defines!

  (define bclass
    (memoize-function ->sym eq?
     (lambda (name)
       (let ((name (symbol->string name)))
	 (call-with-current-continuation
	  (lambda (return)
	    (for-each (lambda (prefix) (with-exception->false
					(lambda ()
					  (return (java-class
						   (string->symbol (string-append
								    prefix "." name)))))))
		      '("org.apache.bcel"
			"org.apache.bcel.classfile"
			"org.apache.bcel.generic"
			"org.apache.bcel.util"
			"org.apache.bcel.verifier"
			"org.apache.bcel.verifier.exc"
			"org.apache.bcel.verifier.statics"
			"org.apache.bcel.verifier.structurals"))
	    (error "No such bclass" name)))))))

  (define bconst
    (memoize-function ->sym eq?
     (lambda (name)
       (->number ((generic-java-field-accessor name) (java-null <Constants>))))))

  (define (bflagset flags)
    (fold logor 0 (map bconst flags)))

  (define (gen-class full-name base-class file-name flags interface-names)
    (java-new <ClassGen>
	      (->jstring full-name)
	      (->jstring base-class)
	      (->jstring file-name)
	      (->jint (bflagset flags))
	      (->jarray (map ->jstring interface-names) <jstring>)))

  (define type-for
    (memoize-function #f equal?
     (lambda (typedesc)
       (cond
	((and (pair? typedesc) (eq? (car typedesc) 'array))
	 (java-new <ArrayType> (type-for (cadr typedesc)) (->jint (if (null? (cddr typedesc))
								      1
								      (caddr typedesc)))))
	((eq? typedesc 'object) (java-new <ObjectType> (->jstring "java.lang.Object")))
	((symbol? typedesc)
	 ((generic-java-field-accessor (string->symbol
					(string-upcase
					 (symbol->string typedesc))))
	  (java-null <Type>)))
	((string? typedesc)
	 (java-new <ObjectType> (->jstring typedesc)))
	(else
	 (error "Unknown typedesc in type-for" typedesc))))))

  (define (alpha-convert chunks)
    (let* ((handle-table (make-hashtable eq?))
	   (lookup (lambda (s) (hashtable/get! handle-table s gensym))))
      (map (lambda (chunk)
	     (map (lambda (instr)
		    (if (symbol? instr)
			(lookup instr)
			(case (car instr)
			  ((int-cond obj-cond) `(,(car instr)
						 ,(cadr instr)
						 ,(lookup (caddr instr))))
			  ((goto jsr) `(,(car instr)
					,(lookup (cadr instr))))
			  (else instr))))
		  chunk))
	   chunks)))

  (define (gen-field! classgen flags type name)
    (let ((field (java-new <FieldGen>
			   (->jint (bflagset flags))
			   (type-for type)
			   (->jstring name)
			   (get-constant-pool classgen))))
      (add-field classgen (get-field field))))

  (define (gen-method! classgen flags rettype argtypes argnames methname assembly exceptions)
    (let* ((il (java-new <InstructionList>))
	   (method (java-new <MethodGen>
			     (->jint (bflagset flags))
			     (type-for rettype)
			     (->jarray (map type-for argtypes) <Type>)
			     (->jarray (map ->jstring argnames) <jstring>)
			     (->jstring methname)
			     (get-class-name classgen)
			     il
			     (get-constant-pool classgen)))
	   (handle-table (make-hashtable eq?)))
      (assemble-code! classgen il handle-table assembly)
      (assemble-exceptions! method handle-table exceptions)
      (set-max-stack method)
      (set-max-locals method)
      (add-method classgen (get-method method))
      (dispose il)))

  (define (assemble-code! classgen il handle-table assembly)
    ;;(pretty-print assembly)(newline)
    (let* ((factory (java-new <InstructionFactory> (get-constant-pool classgen)))
	   (fixups '())
	   (push-fixup! (lambda (fixup) (set! fixups (cons fixup fixups))))
	   (label-fixup (lambda (op label)
			  (lambda (lookup)
			    (set-target op (lookup label)))))
	   (table-fixup (lambda (op cases default)
			  (lambda (lookup)
			    (do ((i 0 (+ i 1))
				 (cases cases (cdr cases)))
				((null? cases))
			      (let* ((entry (car cases))
				     (target (second entry)))
				(set-target op (->jint i) (lookup target))))
			    (set-target op (lookup default))))))
      (define-generic-java-methods
	create-append
	create-array-load
	create-array-store
	create-binary-operation	;; string op, Type type
	create-branch-instruction ;; short opcode, InstructionHandle target
	create-cast		  ;; Type src, Type dest
	create-check-cast
	create-constant	;; Object value
	(create-dup-1 |createDup_1|)
	(create-dup-2 |createDup_2|)
	create-dup
	create-field-access ;; String classname, String name, Type type, short kind
	create-get-field    ;; classname, name, type
	create-get-static   ;; classname, name, type
	create-instance-of
	create-invoke ;; classname, name, rettype, argtypearray, kind
	create-load   ;; type, index
	create-new    ;; type or string
	create-new-array ;; type, dim
	create-null	 ;; type
	create-pop
	create-put-field ;; classname, name, type
	create-put-static ;; classname, name, type
	create-return	  ;; type
	create-store	  ;; type, index
	(ilappend* append)
	)

      (let walk ((assembly assembly))
	(if (null? assembly)
	    #f
	    (let ((instr (car assembly))
		  (rest (cdr assembly)))
	      (if (symbol? instr)
		  (let ((handle (walk rest)))
		    (if handle (hashtable/put! handle-table instr handle))
		    handle)
		  (let* ((opcode (car instr))
			 (args (cdr instr))
			 (a1 (lambda () (if (null? args)
					    (error "Missing first argument" instr)
					    (car args))))
			 (a2 (lambda () (if (< (length args) 2)
					    (error "Missing second argument" instr)
					    (cadr args))))
			 (aN (lambda (n) (if (< (length args) n)
					     (error "Missing nth argument" (cons n instr))
					     (list-ref args (- n 1)))))
			 (! (lambda (creator . args)
			      (let ((handle (ilappend* il (apply creator factory args))))
				(walk rest)
				handle))))
		    (case opcode
		      ((nop) (or (walk rest)
				 (! (lambda _ (java-new (bclass "NOP"))))))
		      ((null) (! create-null (type-for (a1))))
		      ((const) (! create-constant (cond
						   ((number? (a1))
						    (if (exact? (a1))
							(java-new <Integer> (->jint (a1)))
							(java-new <Double> (->jdouble (a1)))))
						   ((string? (a1)) (->jstring (a1)))
						   (else (error "Unsupported constant" instr)))))
		      ((load) (! create-load (type-for (a1)) (->jint (a2))))
		      ((array-load) (! create-array-load (type-for (a1))))
		      ((store) (! create-store (type-for (a1)) (->jint (a2))))
		      ((array-store) (! create-array-store (type-for (a1))))
		      ((pop) (! create-pop (->jint (a1))))
		      ((dup) (! create-dup (->jint (a1))))
		      ((dup1) (! create-dup-1 (->jint (a1))))
		      ((dup2) (! create-dup-2 (->jint (a1))))
		      ((swap) (! (lambda _ (java-new (bclass "SWAP")))))
		      ((add) (! create-binary-operation (->jstring "+") (type-for (a1))))
		      ((sub) (! create-binary-operation (->jstring "-") (type-for (a1))))
		      ((mul) (! create-binary-operation (->jstring "*") (type-for (a1))))
		      ((div) (! create-binary-operation (->jstring "/") (type-for (a1))))
		      ((rem) (! create-binary-operation (->jstring "%") (type-for (a1))))
		      ((neg) (! (lambda _
				  (java-new (bclass (case (a1)
						      ((int) "INEG")
						      ((long) "LNEG")
						      ((float) "FNEG")
						      ((double) "DNEG")
						      (else (error "Unsupported type in neg"
								   instr))))))))
		      ((shl) (! create-binary-operation (->jstring "<<") (type-for (a1))))
		      ((shr) (! create-binary-operation (->jstring ">>") (type-for (a1))))
		      ((ushr) (! create-binary-operation (->jstring ">>>") (type-for (a1))))
		      ((and) (! create-binary-operation (->jstring "&") (type-for (a1))))
		      ((or) (! create-binary-operation (->jstring "|") (type-for (a1))))
		      ((or) (! create-binary-operation (->jstring "^") (type-for (a1))))
		      ((inc) (! (lambda _
				  (java-new (bclass "IINC")
					    (->jint (a1))
					    (->jint (a2))))))
		      ((cast) (! create-cast (type-for (a1)) (type-for (a2))))
		      ((check-cast) (! create-check-cast (type-for (a1))))
		      ((instanceof) (! create-instance-of (type-for (a1))))
		      ((lcmp) (! (lambda _ (java-new (bclass "LCMP")))))
		      ((fcmp) (! (lambda _ (java-new (bclass (case (a1)
							       ((<) "FCMPL")
							       ((>) "FCMPG")
							       (else (error "Unknown order"
									    instr))))))))
		      ((dcmp) (! (lambda _ (java-new (bclass (case (a1)
							       ((<) "DCMPL")
							       ((>) "DCMPG")
							       (else (error "Unknown order"
									    instr))))))))
		      ((int-cond) (let ((op (create-branch-instruction
					     factory
					     (->jshort
					      (bconst (case (a1)
							((eq0 ==0) "IFEQ")
							((ne0 !=0) "IFNE")
							((lt0 <0) "IFLT")
							((ge0 >=0) "IFGE")
							((gt0 >0) "IFGT")
							((le0 <=0) "IFLE")
							((eq ==) "IF_ICMPEQ")
							((ne !=) "IF_ICMPNE")
							((lt <) "IF_ICMPLT")
							((ge >=) "IF_ICMPGE")
							((gt >) "IF_ICMPGT")
							((le <=) "IF_ICMPLE")
							(else (error "Unknown comparison"
								     instr)))))
					     (java-null <InstructionHandle>))))
				    (push-fixup! (label-fixup op (a2)))
				    (! (lambda _ op))))
		      ((obj-cond) (let ((op (create-branch-instruction
					     factory
					     (->jshort
					      (bconst (case (a1)
							((eq ==) "IF_ACMPEQ")
							((ne !=) "IF_ACMPNE")
							((null) "IF_NULL")
							((nonnull) "IF_NONNULL")
							(else (error "Unknown comparison"
								     instr)))))
					     (java-null <InstructionHandle>))))
				    (push-fixup! (label-fixup op (a2)))
				    (! (lambda _ op))))
		      ((goto) (let ((op (java-new (bclass "GOTO_W")
						  (java-null <InstructionHandle>))))
				(push-fixup! (label-fixup op (a1)))
				(! (lambda _ op))))
		      ((jsr) (let ((op (java-new (bclass "JSR_W")
						 (java-null <InstructionHandle>))))
			       (push-fixup! (label-fixup op (a1)))
			       (! (lambda _ op))))
		      ((ret) (! (lambda _ (java-new (bclass "RET") (->jint (a1))))))
		      ((return) (! create-return (type-for (a1))))
		      ((getstatic) (! create-get-static
				      (->jstring (a1))
				      (->jstring (a2))
				      (type-for (aN 3))))
		      ((putstatic) (! create-put-static
				      (->jstring (a1))
				      (->jstring (a2))
				      (type-for (aN 3))))
		      ((getfield) (! create-get-field
				     (->jstring (a1))
				     (->jstring (a2))
				     (type-for (aN 3))))
		      ((putfield) (! create-put-field
				     (->jstring (a1))
				     (->jstring (a2))
				     (type-for (aN 3))))
		      ((invoke) (! create-invoke
				   (->jstring (a1)) ;; classname
				   (->jstring (a2)) ;; methodname
				   (type-for (aN 3)) ;; rettype
				   (->jarray (map type-for (aN 4)) <Type>) ;; argtypes
				   (->jshort
				    (bconst (case (aN 5) ;; kind
					      ((interface) "INVOKEINTERFACE")
					      ((static) "INVOKESTATIC")
					      ((virtual) "INVOKEVIRTUAL")
					      ((special) "INVOKESPECIAL")
					      (else (error "Unknown invoke kind" instr)))))))
		      ((new) (! create-new (type-for (a1))))
		      ((newarray) (! create-new-array (type-for (a1)) (->jshort (a2))))
		      ((arraylength) (! (lambda _ (java-new (bclass "ARRAYLENGTH")))))
		      ((throw) (! (lambda _ (java-new (bclass "ATHROW")))))
		      ((monitorenter) (! (lambda _ (java-new (bclass "MONITORENTER")))))
		      ((monitorexit) (! (lambda _ (java-new (bclass "MONITOREXIT")))))
		      ((tableswitch) (let* ((default (a1))
					    (cases (a2))
					    (ncases (length cases))
					    (matches (->jarray
						      (map ->jint (map first cases))
						      <jint>))
					    (null-handle (java-null <InstructionHandle>))
					    (handles (->jarray
						      (make-vector ncases null-handle)
						      <InstructionHandle>))
					    (op (java-new (bclass "TABLESWITCH")
							  matches
							  handles
							  null-handle)))
				       (push-fixup! (table-fixup op cases default))
				       (! (lambda _ op))))
		      (else (error "Unknown opcode instruction" instr))))))))

      (for-each (let ((lookup (lambda (label) (or (hashtable/get handle-table label)
						  (error "Unknown label" label)))))
		  (lambda (fixup)
		    (fixup lookup)))
		fixups)))

  (define (assemble-exceptions! method handle-table exceptions)
    (define (lookup-label label)
      (or (hashtable/get handle-table label)
	  (error "Unknown label" label)))
    (for-each (lambda (exception)
		(add-exception-handler method
				       (lookup-label (car exceptions))
				       (lookup-label (cadr exceptions))
				       (lookup-label (caddr exceptions))
				       (type-for (caddr exceptions))))
	      exceptions))

  (set! bcel-gen-class gen-class)
  (set! bcel-type-for type-for)
  (set! bcel-alpha-convert alpha-convert)
  (set! bcel-gen-field! gen-field!)
  (set! bcel-gen-method! gen-method!)
))