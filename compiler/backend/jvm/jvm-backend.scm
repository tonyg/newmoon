(load "bcel.scm")

(import s2j)
(import bcel)

(define (make-counter receiver)
  (let ((counter 0))
    (lambda ()
      (let ((c counter))
	(set! counter (+ counter 1))
	(receiver c)))))

(define (char->hex-string ch)
  (let* ((n (char->integer ch))
	 (s (number->string n 16))
	 (l (string-length s)))
    (if (< l 4)
	(string-append (make-string (- 4 l) #\0) s)
	s)))

(define (mangle-id id)
  (let ((str (if (symbol? id)
		 (symbol->string id)
		 id)))
    (list->string
     (fold-right (lambda (ch acc)
		   (cond
		    ((or (char-alphabetic? ch)
			 (char-numeric? ch))
		     (cons ch acc))
		    (else
		     (cons #\_
			   (append (string->list (char->hex-string ch))
				   (cons #\_
					 acc))))))
		 '()
		 (string->list str)))))

(define (make-local-table first-free-index)
  (let ((table (make-hash-table))
	(counter first-free-index))
    (lambda (name)
      (cond
       ((hash-table-get table name (lambda () #f)))
       (else
	(let ((index counter))
	  (set! counter (+ counter 1))
	  (hash-table-put! table name index)
	  index))))))

(define (relabel-single-chunk chunk)
  (car (bcel-alpha-convert (list chunk))))

(define (make-literal-table lambdaname cg)
  (let* ((table (make-hash-table 'equal))
	 (make-literal-field-name
	  (make-counter (lambda (c) (string-append "lit_" (number->string c)))))
	 (clinit-chunks '())
	 (add-chunk! (lambda (fieldname chunk)
		       (set! clinit-chunks (cons (cons fieldname chunk) clinit-chunks)))))
    (lambda (action . args)
      (case action
	((add) (apply (lambda (literal generator-thunk)
			(let ((field-name
			       (cond
				((hash-table-get table literal (lambda () #f)))
				(else
				 (let ((field-name (make-literal-field-name)))
				   (bcel-gen-field! cg '("ACC_PUBLIC" "ACC_STATIC" "ACC_FINAL")
						    "sisc.data.Value" field-name)
				   (hash-table-put! table literal field-name)
				   (add-chunk! field-name
					       (generator-thunk))
				   field-name)))))
			  `((getstatic ,lambdaname ,field-name "sisc.data.Value"))))
		      args))
	((finish)
	 (if (pair? clinit-chunks)
	     (bcel-gen-method! cg '("ACC_PUBLIC" "ACC_STATIC") 'void '() '() "<clinit>"
			       (concatenate
				(reverse
				 (cons `((return void))
				       (map (lambda (entry)
					      (let ((field-name (car entry))
						    (instructions (cdr entry)))
						`(,@instructions
						  (putstatic ,lambdaname ,field-name
							     "sisc.data.Value"))))
					    clinit-chunks))))
			       '())))
	(else (error "Unknown action to literal-table" (cons action args)))))))

(define (location->envt-fieldname loc)
  (string-append "envt_" (number->string loc)))

(define (arginfo-is-boxed? arginfo)
  (and (arginfo-captured? arginfo)
       (arginfo-mutated? arginfo)))

;; Quicksort due to Sebastian Egner <sebastian.egner@philips.com>.
;; From SRFI-42 examples.scm. Modified to use a comparison predicate
;; and to *not* use SRFI-42 (using SRFI-1 and SRFI-11 in its place) by
;; Tony Garnock-Jones <tonyg@kcbbs.gen.nz>.
;;
(define (quick-sort < xs) ; stable
  (let sorter ((xs xs))
    (if (null? xs)
	'()
	(let ((pivot (car xs))
	      (xrest (cdr xs)))
	  (let-values (((left-xs right-xs)
			(partition (lambda (elt) (< elt pivot)) xrest)))
	    (append (sorter left-xs)
		    (list pivot)
		    (sorter right-xs)))))))

(define (compiler-back-end-phases input-filename frontend-result)
  (let* ((module-name (mangle-id (replace-filename-extension input-filename "")))
	 (output-filename (replace-filename-extension input-filename ".class"))
	 (next-lambda-name (make-counter (lambda (c)
					   (string-append "sisc.newmoon.compiled."
							  module-name
							  ".lambda" (number->string c)))))
	 (all-generated-classes '())
	 (record-generated-class! (lambda (typename cg)
				    (set! all-generated-classes
					  (cons (cons typename cg) all-generated-classes)))))

    (define (load-argv) `(load (array "sisc.data.Value") 1))
    (define (load-trampoline) `(load "sisc.interpreter.Interpreter" 2))
    (define (load-empty-list) 
      `(getstatic "sisc.data.EmptyList" "EMPTYLIST" "sisc.data.EmptyList"))
    (define (load-undefined-value)
      `(getstatic "sisc.data.SchemeVoid" "VOID" "sisc.data.SchemeVoid"))

    (define (gen-closure parenttype node)
      (let* ((captures (or (node-get-or-false node 'captures) '()))
	     (all-arginfos (node-get node 'lambda 'all-arginfos))
	     (all-varargs (node-get node 'lambda 'all-varargs))
	     (all-bodies (node-get node 'lambda 'all-bodies))
	     (supertype "sisc.newmoon.CompiledProcedure")
	     (lambdaname (next-lambda-name))
	     (cg (bcel-gen-class lambdaname supertype input-filename
				 '("ACC_PUBLIC" "ACC_SUPER" "ACC_FINAL") '()))
	     (literal-table (make-literal-table lambdaname cg)))

	(define (load-this) `(load ,lambdaname 0))

	(define (compute-capture capture)
	  (let* ((arginfo (car capture))
		 (newloc (cadr capture))
		 (oldloc (caddr capture))
		 (fieldname (location->envt-fieldname newloc))
		 (is-boxed (arginfo-is-boxed? arginfo))
		 (is-rest (arginfo-is-rest? arginfo))
		 (fieldtype (if is-boxed "sisc.data.Box" "sisc.data.Value")))
	    (bcel-gen-field! cg '("ACC_PUBLIC") fieldtype fieldname)
	    (if (< oldloc 0)
		;; We capture an argument.
		`(,(load-this)
		  ,@(if is-rest
			`((load "sisc.data.Value" 3))
			`((load (array "sisc.data.Value") 2)
			  (const ,(- (- oldloc) 1))
			  (array-load "sisc.data.Value")))
		  ,@(if is-boxed `((check-cast ,fieldtype)) '())
		  (putfield ,lambdaname ,fieldname ,fieldtype))
		;; We capture a capture from our parent.
		(begin
		  (compiler-assert captured-environment-has-parenttype
				   parenttype
				   capture)
		  `(,(load-this)
		    (load ,parenttype 1)
		    (getfield ,parenttype ,(location->envt-fieldname oldloc) ,fieldtype)
		    (putfield ,lambdaname ,fieldname ,fieldtype))))))

	(define (compute-boxing-actions arginfos)
	  (do ((counter 0 (+ counter 1))
	       (rest arginfos (cdr rest))
	       (instructions '() (let* ((arginfo (car rest)))
				   (if (arginfo-is-boxed? arginfo)
				       (append (if (arginfo-is-rest? arginfo)
						   `((new "sisc.data.Box")
						     (dup 1)
						     (load "sisc.data.Value" 3)
						     (invoke "sisc.data.Box" "<init>"
							     void ("sisc.data.Value") special)
						     (store "sisc.data.Value" 3))
						   `(,(load-argv)
						     (const ,counter)
						     ;; argv index
						     (new "sisc.data.Box")
						     (dup 1)
						     ;; argv index box box
						     ,(load-argv)
						     (const ,counter)
						     (array-load "sisc.data.Value")
						     ;; argv index box box argval
						     (invoke "sisc.data.Box" "<init>"
							     void ("sisc.data.Value") special)
						     ;; argv index box
						     (array-store "sisc.data.Value")))
					       instructions)
				       instructions))))
	      ((null? rest) instructions)))

	(define (compute-varargs-lambda-body entry)
	  (let* ((arginfos (first entry))
		 (body-node (third entry))
		 (minargs (- (length arginfos) 1))
		 (too-few-label (gensym "VARARGSTOOFEW")))
	    `(;; Check the argv, throwing if it's not acceptable to us.
	      (dup 1)
	      (const ,minargs)
	      (int-cond < ,too-few-label)

	      ;; Loop over the restargs, building a list and
	      ;; putting it into the last slot in our argvec.
					;; argvlen
	      ,(load-argv)		;; argvlen argv
	      (swap)			;; argv argvlen
	      (const ,minargs)		;; argv argvlen minargs
	      (sub int)			;; argv optargcount
	      (const ,minargs)		;; argv optargcount minargs
	      (swap)			;; argv minargs optargcount
	      (invoke "sisc.util.Util" "valArrayToList"
		      "sisc.data.Pair" ((array "sisc.data.Value") int int) static)
					;; optarglist
	      (store "sisc.data.Value" 3)	;; <empty>
	      ,@(compute-boxing-actions arginfos)
	      ,@(gen-node lambdaname literal-table body-node)

	      ,too-few-label)))

	(if #f
	    (begin
	      (pretty-print `((lambdaname ,lambdaname)
			      (parenttype ,parenttype)
			      (captures ,captures)
			      (cases ,(map (lambda (arginfos varargs body)
					     `((arginfos ,arginfos)
					       (varargs ,varargs)
					       (body ,body)))
					   all-arginfos
					   all-varargs
					   all-bodies))))
	      (newline)))

	(compiler-assert root-lambda-no-captures (or parenttype
						     (null? captures)))

	(let ((assembly `((load ,lambdaname 0)
			  (invoke ,supertype "<init>" void () special)
			  ,@(concatenate (map compute-capture captures))
			  (return void))))
	  (if parenttype
	      (bcel-gen-method! cg '("ACC_PUBLIC") 'void
				`(,parenttype (array "sisc.data.Value") "sisc.data.Value")
				'("parent" "parent_argvec" "parent_restlist")
				"<init>" assembly '())
	      (bcel-gen-method! cg '("ACC_PUBLIC") 'void '() '() "<init>" assembly '())))

	(let ((local-table (make-local-table 4))) ;; should we need it in future
	  ;; Local 0 - this lambda (argument)
	  ;; Local 1 - sisc.data.Value[] argv (argument)
	  ;; Local 2 - sisc.interpreter.Interpreter r (argument)
	  ;; Local 3 - sisc.data.Value restlist (proper local)
	  (let-values (((varargs-cases fixed-cases)
			(partition second (zip all-arginfos all-varargs all-bodies))))
	    (let* ((fixed-table (map (lambda (entry)
				       (let ((arginfos (first entry))
					     (body (third entry)))
					 (list (length arginfos)
					       (gensym "CASELAMBDA")
					       `((null "sisc.data.Value")
						 (store "sisc.data.Value" 3)
						 ,@(compute-boxing-actions arginfos)
						 ,@(gen-node lambdaname literal-table body)))))
				     fixed-cases))
		   (min-fixed-argc (reduce min 0 (map first fixed-table)))
		   (max-fixed-argc (reduce max 0 (map first fixed-table)))
		   (argc-enumeration (map (lambda (i) (+ i min-fixed-argc))
					  (iota (+ (- max-fixed-argc min-fixed-argc) 1))))
		   (default-instr (gensym "CASELAMBDADEFAULT"))
		   (body `(,(load-argv)
			   (arraylength)
			   ,@(if (null? fixed-table)
				 '()
				 `((tableswitch ,default-instr
						,(map (lambda (i)
							(cond
							 ((assoc i fixed-table)
							  => (lambda (entry)
							       (list (first entry)
								     (second entry))))
							 (else (list i default-instr))))
						      argc-enumeration))
				   ,@(concatenate (map (lambda (entry)
							 `(,(second entry)
							   ,@(third entry)))
						       fixed-table))
				   ,default-instr
				   ,(load-argv)
				   (arraylength)))
			   ,@(concatenate (map compute-varargs-lambda-body
					       (quick-sort (lambda (a b)
							     (> (length (first a))
								(length (first b))))
							   varargs-cases)))
			   (pop 1)
			   (invoke "sisc.data.Procedure" "throwArgSizeException" void () static)
			   (return void))))
	      (bcel-gen-method! cg '("ACC_PUBLIC") 'void
				'((array "sisc.data.Value") "sisc.interpreter.Interpreter")
				'("argv" "trampoline")
				"applyCompiled"
				body
				'()))))

	(literal-table 'finish)
	(record-generated-class! lambdaname cg)
	lambdaname))

    (define (gen-literal value literal-table)
      (cond
       ((boolean? value)
	`((getstatic "sisc.data.SchemeBoolean" ,(if value "TRUE" "FALSE")
		     "sisc.data.SchemeBoolean")))
       ((pair? value)
	(literal-table 'add value
		       (lambda ()
			 `((new "sisc.data.ImmutablePair")
			   (dup 1)
			   ,@(gen-literal (car value) literal-table)
			   ,@(gen-literal (cdr value) literal-table)
			   (invoke "sisc.data.ImmutablePair" "<init>"
				   void ("sisc.data.Value" "sisc.data.Value") special)))))
       ((symbol? value)
	(literal-table 'add value
		       (lambda ()
			 `((const ,(symbol->string value))
			   (invoke "sisc.data.Symbol" "intern"
				   "sisc.data.Symbol" ("java.lang.String") static)))))
       ((number? value)
	(literal-table 'add value
		       (lambda ()
			 `((const ,(number->string value))
			   (invoke "sisc.newmoon.Util" "numberLiteral"
				   "sisc.data.Quantity" ("java.lang.String") static)))))
       ((char? value)
	(literal-table 'add value
		       (lambda ()
			 `((new "sisc.data.SchemeCharacter")
			   (dup 1)
			   (const ,(char->integer value))
			   (invoke "sisc.data.SchemeCharacter" "<init>"
				   void (char) special)))))
       ((string? value)
	(literal-table 'add value
		       (lambda ()
			 `((new "sisc.data.ImmutableString")
			   (dup 1)
			   (const ,value)
			   (invoke "sisc.data.ImmutableString" "<init>"
				   void ("java.lang.String") special)))))
       ((vector? value)
	(literal-table 'add value
		       (lambda ()
			 `((new "sisc.data.ImmutableVector")
			   (dup 1)
			   (const ,(vector-length value))
			   (newarray "sisc.data.Value" 1)
			   ,@(do ((i 0 (+ i 1))
				  (acc '() (cons `((dup 1)
						   (const ,i)
						   ,@(gen-literal (vector-ref value i)
								  literal-table)
						   (array-store "sisc.data.Value"))
						 acc)))
				 ((= i (vector-length value))
				  (concatenate (reverse acc))))
			   (invoke "sisc.data.ImmutableVector" "<init>"
				   void ((array "sisc.data.Value")) special)))))
       ((null? value)
	`(,(load-empty-list)))
       ((void? value)
	`(,(load-undefined-value)))
       ;; port? and procedure? not literals.
       (else
	(error "Invalid literal in jvm-backend:gen-literal" value))))

    (define (gen-var lambdaname literal-table node location name global?)
      ;; Globals: r.getCtx().toplevel_env.lookup(Symbol) -> Value
      (if global?
	  `(,(load-trampoline)
	    ,@(gen-literal name literal-table)
	    (invoke "sisc.newmoon.Util" "lookupGlobal"
		    "sisc.data.Value" ("sisc.interpreter.Interpreter"
				       "sisc.data.Value") static))
	  (let ((arginfo (node-get node 'var 'arginfo)))
	    (if (< location 0)
		;; Local.
		(let ((instrs (if (arginfo-is-rest? arginfo)
				  `((load "sisc.data.Value" 3))
				  `(,(load-argv)
				    (const ,(- (- location) 1))
				    (array-load "sisc.data.Value")))))
		  (if (arginfo-is-boxed? arginfo)
		      (append instrs
			      `((check-cast "sisc.data.Box")
				(getfield "sisc.data.Box" "val" "sisc.data.Value")))
		      instrs))
		;; From environment.
		(if (arginfo-is-boxed? arginfo)
		    `((load ,lambdaname 0)
		      (getfield ,lambdaname ,(location->envt-fieldname location)
				"sisc.data.Box")
		      (getfield "sisc.data.Box" "val" "sisc.data.Value"))
		    `((load ,lambdaname 0)
		      (getfield ,lambdaname ,(location->envt-fieldname location)
				"sisc.data.Value")))))))

    (define (gen-apply lambdaname literal-table rator rands)
      `(,(load-trampoline)
	(const ,(length rands))
	(newarray "sisc.data.Value" 1)
	,@(do ((i 0 (+ i 1))
	       (rands rands (cdr rands))
	       (acc '() (cons `((dup 1)
				(const ,i)
				,@(gen-node lambdaname literal-table (car rands))
				(array-store "sisc.data.Value"))
			      acc)))
	      ((null? rands)
	       (concatenate (reverse acc))))
	(putfield "sisc.interpreter.Interpreter" "vlr" (array "sisc.data.Value"))
	,(load-trampoline)
	,@(gen-node lambdaname literal-table rator)
	(putfield "sisc.interpreter.Interpreter" "compiledContinuation" "sisc.data.Value")
	(return void)))

    (define (gen-if lambdaname literal-table test true false)
      (let* ((true-code (gen-node lambdaname literal-table true))
	     (false-code (gen-node lambdaname literal-table false))
	     (need-goto (not (eq? (first (last true-code)) 'return))))
	(relabel-single-chunk
	 `(,@(gen-node lambdaname literal-table test)
	   (getstatic "sisc.data.SchemeBoolean" "FALSE" "sisc.data.SchemeBoolean")
	   (obj-cond == false-branch)
	   ,@true-code
	   ,@(if need-goto `((goto end-if)) '())
	   false-branch
	   ,@false-code
	   end-if))))

    (define (gen-set lambdaname literal-table node location name value)
      (if (string? location)
	  ;; Global.
	  `(,(load-trampoline)
	    ,@(gen-literal (string->symbol location) literal-table)
	    ,@(gen-node lambdaname literal-table value)
	    (invoke "sisc.newmoon.Util" "setGlobal"
		    "sisc.data.Value" ("sisc.interpreter.Interpreter"
				       "sisc.data.Value"
				       "sisc.data.Value") static))
	  ;; Non-global.
	  (let* ((arginfo (node-get node 'set 'arginfo))
		 (is-captured (arginfo-captured? arginfo))
		 (is-mutated (arginfo-mutated? arginfo))
		 (is-rest (arginfo-is-rest? arginfo)))
	    (compiler-assert every-set-arginfo-is-mutated is-mutated arginfo)
	    (append (if (< location 0)
			;; Local.
			(if is-captured
			    `(,@(if is-rest
				    `((load "sisc.data.Value" 3))
				    `(,(load-argv)
				      (const ,(- (- loc) 1))
				      (array-load "sisc.data.Value")))
			      (check-cast "sisc.data.Box") ;; want to get rid of these %%%
			      ,@(gen-node lambdaname literal-table value)
			      (putfield "sisc.data.Box" "val" "sisc.data.Value"))
			    (if is-rest
				`(,@(gen-node lambdaname literal-table value)
				  (store "sisc.data.Value" 3))
				`(,(load-argv)
				  (const ,(- (- loc) 1))
				  ,@(gen-node lambdaname literal-table value)
				  (array-store "sisc.data.Value"))))
			;; In environment.
			`((load ,lambdaname 0)
			  ,@(if is-captured
				`((getfield ,lambdaname ,(location->envt-fieldname location)
					    "sisc.data.Box")
				  ,@(gen-node lambdaname literal-table value)
				  (putfield "sisc.data.Box" "val" "sisc.data.Value"))
				`(,@(gen-node lambdaname literal-table value)
				  (putfield ,lambdaname ,(location->envt-fieldname location)
					    "sisc.data.Value")))))
		    `(,(load-undefined-value))))))

    (define (gen-jvm-assemble lambdaname literal-table formals actuals code)
      (let ((environ (map cons formals actuals)))
	(relabel-single-chunk
	 (fold-right (lambda (instr tail)
		       (if (and (pair? instr) (eq? (car instr) '$))
			   (let* ((refname (cadr instr))
				  (cell (assq refname environ)))
			     (if (not cell)
				 (error "Unknown refname in jvm-assemble" refname))
			     (append (gen-node lambdaname literal-table (cdr cell)) tail))
			   (cons instr tail)))
		     '()
		     code))))

    (define (gen-node lambdaname literal-table node)
      (node-match node
		  ((lit value) (gen-literal value literal-table))
		  ((singleton identifier)
		   (case identifier
		     ((undefined) `(,(load-undefined-value)))
		     (else
		      (error "Unsupported singleton identifier" identifier))))
		  ((var location name global?)
		   (gen-var lambdaname literal-table node location name global?))
		  ((begin head tail)
		   `(,@(gen-node lambdaname literal-table head)
		     (pop 1)
		     ,@(gen-node lambdaname literal-table tail)))
		  ((lambda)
		   (let ((closuretypename (gen-closure lambdaname node)))
		     `((new ,closuretypename)
		       (dup 1)
		       (load ,lambdaname 0)
		       ,(load-argv)
		       (load "sisc.data.Value" 3)
		       (invoke ,closuretypename "<init>" void
			       (,lambdaname
				(array "sisc.data.Value")
				"sisc.data.Value")
			       special))))
		  ((apply rator rands)
		   (gen-apply lambdaname literal-table rator rands))
		  ((if test true false) (gen-if lambdaname literal-table test true false))
		  ((set location name value)
		   (gen-set lambdaname literal-table node location name value))
		  ((jvm-assemble formals actuals code)
		   (gen-jvm-assemble lambdaname literal-table formals actuals code))
		  (else (error "Can't match node in jvm-backend.scm:gen-node"
			       (node-kind node)))))

    ;;(delete-file-if-exists output-filename)
    (let ((root-closure-typename (gen-closure #f frontend-result)))
      (for-each display (list "Root closure is "root-closure-typename".\n"))
      (if #t
	  (for-each (lambda (entry)
		      (let ((typename (car entry))
			    (cg (cdr entry)))
			(bcel-dump-to-file cg (string-append
					       "OUTPUT/"
					       (string-map (lambda (c)
							     (if (eqv? c #\.)
								 #\/
								 c))
							   typename)
					       ".class"))))
		    all-generated-classes))
      (if #t
	  (let* ((cgvec (->jarray (map cdr all-generated-classes)
				  (java-class '|org.apache.bcel.generic.ClassGen|)))
		 (class-loader (java-new (java-class '|sisc.newmoon.ClassLoader|)
					 cgvec))
		 (thunk (java-unwrap ((generic-java-method '|getRootLambda|) class-loader))))
	    ;;(display "Calling thunk.")
	    ;;(newline)
	    (thunk)
	    ;;(display "Called thunk.")
	    ;;(newline)
	    )))))
