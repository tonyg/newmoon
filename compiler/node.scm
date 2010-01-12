;; node.scm
;;
;; AST nodes

;;; Core

(define-record-type node-field-type
  (make-node-field-type* name validator)
  node-field-type?
  (name node-field-type-name)
  (validator node-field-type-validator*))

(define-record-type node-type
  (make-node-type* name fields)
  node-type?
  (name node-type-name)
  (fields node-type-fields))

(define-record-type node
  (make-node* type fields)
  node?
  (type node-type)
  (fields node-fields))

(define (node-field-type-validator ft)
  (force (node-field-type-validator* ft)))

(defmacro define-node-type (typename . fieldspecs)
  (define (mkspec spec)
    `(make-node-field-type* ',(car spec)
			    (delay ,(cadr spec))))
  (define (mkgetter spec)
    `(define ,(symbol-append (symbol-append typename '-) (car spec))
       (node-getter ,typename ',(car spec))))
  `(begin
     (define ,typename (make-node-type* ',typename
					(vector ,@(map mkspec fieldspecs))))
     ,@(map mkgetter fieldspecs)))

(defmacro define-language (langname . nodetypedefs)
  `(begin
     ,@(map (lambda (nodetypedef)
	      (if (symbol? nodetypedef)
		  `(begin)
		  `(define-node-type ,@nodetypedef)))
	    nodetypedefs)
     (define ,langname (%or ,@(map (lambda (nodetypedef)
				     (if (symbol? nodetypedef)
					 nodetypedef
					 (car nodetypedef)))
				   nodetypedefs)))))

(define (node-type-field-index type field-name)
  (let ((v (node-type-fields type)))
    (let loop ((i 0))
      (cond
       ((= i (vector-length v)) (error "Unknown field name" type field-name))
       ((eq? (node-field-type-name (vector-ref v i)) field-name) i)
       (else (loop (+ i 1)))))))

(define (make-node type . fields0)
  ;; TODO: construct and reuse singleton nullary nodes, now that nodes are immutable
  (when (not (node-type? type))
    (error "make-node not given node-type" type fields0))
  (let ((fields (list->vector fields0))
	(field-types (node-type-fields type)))
    (let ((field-count (vector-length fields)))
      (if (= field-count (vector-length field-types))
	  (do ((i 0 (+ i 1)))
	      ((= i field-count) (make-node* type fields))
	    (let ((field (vector-ref fields i))
		  (field-type (vector-ref field-types i)))
	      (when (not (apply-validator (node-field-type-validator field-type) field))
		(error "Validation failure" type field-type field))))
	  (error "Wrong number of fields" type fields)))))

(define (node-getter type name)
  (let ((i (node-type-field-index type name)))
    (lambda (node)
      (if (node-kind? node type)
	  (vector-ref (node-fields node) i)
	  (error "Wrong node-type in getter" type name node)))))

(define (node-get node type name)
  ((node-getter type name) node))

(define (node-kind? node type)
  (when (not (node? node))
    (error "Expected node in node-kind?" node type))
  (when (not (node-type? type))
    (error "Expected node-type in node-kind?" node type))
  (eq? (node-type node) type))

(define (node-fold child-attrs-of node-fn node)
  (let walk ((node node))
    (node-fn node
	     (lambda (node)
	       (let* ((t (node-type node))
		      (new-fields ;; Bletch, no vector-clone:
		       (list->vector (vector->list (node-fields node))))
		      (new-node (make-node* t new-fields)))
		 (let loop ((attrnames (child-attrs-of node)))
		   (cond
		    ((null? attrnames))
		    ((symbol? attrnames)
		     (let ((i (node-type-field-index t attrnames)))
		       (vector-set! new-fields i (map walk (vector-ref new-fields i)))))
		    (else
		     (let ((i (node-type-field-index t (car attrnames))))
		       (vector-set! new-fields i (walk (vector-ref new-fields i)))
		       (loop (cdr attrnames))))))
		 new-node)))))

(define (node->list x)
  (cond
   ((pair? x) (cons (node->list (car x)) (node->list (cdr x))))
   ((vector? x) (list->vector (map node->list (vector->list x))))
   ((node? x) (let ((t (node-type x)))
		`(,(node-type-name t) ,@(map (lambda (d v)
					       `(,(node-field-type-name d) ,(node->list v)))
					     (vector->list (node-type-fields t))
					     (vector->list (node-fields x))))))
   (else x)))

;;; Validators

(define (apply-validator validator value)
  (if (node-type? validator)
      ((%node validator) value)
      (validator value)))

(define %any
  (lambda (value)
    #t))

(define (%or . validators)
  (lambda (value)
    (let loop ((validators validators))
      (if (null? validators)
	  #f
	  (or (apply-validator (car validators) value)
	      (loop (cdr validators)))))))

(define (%list-of validator)
  (define (loop value)
    (or (null? value)
	(and (pair? value)
	     (apply-validator validator (car value))
	     (loop (cdr value)))))
  loop)

(define %set-of %list-of) ;; ick.

(define (%box-of validator)
  (lambda (value)
    (and (box? value)
	 (apply-validator validator (unbox value)))))

(define (%node type)
  (lambda (value)
    (and (node? value)
	 (node-kind? value type)
	 ;; no need to check fields, done at node construction time
	 )))

(define (true? x) x)
(define (false? x) (not x))

(define (%optional validator)
  (%or false? validator))

(defmacro node-match (expr . clauses)
  (let ((v (compiler-gensym))
	(ty (compiler-gensym)))
    (define (mkclauses clauses)
      (if (null? clauses)
	  `(error "No match in node-match" ,ty ,v)
	  (let ((clause (car clauses)))
	    (if (eq? (car clause) 'else)
		`(begin ,@(cdr (car clauses)))
		(let* ((pat (car clause))
		       (type-name (car pat))
		       (body-exps (cdr clause)))
		  (define (getter-for name)
		    (symbol-append (symbol-append type-name '-) name))
		  (define (mkbinding binding)
		    (if (symbol? binding)
			`(,binding (,(getter-for binding) ,v))
			`(,(car binding) (,(getter-for (cadr binding)) ,v))))
		  `(if (eq? ,ty ,type-name)
		       (let ,(map mkbinding (cdr pat))
			 ,@body-exps)
		       ,(mkclauses (cdr clauses))))))))
    `(let* ((,v ,expr)
	    (,ty (node-type ,v)))
       ,(mkclauses clauses))))

;;; Local Variables:
;;; eval: (put 'node-match 'scheme-indent-function 1)
;;; End:
