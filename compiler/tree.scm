; (make-node-constructor 'classname '(field1 field2))
;
; Defines a constructor for the class classname, accepting
; fields named by constructor-fields.

(define (make-node-constructor name constructor-fields)
  (let ((num-constructor-fields (length constructor-fields)))
    (lambda args
      (if (not (= num-constructor-fields (length args)))
	  (error "internal-compiler-error" "wrong argc to node constructor; name =" name)
	  (cons name (map list constructor-fields args))))))

(define make-node (make-node-constructor 'node '()))

(define make-lit (make-node-constructor 'lit '(value)))
(define make-singleton (make-node-constructor 'singleton '(identifier)))
(define make-var (make-node-constructor 'var '(name global?)))
(define make-begin (make-node-constructor 'begin '(head tail)))
(define make-lambda (make-node-constructor 'lambda '(args body)))
(define make-apply (make-node-constructor 'apply '(rator rands)))
(define make-if (make-node-constructor 'if '(test true false)))
(define make-set (make-node-constructor 'set '(name value)))
(define make-extern-apply (make-node-constructor 'extern-apply '(name detail rands)))

(define (node-child-attr-names node)
  (case (node-kind node)
    ((lit singleton var) '())
    ((begin) '(head tail))
    ((lambda) '(body))
    ((apply) '(rator . rands))
    ((if) '(test true false))
    ((set) '(value))
    ((extern-apply) 'rands)
    (else
     (error "internal-compiler-error"
	    "unknown node kind in node-child-attr-names:" node))))

(define (node-collect-subnodes expr attrs)
  (let loop ((attrs attrs))
    (cond
     ((null? attrs) '())
     ((pair? attrs) (cons (node-get expr (node-kind expr) (car attrs))
			  (loop (cdr attrs))))
     (else
      (node-get expr (node-kind expr) attrs)))))

(define (node-tree-foreach fn node)
  (fn node)
  (for-each (lambda (child) (node-tree-foreach fn child))
	    (node-collect-subnodes node (node-child-attr-names node))))

(define (node-tree-map! fn node)
  (let loop ((attrs (node-child-attr-names node)))
    (cond
     ((null? attrs))
     ((pair? attrs)
      (let* ((name (car attrs))
	     (cell (assq name (cdr node)))
	     (oldval (cadr cell))
	     (newval (node-tree-map! fn oldval)))
	(if (not (eq? oldval newval))
	    (set-car! (cdr cell) newval))
	(loop (cdr attrs))))
     (else
      (let ((cell (assq attrs (cdr node))))
	(set-car! (cdr cell)
		  (map (lambda (v) (node-tree-map! fn v))
		       (cadr cell)))))))
  (fn node))

(define (node-kind node)
  (car node))

(define (node-kind? node k)
  (eq? (node-kind node) k))

(define (node-get node kind name)
  (if (eq? kind (car node))
      (let ((cell (assq name (cdr node))))
	(if cell
	    (cadr cell)
	    (error "internal-compiler-error" "unknown field name in node-get:" (car node) name node)))
      (error "internal-compiler-error" "node kind did not match in node-get:" (car node) kind node)))

(define (node-get-or-false node name)
  (let ((cell (assq name (cdr node))))
    (and cell
	 (cadr cell))))

(define (node-set! node kind name value)
  (if (eq? kind (car node))
      (let ((cell (assq name (cdr node))))
	(if cell
	    (set-car! (cdr cell) value)
	    (set-cdr! node (cons (list name value) (cdr node)))))
      (error "internal-compiler-error" "node kind did not match in node-set!:" (car node) kind node)))

(define-syntax node-match
  (let ()
    (lambda (x)
      (syntax-case x (else)
	((_ expr clause ...)
	 (not (boolean? (syntax-object->datum (syntax expr))))
	 (syntax (let* ((v expr)
			(k (node-kind v)))
		   (node-match #f v k clause ...))))

	((_ #f v k)
	 (syntax (error "node-match: no match for node-kind" k)))

	((_ #f v k (else expr ...))
	 (syntax (begin expr ...)))

	((_ #f v k ((kind var ...) body ...) clause ...)
	 (syntax (if (eq? k 'kind)
		     (node-match #t v kind (var ...) () body ...)
		     (node-match #f v k clause ...))))

	((_ #t v kind () (binding ...) body ...)
	 (syntax (let (binding ...)
		   body ...)))

	((_ #t v kind ((var name) vars ...) (binding ...) body ...)
	 (syntax (node-match #t v kind (vars ...) ((var (node-get v 'kind 'name)) binding ...)
			     body ...)))

	((_ #t v kind (var vars ...) (binding ...) body ...)
	 (syntax (node-match #t v kind (vars ...) ((var (node-get v 'kind 'var)) binding ...)
			     body ...)))))))
