;; node.scm
;;
;; Defines AST nodes, a macro for binding variables from a node, and a
;; language-checker for dynamic type checking.

(define-record-type node
  (make-node* kind fields)
  node?
  (kind node-kind)
  (fields node-fields set-node-fields!))

(define (make-node kind . fields0)
  (make-node* kind
	      (let loop ((fields fields0))
		(cond
		 ((null? fields) '())
		 ((null? (cdr fields)) (error "Odd number of arguments to make-node"
					      (list kind fields0)))
		 (else (cons (list (car fields) (cadr fields)) (loop (cddr fields))))))))

(define (node-kind? node k)
  (eq? (node-kind node) k))

(define (node-get node kind name)
  (if (eq? kind (node-kind node))
      (let ((cell (assq name (node-fields node))))
	(if cell
	    (cadr cell)
	    (error "Unknown field name in node-get:" (list (node-kind node) name node))))
      (error "Node kind did not match in node-get:" (list (node-kind node) kind node))))

(define (node-getter kind name)
  (lambda (node) (node-get node kind name)))

(define (node-get/default! node name deft)
  (let ((cell (assq name (node-fields node))))
    (if cell
	(cadr cell)
	(begin
	  (set-node-fields! node (cons (list name deft) (node-fields node)))
	  deft))))

(define (node-set! node kind name value)
  (if (eq? kind (node-kind node))
      (let ((cell (assq name (node-fields node))))
	(if cell
	    (set-car! (cdr cell) value)
	    (set-node-fields! node (cons (list name value) (node-fields node)))))
      (error "Node kind did not match in node-set!:" (list (node-kind node) kind node))))

(define (node-push! node kind name value)
  (node-set! node kind name (cons value (node-get node kind name))))

(define (node->list node)
  (cond
   ((node? node) (cons (node-kind node) (node->list (node-fields node))))
   ((pair? node) (cons (node->list (car node)) (node->list (cdr node))))
   (else node)))

(define (lookup-language-token token language)
  (cond
   ((assq token language) => cadr)
   (else (error "Missing language token" (list token language)))))

(define (type-error node type)
  (error "Language error" (list node type)))

(define (check-language node start-token language error-handler)
  (let validate ((node node)
		 (type start-token))
    (cond
     ((procedure? type) (type node))
     ((symbol? type) (or (validate node (lookup-language-token type language))
			 (and error-handler (error-handler node type))))
     ((pair? type)
      (case (car type)
	((%or) (let loop ((types (cdr type)))
		 (if (null? types)
		     #f
		     (or (validate node (car types))
			 (loop (cdr types))))))
	((%list-of) (and (list? node)
			 (every (lambda (n) (validate n (cadr type))) node)))
	(else
	 (and (node? node)
	      (eq? (car type) (node-kind node))
	      (let loop ((type-fields (cdr type)))
		(or (null? type-fields)
		    (let* ((expected-field-name (caar type-fields))
			   (expected-field-type (cadar type-fields))
			   (field-cell (assq expected-field-name (node-fields node))))
		      (and field-cell
			   (validate (cadr field-cell) expected-field-type)
			   (loop (cdr type-fields))))))))))
     ((eq? type #t) #t)
     ((eq? type #f) #f)
     (else (error "Illegal language" (list type language))))))

(define (node-collect-subnodes expr attrs)
  (let loop ((attrs attrs))
    (cond
     ((null? attrs) '())
     ((pair? attrs) (cons (node-get expr (node-kind expr) (car attrs))
			  (loop (cdr attrs))))
     (else
      (node-get expr (node-kind expr) attrs)))))

(define (node-tree-foreach child-attrs-of fn node)
  (fn node)
  (for-each (lambda (child) (node-tree-foreach fn child))
	    (node-collect-subnodes node (child-attrs-of node))))

(define (node-children-map! child-attrs-of fn node)
  (let loop ((attrs (child-attrs-of node)))
    (cond
     ((null? attrs) node)
     ((pair? attrs)
      (let* ((name (car attrs))
	     (cell (assq name (node-fields node)))
	     (oldval (cadr cell))
	     (newval (fn oldval)))
	(if (not (eq? oldval newval))
	    (set-car! (cdr cell) newval))
	(loop (cdr attrs))))
     (else
      (let ((cell (assq attrs (node-fields node))))
	(set-car! (cdr cell) (map fn (cadr cell))))))))

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
