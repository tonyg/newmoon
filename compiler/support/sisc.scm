; SISC support code.

(define (newmoon-basis) 'sisc)

(with-output-to-file "/dev/null"
  (lambda ()
    (require-library "sisc/libs/srfi")))

(import srfi-1)
(import srfi-11)
(import srfi-13)
(import file-manipulation)
(import hashtable)

;;-- Provide compatible hash-table interface over SISC's hashtables.
(define (make-hash-table . opt)
  (make-hashtable (cond
		   ((null? opt) eq?)
		   ((eq? (car opt) 'equal) equal?)
		   ((eq? (car opt) 'weak) (error "Weak hashtables unsupported"))
		   (else (error "Unknown optional argument(s) to make-hash-table" opt)))))

(define (hash-table-put! ht key value)
  (hashtable/put! ht key value))

(define hash-table-get
  (let ((*default* "*default*"))
    (lambda (ht key default-thunk)
      (let ((v (hashtable/get ht key *default*)))
	(if (eq? v *default*)
	    (default-thunk)
	    v)))))

(define (hash-table-for-each ht f)
  (hashtable/for-each f ht))
;;--

(define (make-gensym-session-key)
  (symbol->string (gensym))) ;; as explained in the SISC manual, this is quite random enough.

(define error
  (let ((error error)
	(format format))
    (lambda args
      (error (format "ERROR: ~s" args)))))

(define gensym
  (let ((g gensym))
    (lambda x ;; ignore arguments
      (g))))

(define compiler$visit-time (make-parameter '()))

(set! eval
  (let ((eval eval))
    (lambda (x)
      (let ((x (if (and (pair? x)
			(string? (car x))
			(string=? (car x) "noexpand"))
		   (cadr x)
		   x)))
	(compiler$visit-time (cons x (compiler$visit-time)))
	(eval x)))))

;; This next line is a bit bizarre. It turns out that if it's absent,
;; the existing binding for compile-file (from SISC's libraries) is
;; used below, *even though* we redefine the toplevel binding later
;; on! So in the one case, compile-file is defined somewhere else, we
;; override it later on, and the earlier binding is used; and in the
;; other, compile-file is defined somewhere else, we override it
;; *here*, and then *again* later on, and this time the *final*
;; binding is used! Colour me confused.
(define compile-file #f)

;; MzScheme-compatible aliases.
(define print-graph print-shared)
(define print-vector-length vector-length-prefixing)

(define (resolve-library-path filename collection-path)
  (let* ((cpath (if (null? collection-path)
		    "newmoon-lib/"
		    (string-append (string-join collection-path "/") "/")))
	 (frag (string-append cpath filename)))
    (let loop ((p (main$libpath)))
      (if (null? p)
	  (error "Could not resolve-library-path" filename collection-path)
	  (let* ((path (car p))
		 (rest (cdr p))
		 (f (string-append path "/" frag)))
	    (display (string-append ";; resolve-library-path trying "f" ..."))
	    (newline)
	    (if (file-exists? f)
		f
		(loop rest)))))))

(define (visit filename)
  (let ((fc (replace-filename-extension filename ".class")))
    (for-each display (list ";; checking "fc"..."#\newline))
    (if (not (file-exists? fc))
	(compile-file filename))
    (with-input-from-file fc
      (lambda ()
	(let* ((detail (read))
	       (visit-time (car detail))
	       (parse-tree (cadr detail)))
	  (parameterize ((compiler$visit-time '()))
	    (for-each eval visit-time)))))))

(define (delete-file-if-exists filename)
  (file-delete! filename))

(define (%extern-apply name detail . args)
  (for-each display
	    (list "WARNING:: extern-apply called in wrong phase: "name" "detail" "args))
  (newline)
  ;; We support a few extern-applys for the sake of macro-expansion
  ;; during bootstrapping.
  (case name
    ((boolean?) (apply boolean? args))
    (else
     (error "extern-apply called in wrong phase" args))))
