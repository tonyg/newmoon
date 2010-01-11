; MzScheme support code.
(require mzscheme)
(require scheme/base)

(define (newmoon-basis) 'mzscheme)

(read-case-sensitive #t)
(print-struct #t)

(require-for-syntax srfi/1)
(require srfi/1)
(require srfi/13)
(require srfi/9)
(require-for-syntax scheme/pretty)
(require mzlib/defmacro)
(require mzlib/process)
(require mzlib/etc)

(define error
  (let ((old-error error))
    (lambda (msg . vals)
      (display "---------------------------------------------------------------------------")
      (newline)
      (display msg)
      (newline)
      (for-each (compose pretty-print node->list) vals)
      (display "---------------------------------------------------------------------------")
      (newline)
      (old-error msg))))

(define (complete-path p)
  (let ((reldir (current-load-relative-directory)))
    (cond
     (reldir (path->complete-path p reldir))
     ((string? p) (string->path p))
     (else p))))

(define-syntax include
  (lambda (x)
    (define (complete-path p)
      (let ((reldir (current-load-relative-directory)))
	(cond
	 (reldir (path->complete-path p reldir))
	 ((string? p) (string->path p))
	 (else p))))
    (define read-file
      (lambda (fn k)
        (let ((p (open-input-file (complete-path fn))))
          (let f ()
            (let ((x (read p)))
              (if (eof-object? x)
                  (begin (close-input-port p) '())
                  (cons (datum->syntax k x) (f))))))))
    (syntax-case x ()
      ((k filename)
       (let ((fn (syntax->datum (syntax filename))))
         (with-syntax (((exp ...) (read-file fn (syntax k))))
           (syntax (begin exp ...))))))))

(define compiler$visit-time (make-parameter '())) ;; obsolete?? %%%

(define (resolve-library-path filename collection-path)
  (let* ((cpath (if (null? collection-path)
		    "newmoon-lib/"
		    (string-append (string-join collection-path "/") "/")))
	 (frag (string-append cpath filename)))
    (let loop ((p (map (lambda (p1) (path->string (complete-path p1))) (main$libpath))))
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
  (display (string-append ";; visiting "filename" ..."))
  (newline)
  (with-input-from-file filename
    (lambda ()
      (let loop ()
	(let ((expr (read)))
	  (if (eof-object? expr)
	      (begin
		(display (string-append ";; visited "filename"."))
		(newline))
	      (begin
		;; We call macro-expand for its side-effects on the symbol-table!
		(macro-expand expr)
		(loop))))))))

(define (delete-file-if-exists filename)
  (with-handlers
   ((exn:fail:filesystem? (lambda (exn) 'failed-ok)))
   (delete-file filename)))

(define (call-external-program program . args)
  (apply system* program args))

;;(load "tests/test-cases.scm")
