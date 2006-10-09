; MzScheme support code.

(define (newmoon-basis) 'mzscheme)

(read-case-sensitive #t)
(print-struct #t)

(require-for-syntax (lib "1.ss" "srfi"))
(require (lib "1.ss" "srfi")
	 (lib "13.ss" "srfi")
	 (lib "9.ss" "srfi")
	 (lib "pretty.ss")
	 (lib "defmacro.ss")
	 (lib "process.ss")
	 (lib "etc.ss"))

(define-syntax include
  (lambda (x)
    (define read-file
      (lambda (fn k)
        (let ((p (open-input-file fn)))
          (let f ()
            (let ((x (read p)))
              (if (eof-object? x)
                  (begin (close-input-port p) '())
                  (cons (datum->syntax-object k x) (f))))))))
    (syntax-case x ()
      ((k filename)
       (let ((fn (syntax-object->datum (syntax filename))))
         (with-syntax (((exp ...) (read-file fn (syntax k))))
           (syntax (begin exp ...))))))))

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
