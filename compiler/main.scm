;;;;; MAIN ENTRY POINT FOR SYSTEM

(define (compiler-gensym)
  (gensym "GENSYM"))

(define-syntax compiler-assert
  (syntax-rules ()
    ((_ excp-sym test-expr more ...)
     (if (not test-expr)
	 (error "internal-compiler-error" "assertion failed:" 'excp-sym 'test-expr more ...)))))

(define-syntax syntax-assert
  (syntax-rules ()
    ((_ excp-sym test-expr)
     (if (not test-expr)
	 (error "syntax-error" "this rule failed:" 'excp-sym 'test-expr)))))

;;; Load compiler modules

(include "tree.scm")

(include "toplevel.scm")
(include "parse.scm")
(include "cps.scm")
(include "optimize.scm")
(include "annotate.scm")

(include "driver.scm")

(define main$debug (make-parameter #f))
(define main$libpath (make-parameter (list "../lib")))
(define compiler$without-basic-libraries (make-parameter #f))

(define compile-file
  (let ((basic-visited #f))
    (lambda (filename)
      (display ";; compile-file compiling ") (display filename) (newline)
      (let ((exprs (call-with-input-file filename
		     (lambda (i)
		       (let loop ()
			 (let ((expr (read i)))
			   (if (eof-object? expr)
			       '()
			       (cons expr (loop))))))))
	    (output-filename (string-append filename ".sil")))
	(delete-file-if-exists output-filename)
	(call-with-output-file output-filename
	  (lambda (o)

	    (if (not basic-visited)
		(if (compiler$without-basic-libraries)
		    (sc-expand '(define-syntax sys$install-binding
				  (syntax-rules ()
				    ((_ name kind val)
				     (#%extern-apply 'sys$install-binding
						     '(assembler
						       ()
						       sys$install-binding
						       ("Newmoon.InstallBindingAssembler"))
						     name kind val)))))
		    (begin
		      (visit (resolve-library-path "basic-library.scm" '()))
		      (set! basic-visited #t))))

	    (parameterize ((compiler$visit-time '()))
	      (let ((sil (compiler-front-end-phases
			  `(begin
			     ,@exprs))))
		(parameterize ((print-vector-length #f)
			       (print-graph #t))
		  (write (list (reverse (compiler$visit-time)) sil)
			 o)
		  (display ";; compile-file finished  ") (display filename) (newline))))))))))
