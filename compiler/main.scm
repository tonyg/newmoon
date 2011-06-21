;;;;; MAIN ENTRY POINT FOR SYSTEM

(begin-for-syntax
 (define (compiler-gensym)
   (gensym "GENSYM"))
 (define (symbol-append a b)
   (string->symbol (string-append (symbol->string a) (symbol->string b)))))

(defmacro compiler-assert (excp-sym test-expr . more)
  `(when (not ,test-expr)
     (error "internal-compiler-error" "assertion failed:" ',excp-sym ',test-expr ,@more)))

(defmacro syntax-assert (excp-sym test-expr)
  `(when (not ,test-expr)
     (error "syntax-error" "this rule failed:" ',excp-sym ',test-expr)))

;;; Load compiler modules

(include "node.scm")
(include "absyn.scm")

(include "macro.scm")
(include "toplevel.scm")
(include "intdef.scm")
(include "cps.scm")
(include "optimize.scm")
(include "annotate.scm")
(include "coreeval.scm")

(include "driver.scm")

(define main$debug (make-parameter #f))
(define main$libpath (make-parameter (list "../lib")))
(define compiler$without-basic-libraries (make-parameter #f))
(define compiler$make-program (make-parameter #f))
(define compiler$target-namespace (make-parameter #f))
(define compiler$extra-backend-args (make-parameter '()))

(define (debug-mode=? mode)
  ;; Really, main$debug should be a set of symbols, to allow more than
  ;; one mode at once.
  (and (main$debug)
       (string? (main$debug))
       (string=? (main$debug) (symbol->string mode))))

(define (replace-filename-extension filename new-extension)
  (cond
   ((string-index-right filename #\.) =>
    (lambda (pos)
      (string-append (substring filename 0 pos) new-extension)))
   (else
    (string-append filename new-extension))))

(define (read-all-sexps port)
  (let loop ()
    (let ((expr (read port)))
      (if (eof-object? expr)
	  '()
	  (cons expr (loop))))))

(define compile-file
  (let ((basic-visited #f))
    (lambda (filename)
      (display ";; compile-file compiling ") (display filename) (newline)
      (let ((exprs (call-with-input-file filename read-all-sexps)))
	(when (not basic-visited)
	  (when (not (compiler$without-basic-libraries))
	    (visit (resolve-library-path "basic-library.scm" '()))
	    (set! basic-visited #t)))
	(parameterize ((compiler$visit-time '()))
	  (compiler-back-end-phases filename
				    (compiler-front-end-phases `(begin ,@exprs)))
	  (display ";; compile-file finished  ") (display filename) (newline))))))

(define (visit-libraries libspecs)
  (for-each (lambda (libspec)
	      (case (car libspec)
		((lib) (visit (resolve-library-path (cadr libspec) (cddr libspec))))
		(else (error "Bad libspec" libspec))))
	    libspecs))

(define (compile-expr expr)
  ;; Not finished/working yet
  (parameterize ((compiler$make-program #f))
    (compiler-back-end-phases "IMMEDIATEcompileexpr"
			      (compiler-front-end-phases expr))))
