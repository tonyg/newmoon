; Newmoon compiler support code.

(define (newmoon-basis) 'newmoon)

(require (lib "1.scm" "srfi")
	 (lib "13.scm" "srfi"))

(define ($sc-put-cte . x)
  (write (list "SC-PUT" x))(newline))

(define (make-gensym-session-key)
  (let-primitive ((guid (specific-static "System.Guid" "NewGuid")))
    (string-append "sessionkey-"
		   (symbol->string (->symbol (guid))))))

(include "support/psyntax-support.scm")
(letrec ((oldeval eval)
	 (eval (lambda (x)
		 (display ";;; SC-EVALING ")
		 (write x)
		 (newline)
		 (compiler$visit-time (cons x (compiler$visit-time)))
		 (let ((y (oldeval x)))
		   (display ";;; done")
		   (newline)
		   (newline)
		   y))))
  (include "support/psyntax.pp"))

(define visit
  (let ((eval-noexpand (lambda (exp) (eval (list "noexpand" exp)))))
    (lambda (filename)
      (let ((fc (string-append filename ".sil")))
	(if (not (file-exists? fc))
	    (compile-file filename))
	(with-input-from-file fc
	  (lambda ()
	    (let* ((detail (read))
		   (visit-time (car detail))
		   (parse-tree (cadr detail)))
	      (for-each eval-noexpand visit-time))))))))

(define (delete-file-if-exists filename)
  (if (file-exists? filename)
      (let-primitive ((del (specific-static "System.IO.File" "Delete" symbol)))
	(del (->symbol filename)))))

(define print-vector-length (make-parameter #f))
(define print-graph (make-parameter #t))
