;(require (lib "test.ss" "schemeunit")
;	 (lib "text-ui.ss" "schemeunit"))

(load "reader.scm")

(define (run-reader-tests)
  (define (rt0 expected str cs)
    (assert-equal? expected ((make-string-reader str cs))))

  (define (rt expected str)
    (rt0 expected str #t)
    (rt0 expected str #f))

  (define (rtl expected str)
    (define (collect p)
      (let ((x (p)))
	(if (eof-object? x)
	    '()
	    (cons x (collect p)))))
    (assert-equal? expected (collect (make-string-reader str #f))))

  (define (string-downcase s)
    (list->string
     (map char-downcase
	  (string->list s))))

  (test/text-ui
   (make-test-suite "reader tests"

		    (make-test-case
		     "simple numbers"
		     (for-each (lambda (s)
				 (rt (string->number s) s))
			       '("0" "10" "945" "-1" "-101")))

		    (make-test-case
		     "booleans"
		     (for-each (lambda (p)
				 (rt (car p) (cdr p)))
			       '((#t . "#t") (#t . "#T")
				 (#f . "#f") (#f . "#F"))))

		    (make-test-case
		     "characters"
		     (rt #\  "#\\ ")
		     (rt #\x "#\\x")
		     (rt #\3 "#\\3"))

		    (make-test-case
		     "case-sensitive symbol reading"
		     (for-each (lambda (s)
				 (rt0 (string->symbol s) s #t)
				 (rt0 (string->symbol (string-downcase s)) s #f))
			       '("abc" "DEF" "aBc" "DeF" "a9" "A9" "a.a" "-" "...")))

		    (make-test-case
		     "lists"
		     (rt0 '(1 -2 - + ++ -- 3 abc "ab\"cd")
			  "(1 -2 - + ++ -- 3 abc \"ab\\\"cd\""
			  #f)
		     (rt0 '(#\a  #t #t #\A  #\newline  (c) (a . d) () #(1 2) #())
			  "(#\\a #T #t #\\A #\\newline (c) (a . d) () #(1 2) #())"
			  #f))

		    (make-test-case
		     "fancy quoting"
		     (rt0 '('(a) `(b c ,d ,@(f g)))
			  "('(a) `(b c ,d ,@(f g)))"
			  #f))

		    (make-test-case
		     "comments and whitespace"
		     (rt0 '(a b)
			  "(a ; this is a comment\n;so is this\n b;)\n)"
			  #f))

		    (make-test-case
		     "hex numbers"
		     (rt #xa "#XA")
		     (rt #x-b "#x-b"))

		    (make-test-case
		     "syntax reader extension"
		     (rt '(syntax 123)
			 "#'123")
		     (rt0 '(syntax (let f () body ... (f)))
			  "#'(let f () body ... (f))"
			  #f))

		    (make-test-case
		     "magic symbol syntax extension"
		     (rt (string->symbol "#%heLLO")
			 "#%heLLO"))

		    )))

;(run-reader-tests)

(define (rpl)
  (display "scheme-port-reader> ")
  (flush-output)
  (let ((r (scheme-port-reader)))
    (if (eof-object? r)
	(display ";; Bye\n")
	(begin
	  (display ";; Read: ")
	  (write r)
	  (newline)
	  (rpl)))))

; (require (lib "pretty.ss"))
; (with-input-from-file "reader.scm"
;   (lambda ()
;     (let loop ((exprs '()))
;       (let ((x (scheme-port-reader)))
; 	(if (eof-object? x)
; 	    (pretty-print (reverse exprs))
; 	    (loop (cons x exprs)))))))

(rpl)
(exit)
