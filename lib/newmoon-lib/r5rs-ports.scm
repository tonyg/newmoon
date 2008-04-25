(define (display x . p)
  (if (null? p)
      (%assemble (x) (x)
	(c "return(scheme_display("x", scheme_current_output_port));")
	(dotnet ($ x)
		(call "class [Newmoon]Newmoon.Primitive class [Newmoon]Newmoon.Primitive::Display(object)")))
      (%assemble (x p) (x (car p))
	(c "return(scheme_display("x", "p"));")
	(dotnet ($ x)
		($ p)
		(castclass "class [mscorlib]System.IO.TextWriter")
		(call "class [Newmoon]Newmoon.Primitive class [Newmoon]Newmoon.Primitive::Display(object, class [mscorlib]System.IO.TextWriter)")))))

; (define (write x . p)
;   (if (null? p)
;       (let-primitive ((w (specific-static "Newmoon.Primitive" "Write" object))) (w x))
;       (let-primitive ((w (specific-static "Newmoon.Primitive" "Write" object output-port)))
; 	(w x (car p)))))

(define (newline . p)
  (if (null? p)
      (%assemble () ()
	(c "return(scheme_newline(scheme_current_output_port));")
	(dotnet (call "class [mscorlib]System.Console class [mscorlib]System.Console::WriteLine()")))
      (%assemble (p) ((car p))
	(c "return(scheme_newline("p"));")
	(dotnet ($ p)
		(castclass "class [mscorlib]System.IO.TextWriter")
		(call "instance string class [mscorlib]System.IO.TextWriter::WriteLine()")))))

; (let-primitive ((? (type-predicate "Newmoon.EofObject")))
;   (define (eof-object? x) (? x)))
; (let-primitive ((*eof* (specific-static "Newmoon.EofObject" "GetEof")))
;   (define eof (*eof*)))

; (let-primitive ((%%get-current-input-port (specific-static "System.Console" "get_In"))
; 		(%%set-current-input-port!
; 		 (specific-static "System.Console" "SetIn" input-port)))
;   (define (current-input-port . p)
;     (if (pair? p)
; 	(%%set-current-input-port! (car p))
; 	(%%get-current-input-port))))

; (let-primitive ((%%get-current-output-port (specific-static "System.Console" "get_Out"))
; 		(%%set-current-output-port!
; 		 (specific-static "System.Console" "SetOut" output-port)))
;   (define (current-output-port . p)
;     (if (pair? p)
; 	(%%set-current-output-port! (car p))
; 	(%%get-current-output-port))))

; (let-primitive ((%%get-current-error-port (specific-static "System.Console" "get_Error"))
; 		(%%set-current-error-port!
; 		 (specific-static "System.Console" "SetError" output-port)))
;   (define (current-error-port . p)
;     (if (pair? p)
; 	(%%set-current-error-port! (car p))
; 	(%%get-current-error-port))))

; (define-primitive %%peek-char (specific-method "System.IO.TextReader" "Peek"))
; (define-primitive %%read-char (specific-method "System.IO.TextReader" "Read"))
; (define-primitive %%write-char (specific-method "System.IO.TextWriter" "Write" char))

; (define (%%check-eof i)
;   (if (= i -1)
;       eof
;       (integer->char i)))

; (define (peek-char . p)
;   (%%check-eof (if (pair? p)
; 		   (%%peek-char (car p))
; 		   (%%peek-char (current-input-port)))))

; (define (read-char . p)
;   (%%check-eof (if (pair? p)
; 		   (%%read-char (car p))
; 		   (%%read-char (current-input-port)))))

; (define (write-char ch . p)
;   (if (pair? p)
;       (%%write-char (car p) ch)
;       (%%write-char (current-output-port) ch)))

; (let-primitive ((%%read (specific-static "Newmoon.Reader" "Read" "System.IO.TextReader")))
;   (define (read . p)
;     (if (pair? p)
; 	(%%read (car p))
; 	(%%read (current-input-port)))))

; (let-primitive ((%%flush-output (specific-method "System.IO.TextWriter" "Flush")))
;   (define (flush-output . p)
;     (if (pair? p)
; 	(%%flush-output (car p))
; 	(%%flush-output (current-output-port)))))

; (let-primitive ((? (type-predicate input-port)))
;   (define (input-port? x) (? x)))

; (let-primitive ((? (type-predicate output-port)))
;   (define (output-port? x) (? x)))

; (define (char-ready? . p) #f) ;; %%%

; (let-primitive ((make-stream-reader (specific-constructor "System.IO.StreamReader" symbol)))
;   (define (open-input-file filename)
;     (make-stream-reader (->symbol filename))))

; (let-primitive ((make-stream-writer (specific-constructor "System.IO.StreamWriter" symbol)))
;   (define (open-output-file filename)
;     (make-stream-writer (->symbol filename))))

; (let-primitive ((close (specific-method input-port "Close")))
;   (define (close-input-port port)
;     (close port)))

; (let-primitive ((close (specific-method output-port "Close")))
;   (define (close-output-port port)
;     (close port)))

; (define (call-with-input-file string proc)
;   (let ((i (open-input-file string)))
;     (call-with-values (lambda () (proc i))
;       (lambda results
; 	(close-input-port i)
; 	(apply values results)))))

; (define (call-with-output-file string proc)
;   (let ((o (open-output-file string)))
;     (call-with-values (lambda () (proc o))
;       (lambda results
; 	(close-output-port o)
; 	(apply values results)))))

; (define (with-input-from-file string thunk)
;   (call-with-input-file string
;     (lambda (p)
;       (parameterize ((current-input-port p))
; 	(thunk)))))

; (define (with-output-to-file string thunk)
;   (call-with-output-file string
;     (lambda (p)
;       (parameterize ((current-output-port p))
; 	(thunk)))))
