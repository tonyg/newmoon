;; Not quite R5RS yet. In particular, doesn't do floats or complex
;; numbers.
;;
;; It's quite inefficient - all those closures created on every call...

(define scheme-reader-dot-symbol (string->symbol "."))

;; (...objects -> char) (...objects -> char) boolean -> (...objects -> object)
(define (make-scheme-reader peek-char read-char case-sensitive)

  ;; Lexing. Per R5RS, *must* be case-insensitive for all alphabetical
  ;; characters.
  ;;

  (define (lex params)
    (define swallowed #f)

    (define (go eof-treatment state . args)
      (if swallowed
	  (apply read-char params))
      (let ((ch (apply peek-char params)))
	(set! swallowed #t)
	(if (eof-object? ch)
	    (if eof-treatment
		(apply state eof-treatment args)
		ch)
	    (apply state ch args))))

    (define (emit eating-character token-class token-value)
      (if eating-character
	  (apply read-char params))
      (cons token-class token-value))

    (define (start ch)
      (case ch
	((#\,)	(go #\space comma #f))
	((#\;)	(go #f comment))
	((#\")	(go #f lex-string '()))
	((#\-)	(go #\space sign #\- 10))
	((#\+)	(go #\space sign #\+ 10))
	((#\#)	(go #f hash))
	((#\()	(emit #t 'paren 'open-paren))
	((#\))	(emit #t 'paren 'close-paren))
	((#\')	(emit #t 'quote 'quote))
	((#\`)	(emit #t 'quasiquote 'quasiquote))
	(else
	 (cond
	  ((char-numeric? ch)		(num ch 0 10 1))
	  ((char-whitespace? ch)	(go #f start))
	  (else				(ident ch '() case-sensitive))))))

    (define (comma ch quasi)
      (if (char=? ch #\@)
	  (if quasi
	      (emit #t 'unsyntax-splicing 'unsyntax-splicing)
	      (emit #t 'unquote-splicing 'unquote-splicing))
	  (if quasi
	      (emit #f 'unsyntax 'unsyntax)
	      (emit #f 'unquote 'unquote))))

    (define (comment ch)
      (go #f (if (char=? ch #\newline)
		 start
		 comment)))

    (define (lex-string ch acc)
      (case ch
	((#\")	(emit #t 'literal (list->string (reverse acc))))
	((#\\)	(go #f escaped-char acc))
	(else	(go #f lex-string (cons ch acc)))))

    (define (escaped-char ch acc)
      (case ch
	((#\r)		(go #f lex-string (cons (integer->char 13) acc)))
	((#\n)		(go #f lex-string (cons (integer->char 10) acc)))
	((#\t)		(go #f lex-string (cons (integer->char  9) acc)))
	((#\b)		(go #f lex-string (cons (integer->char  8) acc)))
	((#\a)		(go #f lex-string (cons (integer->char  7) acc)))
	((#\" #\\)	(go #f lex-string (cons ch acc)))
	(else		(go #f lex-string acc))))

    (define (digit-for-radix ch radix)
      (let ((digit (cond
		    ((char-numeric? ch)		(- (char->integer ch)
						   (char->integer #\0)))
		    ((char-alphabetic? ch)	(- (char->integer (char-downcase ch))
						   (- (char->integer #\a) 10)))
		    (else #f))))
	(and digit
	     (cond ((negative? digit)	(error "scheme-reader: lexer: negative digit"
					       (list 'digit digit)))
		   ((>= digit radix)	(error "scheme-reader: lexer: digit out of range for radix"
					       (list 'digit digit)
					       (list 'radix radix)))
		   (else digit)))))

    (define (sign ch s radix)
      (if (digit-for-radix ch radix)
	  (num ch 0 radix (if (eqv? s #\-) -1 1))
	  (ident ch (list s) case-sensitive)))

    (define (num ch acc radix sign)
      (let ((digit (digit-for-radix ch radix)))
	(if digit
	    (go #\space num (+ (* acc radix) digit) radix sign)
	    (emit #f 'literal (* sign acc)))))

    (define (maybe-signed-num ch radix)
      (case ch
	((#\- #\+)	(go #f sign ch radix))
	(else		(num ch 0 radix 1))))

    (define (hash ch)
      (case (char-downcase ch)
	((#\b)	(go #f maybe-signed-num 2))
	((#\o)	(go #f maybe-signed-num 8))
	((#\d)	(go #f maybe-signed-num 10))
	((#\x)	(go #f maybe-signed-num 16))
	((#\t)	(emit #t 'literal #t))
	((#\f)	(emit #t 'literal #f))
	((#\%)	(ident ch '(#\#) #t))
	((#\')	(emit #t 'syntax 'syntax))
	((#\`)	(emit #t 'quasisyntax 'quasisyntax))
	((#\,)	(go #\space comma #t))
	((#\()	(emit #t 'paren 'open-vector-paren))
	((#\\)	(go #f named-character))
	(else	(error "Invalid hash-sign syntax" (list 'char ch)))))

    (define (named-character ch)
      (if (char-alphabetic? ch)
	  (let ((name (ident ch '() #t)))
	    (if (not (and (eq? (car name) 'literal)
			  (symbol? (cdr name))))
		(error "Named character name must be identifier" (list 'token name)))
	    (let* ((namesym (cdr name))
		   (namestr (symbol->string namesym)))
	      (case (string->symbol
		     (list->string
		      (map char-downcase
			   (string->list namestr))))
		((newline)	(emit #f 'literal #\newline))
		((space)	(emit #f 'literal #\space))
		(else		(emit #f 'literal (string-ref namestr 0))))))
	  (emit #t 'literal ch)))

    (define (ident ch acc case-sensitive)
      (if (not case-sensitive)
	  (set! ch (char-downcase ch)))
      (if (or (char-whitespace? ch)
	      (memv ch '(#\( #\) #\; #\")))
	  (emit #f 'literal (string->symbol (list->string (reverse acc))))
	  (go #\space ident (cons ch acc) case-sensitive)))

    (go #f start))

  ;; Parsing.
  ;;

  (define (reader . params)
    (define need-lex #t)
    (define cache '())

    (define (token)
      (if need-lex
	  (begin
	    (set! cache (lex params))
	    (set! need-lex #f)))
      cache)

    (define (drop)
      (token)
      (set! need-lex #t))

    (define (check class . value)
      (let ((t (token)))
	(and (eq? (car t) class)
	     (or (null? value)
		 (equal? (cdr t) (car value)))
	     t)))

    (define (parse-list org prev)
      (cond
       ((eof-object? (token))
	org)

       ((check 'paren 'close-paren)
	(drop)
	org)

       ((check 'literal scheme-reader-dot-symbol)
	(drop)
	(if (null? prev)
	    (error "scheme-reader: missing car in dotted pair"))
	(set-cdr! prev (parse))
	(if (check 'paren 'close-paren)
	    (drop)
	    (error "scheme-reader: double tail in dotted pair"))
	org)

       ((null? prev)
	(let ((l (list (parse))))
	  (parse-list l l)))

       (else
	(set-cdr! prev
		  (list (parse)))
	(parse-list org (cdr prev)))))

    (define (parse-special-prefix sym)
      (and (check sym sym)
	   (begin
	     (drop)
	     (list sym (parse)))))

    (define (parse)
      (cond
       ((eof-object? (token))
	(token))

       ((check 'paren 'open-paren)
	(drop)
	(parse-list '() '()))

       ((check 'paren 'close-paren)
	(drop)
	;; Skipping extra unmatched close-paren.
	(parse))

       ((parse-special-prefix 'quote))
       ((parse-special-prefix 'quasiquote))
       ((parse-special-prefix 'unquote))
       ((parse-special-prefix 'unquote-splicing))

       ((parse-special-prefix 'syntax))
       ((parse-special-prefix 'quasisyntax))
       ((parse-special-prefix 'unsyntax))
       ((parse-special-prefix 'unsyntax-splicing))

       ((check 'paren 'open-vector-paren)
	(drop)
	(list->vector (parse-list '() '())))

       ((check 'literal)
	(let ((v (cdr (token))))
	  (drop)
	  v))

       (else
	(error "scheme-reader: parse: Unknown token" (list 'token (token))))))

    (parse))

  reader)

(define (make-string-reader str case-sensitive)
  (define pos 0)
  (define len (string-length str))

  (define (peek-char)
    (if (= pos len)
	eof
	(string-ref str pos)))

  (define (read-char)
    (let ((c (peek-char)))
      (if (not (eof-object? c))
	  (set! pos (+ pos 1)))
      c))

  (make-scheme-reader peek-char read-char case-sensitive))

(define scheme-port-reader
  (make-scheme-reader peek-char read-char #f))
