(require (lib "9.scm" "srfi"))

(%backend c "#include <errno.h>")

(define (display x . p)
  ;; TODO
  (%assemble (x) (x)
    (c "scheme_display(stdout, "x"); fflush(NULL); return(mkvoid());")
    (scheme (display x))))

(define (newline . p)
  ;; TODO
  (%assemble () ()
    (c "printf(\"\\n\"); fflush(NULL); return(mkvoid());")
    (scheme (newline))))

(define-record-type <port-buffer>
  (make-port-buffer* bin write-pos read-pos)
  port-buffer?
  (bin port-buffer-bin)
  (write-pos port-buffer-write-pos set-port-buffer-write-pos!)
  (read-pos port-buffer-read-pos set-port-buffer-read-pos!))

(define-record-type <input-port>
  (make-input-port* fd buf)
  input-port?
  (fd input-port-fd)
  (buf input-port-buffer set-input-port-buffer!))

(define-record-type <output-port>
  (make-output-port* fd buf)
  output-port?
  (fd output-port-fd)
  (buf output-port-buffer set-output-port-buffer!))

(define default-port-buffer-length (make-parameter 16384))

(define (make-binary len)
  (%assemble (len) (len)
    (c "defbinaryset(binoop, DETAG("len"), 0);"
       "return(binoop);")))

(define (make-port-buffer size)
  (make-port-buffer* (make-binary size) 0 0))

(define (make-default-port-buffer)
  (make-port-buffer (default-port-buffer-length)))

(define current-input-port (make-parameter (make-input-port* 0 (make-default-port-buffer))))
(define current-output-port (make-parameter (make-output-port* 1 (make-default-port-buffer))))
(define current-error-port (make-parameter (make-output-port* 2 (make-default-port-buffer))))

(define eof (%assemble () () (c "return(mkeof());")))

(define (eof-object? x)
  (%assemble (x) (x)
    (c "return(scheme_boolean(isspecial("x") && DETAG("x") == SPECIAL_EOF));")))

(define (binary? bin)
  (%assemble (bin) (bin)
    (c "return(scheme_boolean(isbinary("bin")));")))

(define (binary-length bin)
  (if (not (binary? bin))
      (error 'binary-length "Not a binary" bin)
      (%assemble (bin) (bin)
	(c "return(litint(oop_len("bin")));"))))

(define (binary-copy! target target-offset source source-offset count)
  (cond
   ((not (binary? source)) (error 'binary-copy! "Source not a binary" source))
   ((not (binary? target)) (error 'binary-copy! "Target not a binary" target))
   ((< source-offset 0) (error 'binary-copy! "Source offset negative" source-offset))
   ((< target-offset 0) (error 'binary-copy! "Target offset negative" target-offset))
   ((> (+ source-offset count) (binary-length source))
    (error 'binary-copy! "Count would overrun source" source-offset count))
   ((> (+ target-offset count) (binary-length target))
    (error 'binary-copy! "Count would overrun target" target-offset count))
   (else
    (%assemble (source target s-o t-o count) (source target source-offset target-offset count)
      (c "memcpy(((binary *) "target")->data + DETAG("t-o"),"
	 "       ((binary *) "source")->data + DETAG("s-o"),"
	 "       DETAG("count"));"
	 "return(mkvoid());")))))

(define (subbytes bin start count)
  (cond
   ((not (binary? bin)) (error 'subbytes "Not a binary" bin))
   ((< start 0) (error 'subbytes "Start negative" start))
   ((> (+ start count) (binary-length bin))
    (error 'subbytes "Count would overrun binary" start count))
   (else (let ((tmp (make-binary count)))
	   (binary-copy! tmp 0 bin start count)
	   tmp))))

(define (binary-ref bin index)
  (cond
   ((not (binary? bin)) (error 'binary-ref "Not a binary" bin))
   ((< index 0) (error 'binary-ref "Index negative" index))
   ((>= index (binary-length bin)) (error 'binary-ref "Index would overrun binary" index))
   (else
    (%assemble (bin index) (bin index)
      (c "return(litint((intptr_t) (((binary *) "bin")->data[DETAG("index")])));")))))

(define (binary-set! bin index val)
  (cond
   ((not (binary? bin)) (error 'binary-set! "Not a binary" bin))
   ((not (fixnum? val)) (error 'binary-set! "Not an integer" val))
   ((< index 0) (error 'binary-set! "Index negative" index))
   ((>= index (binary-length bin)) (error 'binary-set! "Index would overrun binary" index))
   (else
    (%assemble (bin index val) (bin index val)
      (c "((binary *) "bin")->data[DETAG("index")] = DETAG("val");"
	 "return(mkvoid());")))))

(define (read-into-binary bin fd start count)
  (cond
   ((not (binary? bin))
    (error 'read-into-binary "Not a binary" bin))
   ((< start 0)
    (error 'read-into-binary "Negative start position" start))
   ((> (+ start count) (binary-length bin))
    (error 'read-into-binary "Read would overrun buffer" start count))
   (else
    (let ((result
	   (%assemble (bin fd start count) (bin fd start count)
	     (c "intptr_t read_result = "
		"  read(DETAG("fd"), ((binary *) "bin")->data + DETAG("start"), DETAG("count"));"
		"if (read_result == 0) { return(mkeof()); }"
		"if (read_result < 0) { return(litint((intptr_t) -errno)); }"
		"return(litint(read_result));"))))
      (if (and (fixnum? result)
	       (negative? result))
	  (raise-posix-error (- result))
	  result)))))

(define (write-from-binary bin fd start count)
  (cond
   ((not (binary? bin))
    (error 'write-from-binary "Not a binary" bin))
   ((< start 0)
    (error 'write-from-binary "Negative start position" start))
   ((> (+ start count) (binary-length bin))
    (error 'write-from-binary "Read would overrun buffer" start count))
   (else
    (let ((result
	   (%assemble (bin fd start count) (bin fd start count)
	     (c "intptr_t write_result = "
		"  write(DETAG("fd"), ((binary *) "bin")->data + DETAG("start"), DETAG("count"));"
		"if (write_result < 0) { return(litint((intptr_t) -errno)); }"
		"return(litint(write_result));"))))
      (if (and (fixnum? result)
	       (negative? result))
	  (raise-posix-error (- result))
	  result)))))

(define (%%get-port maybe-port parameter)
  (if (null? maybe-port)
      (parameter)
      (car maybe-port)))

(define (port-buffer-available buf)
  (- (port-buffer-write-pos buf)
     (port-buffer-read-pos buf)))

(define (port-buffer-remaining buf)
  (- (binary-length (port-buffer-bin buf))
     (port-buffer-write-pos buf)))

(define (ensure-port-buffer-nonempty! p)
  (let* ((buf (input-port-buffer p))
	 (wp (port-buffer-write-pos buf))
	 (rp (port-buffer-read-pos buf)))
    (if (> wp rp)
	#t
	(let* ((fd (input-port-fd p))
	       (bin (port-buffer-bin buf))
	       (result (read-into-binary bin fd wp (- (binary-length bin) wp))))
	  (if (eof-object? result)
	      #f
	      (begin (set-port-buffer-write-pos! buf (+ wp result))
		     #t))))))

(define (advance-port-buffer-read-pos! buf new-position)
  (if (= new-position (port-buffer-write-pos buf))
      (begin (set-port-buffer-read-pos! buf 0)
	     (set-port-buffer-write-pos! buf 0))
      (set-port-buffer-read-pos! buf new-position)))

(define (advance-port-buffer-write-pos! buf new-position)
  (if (> new-position (binary-length (port-buffer-bin buf)))
      (error 'advance-port-buffer-write-pos! "Buffer overrun")
      (set-port-buffer-write-pos! buf new-position)))

(define (port-buffer-consume! buf count)
  (let ((old-rp (port-buffer-read-pos buf))
	(wp (port-buffer-write-pos buf))
	(new-rp (+ old-rp count)))
    (if (> new-rp wp)
	(begin (advance-port-buffer-read-pos! buf wp)
	       (- wp old-rp))
	(begin (advance-port-buffer-read-pos! buf new-rp)
	       count))))

(define (read-byte* p)
  (let ((buf (input-port-buffer p)))
    (if (ensure-port-buffer-nonempty! p)
	(let ((b (binary-ref (port-buffer-bin buf) (port-buffer-read-pos buf))))
	  (port-buffer-consume! buf 1)
	  b)
	eof)))

(define (read-byte . maybe-port)
  (read-byte* (%%get-port maybe-port current-input-port)))

(define (read-char . maybe-port)
  (let* ((p (%%get-port maybe-port current-input-port))
	 (b (read-byte* p)))
    (if (eof-object? b)
	b
	;; TODO: UTF-8
	(integer->char b))))

(define (read-bytes* count p)
  (let ((tmp (make-binary count))
	(buf (input-port-buffer p)))
    (let loop ((offset 0))
      (if (< offset count)
	  (if (ensure-port-buffer-nonempty! p)
	      (let ((chunk-size (min (- count offset)
				     (port-buffer-available buf)))
		    (rp (port-buffer-read-pos buf)))
		(binary-copy! tmp offset (port-buffer-bin buf) rp chunk-size)
		(advance-port-buffer-read-pos! buf (+ rp chunk-size))
		(loop (+ offset chunk-size)))
	      (if (= offset 0)
		  eof
		  (subbytes tmp 0 offset)))
	  tmp))))

(define (read-bytes count . maybe-port)
  (read-bytes* count (%%get-port maybe-port current-input-port)))

(define (flush-output* p)
  (let* ((buf (output-port-buffer p))
	 (rp (port-buffer-read-pos buf))
	 (wp (port-buffer-write-pos buf)))
    (if (> wp rp)
	(begin
	  (write-from-binary (port-buffer-bin buf) (output-port-fd p) rp wp)
	  (advance-port-buffer-read-pos! buf wp)
	  (- wp rp))
	0)))

(define (flush-output . maybe-port)
  (flush-output* (%%get-port maybe-port current-output-port)))

(define (ensure-port-buffer-nonfull! p)
  (let* ((buf (output-port-buffer p)))
    (if (= (port-buffer-remaining buf) 0)
	(flush-output* p))))

(define (write-byte* b p)
  (ensure-port-buffer-nonfull! p)
  (let* ((buf (output-port-buffer p))
	 (wp (port-buffer-write-pos buf)))
    (binary-set! (port-buffer-bin buf) wp b)
    (advance-port-buffer-write-pos! buf (+ wp 1))))

(define (write-byte b . maybe-port)
  (write-byte* b (%%get-port maybe-port current-output-port)))

(define (write-char c . maybe-port)
  ;; TODO: UTF-8
  (write-byte* (char->integer c) (%%get-port maybe-port current-output-port)))

(define (write-bytes* bin-to-write p start-pos end-pos)
  (let* ((total-count (- end-pos start-pos))
	 (buf (output-port-buffer p))
	 (bin (port-buffer-bin buf)))
    (let loop ((start-pos start-pos))
      (let ((count (- end-pos start-pos)))
	(if (> count 0)
	    (begin (ensure-port-buffer-nonfull! p)
		   (let ((wp (port-buffer-write-pos buf))
			 (chunk-size (min count (port-buffer-remaining buf))))
		     (binary-copy! bin wp bin-to-write start-pos chunk-size)
		     (advance-port-buffer-write-pos! buf (+ wp chunk-size))
		     (loop (+ start-pos chunk-size))))
	    total-count)))))

(define (write-bytes bin . maybe-port-etc)
  (let* ((p (%%get-port maybe-port-etc current-output-port))
	 (has-start-pos (and (pair? maybe-port-etc)
			     (pair? (cdr maybe-port-etc))))
	 (start-pos (if has-start-pos (cadr maybe-port-etc) 0))
	 (end-pos (if (and has-start-pos
			   (pair? (cddr maybe-port-etc)))
		      (caddr maybe-port-etc)
		      (binary-length bin))))
    (write-bytes* bin p start-pos end-pos)))

(define (write-string str . maybe-port-etc)
  ;; TODO: UTF-8 etc.
  (apply write-bytes str maybe-port-etc))

; (define (write x . p)
;   (if (null? p)
;       (let-primitive ((w (specific-static "Newmoon.Primitive" "Write" object))) (w x))
;       (let-primitive ((w (specific-static "Newmoon.Primitive" "Write" object output-port)))
; 	(w x (car p)))))

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
