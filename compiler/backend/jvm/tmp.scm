(newmoon-main "-b" "../../h2.scm")

(define (run-loop-i)
  (define repeats 1000000)
  (do ((i repeats (- i 1)))
      ((= i 0))
    'nothing))

(define (run-loop-i1)
  (define repeats 1000000)
  (do ((i repeats (compiled-binary-- i 1)))
      ((= i 0))
    'nothing))

(define (time1-proc howmany . entries)
  (define repeats 1000000)
  (let ((results (map (lambda (entry)
			(let ((thunk (cdr entry))
			      (name (car entry)))
			  (do ((counter 0 (+ counter 1))
			       (total-ms 0 (+ total-ms (let ((t (begin
								  (for-each display
									    (list name
										  " iteration "
										  (+ counter 1)
										  "..."
										  #\newline))
								  (time (thunk)))))
							 (caadr t)))))
			      ((= counter howmany)
			       (list name (exact->inexact (/ (* repeats counter)
							     (/ total-ms 1000))))))))
		      entries)))
    (for-each (lambda (result)
		(let ((name (car result))
		      (persecond (cadr result)))
		  (for-each display (list "Test "name" runs "persecond"/second"#\newline))))
	      results)
    results))

(define-syntax time1
  (syntax-rules ()
    ((_ howmany name ...)
     (time1-proc howmany (cons 'name name) ...))))

(define (format-relative wrt results)
  (let* ((baseline-datum (assq wrt results))
	 (normalized (map (lambda (result)
			    (list (car result)
				  (/ (log (/ (cadr baseline-datum)
					     (cadr result)))
				     (log 2))))
			  results)))
    (for-each (lambda (result)
		(let* ((name (car result))
		       (rellog (cadr result))
		       (message (if (negative? rellog)
				    (string-append (number->string (expt 2 (- rellog)))
						   "x faster")
				    (string-append (number->string (expt 2 rellog))
						   "x slower"))))
		(for-each display (list "** " name #\tab
					" delta " rellog #\tab
					message #\newline))))
	      normalized)))

(define (speed-test howmany)
  (let* ((results (time1 howmany
			 run-loop-i
			 run-loop-i1
			 run-loop
			 run-loop1
			 run-loop1a
			 run-loop1b
			 run-loop1c
			 run-loop1d
			 run-loop2))
	 (wrt 'run-loop-i))
    (format-relative wrt results)
    results))
