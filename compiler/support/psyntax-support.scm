(define andmap every)
(define ormap any)

(define compiler$visit-time (make-parameter '()))

(define sc-properties (make-hash-table))

(define (getprop symbol key)
  (cond
   ((assq key (hash-table-get sc-properties symbol (lambda () '())))
    => cdr)
   (else #f)))

(define (putprop symbol key value)
  (let ((l (hash-table-get sc-properties symbol (lambda () '()))))
    (cond
     ((assq key l) => (lambda (cell) (set-cdr! cell value)))
     (else
      (hash-table-put! sc-properties symbol (cons (cons key value) l))))))

(define (remprop symbol key)
  (call-with-current-continuation
   (lambda (return)
     (let ((l (hash-table-get sc-properties symbol (lambda () (return 'noprops)))))
       (let ((l2 (let loop ((l l))
		   (cond
		    ((null? l) '())
		    ((eq? (caar l) key) (cdr l))
		    (else (cons (car l) (loop (cdr l))))))))
	 (if (null? l2)
	     (hash-table-remove! sc-properties symbol)
	     (hash-table-put! sc-properties symbol l2)))))))

(define sc-expand 'forward)
(define $make-environment 'forward)
(define environment? 'forward)
(define literal-identifier=? 'forward)
(define syntax-error 'forward)
(define $syntax-dispatch 'forward)
