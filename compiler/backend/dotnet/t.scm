(define-primitive System.Console.Write (generic-static "System.Console" "Write"))

(let-primitive ((writeline (generic-static "System.Console" "WriteLine")))
  ;; Really need case-lambda!
  (define (System.Console.WriteLine . x)
    (if (null? x)
	(writeline)
	(writeline (car x)))))

(define-primitive ->string (specific-method object "ToString"))

(define (print . args)
  (let loop ((args args))
    (if (null? args)
	(System.Console.WriteLine)
	(begin
	  (System.Console.Write (car args))
	  (loop (cdr args))))))

;---------------------------------------------------------------------------
(define ht (make-hash-table))
(print "HT is: "ht)
(hash-table-put! ht 'a "Hello")
(hash-table-put! ht 'b "World")
(define (print-ht-contents n)
  (print "("n") Getting a: "(hash-table-get ht 'a (lambda () #f)))
  (print "("n") Getting b: "(hash-table-get ht 'b (lambda () #f)))
  (print "("n") Getting c: "(hash-table-get ht 'c (lambda () #f))))
(print-ht-contents 1)
(hash-table-remove! ht 'b)
(print-ht-contents 2)
;---------------------------------------------------------------------------

(define (caar x)
  (car (car x)))

(define (cadr x)
  (car (cdr x)))

(define (list . x) x)

(define (assq key alst)
  (cond
   ((null? alst) #f)
   ((eq? (caar alst) key) (car alst))
   (else (assq key (cdr alst)))))

(define (my-list1 a . x)
  (print "my-list1's a is: " a)
  (print "my-list1's x is: " x)
  (print "dotted pair is: " (cons 123 456))
  (cons a x))

(define (foo val)
  (lambda (delta)
    (set! val (+ val delta))
    val))

(define (my-set-car! p v) (set-car! p v))
(define (my-car p) (car p))

(System.Console.WriteLine "Hi There!")

(define (compose f g)
  (lambda (x)
    (f (g x))))

(do ((i 0 (+ i 1)))
    ((= i 10))
  (print "i == " i)
  (print "(eq? 'a 'a) ==> " (eq? 'a 'a))
  (print "(eq? 'a 'b) ==> " (eq? 'a 'b))
  (print "(eq? 1 1) ==> " (eq? 1 1))
  (print "(eq? (+ 1 1) (+ 1 1)) ==> " (eq? (+ 1 1) (+ 1 1)))
  (print "(eq? 2 2) ==> " (eq? 2 2))
  (print "(eq? 1 0) ==> " (eq? 1 0))
  (print "(eq? 1 '()) ==> " (eq? 1 '()))
  (print "(eq? '() '()) ==> " (eq? '() '()))

  (let ((x (cons 'a 'a))
	(y (cons 'a 'a)))
    (print "(eq? x x) ==> " (eq? x x))
    (print "(eq? y y) ==> " (eq? y y))
    (print "(eq? x y) ==> " (eq? x y))))

(print (null? '(a)))
(print (null? '()))
(print (pair? '(a)))
(print (pair? '()))

(print `(a b c ,(+ 1 2) d e f))

(cond
 ((assq 'f '((a 1) (b 2) (c 3) (d 4) (e 5) (f 6) (g 7)))
  => (compose System.Console.WriteLine cadr))
 (else (System.Console.WriteLine "Nothing")))

(System.Console.WriteLine
 (cond
  ((assq 'z '((a 1) (b 2) (c 3) (d 4) (e 5) (f 6) (g 7))) => cadr)
  (else "Nothing...")))

(define (not x) (if x #f #t))

(print "R5RS pitfall 7.2 (should be 28): "
       (let ()
	 (define r #f)
	 (define a #f)
	 (define b #f)
	 (define c #f)
	 (define i 0)
	 (let () 
	   (set! r (+ 1
		      (+ 2 (+ 3 (call-with-current-continuation
				 (lambda (k) (set! a k) 4))))
		      (+ 5 (+ 6 (call-with-current-continuation
				 (lambda (k) (set! b k) 7))))))
	   (if (not c) 
	       (set! c a))
	   (set! i (+ i 1))
	   (cond
	    ((= i 1) (b 8))
	    ((= i 2) (a 5))
	    ((= i 3) (b 7))
	    ((= i 4) (c 4)))
	   r)))

(print "(values 1): " (values 1))
(print "(values 1 2 3 4): " (call-with-values (lambda () (values 1 2 3 4)) my-list1))
(print "(values): " (call-with-values (lambda () (values)) (lambda args args)))

(System.Console.WriteLine "Bye now!")
