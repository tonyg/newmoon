; MzScheme support code.

(define (run-tests)
  (define compiler-tests
    (make-test-suite "newmoon compiler tests"

		     (make-test-case
		      "exceptions capturable using '=>'"
		      (assert-equal? 2
				     (try-catch (raise 'foo 1)
						((foo) => (lambda (e)
							    (+ (car (exception-arguments e))
							       1)))
						(else => (lambda (e) e)))))

		     (make-test-case
		      "exceptions catchable without using '=>'"
		      (assert-equal? 2
				     (try-catch (raise 'foo 1)
						((foo) 2)
						(else 'other-exception))))

		     (make-test-case
		      "missed exceptions capturable using '=>'"
		      (assert-equal? '(bar 1)
				     (try-catch (raise 'bar 1)
						((foo) => (lambda (e)
							    (+ (car (exception-arguments e))
							       1)))
						(else => (lambda (e) e)))))

		     (make-test-case
		      "missed exceptions catchable without using '=>'"
		      (assert-equal? 2
				     (try-catch (raise 'foo 1)
						((foo) 2)
						(else 'other-exception))))

		     (make-test-case
		      "finalizer runs normally"
		      (assert-equal? #t
				     (let ((v #f))
				       (try-catch 1
						  ((foo) 2)
						  (else 3)
						  (finally (set! v #t)))
				       v)))

		     (make-test-case
		      "finalizer runs on expected exception"
		      (assert-equal? #t
				     (let ((v #f))
				       (try-catch (raise 'foo 1)
						  ((foo) 2)
						  (else 3)
						  (finally (set! v #t)))
				       v)))

		     (make-test-case
		      "finalizer runs on unexpected exception with else"
		      (assert-equal? #t
				     (let ((v #f))
				       (try-catch (raise 'bar 1)
						  ((foo) 2)
						  (else 3)
						  (finally (set! v #t)))
				       v)))

		     (make-test-case
		      "flatten-body works"
		      (assert-equal? '((define a 1)
				       (define b 2)
				       (display (+ a b))
				       (newline)
				       (define c 3)
				       (* b c))
				     (flatten-body '((define a 1)
						     (begin
						       (define b 2)
						       (display (+ a b))
						       (newline))
						     (define c 3) ; note: illegal in this position
						     (* b c)))))

		     )) ; end of newmoon compiler tests

  (define (rt-eval expr)
    (bytecode-apply (bytecode-compile expr) '()))

  (define (rt-expect result expr)
    (assert-equal? result (rt-eval expr)))

  (define runtime-tests
    (make-test-suite "newmoon runtime tests"

		     (make-test-case
		      "quasiquote on lists works"
		      (rt-expect '(1 (2 0) 3 4 5)
				 '`(1 (2 ,(- 2 2)) ,(+ 1 2) ,@(list 4 5))))

		     (make-test-case
		      "quasiquote on vectors works"
		      (rt-expect '#(1 #(2 0) 3 4 5)
				 '`#(1 #(2 ,(- 2 2)) ,(+ 1 2) ,@(list 4 5))))

		     (make-test-case
		      "begin is always its own head expression"
		      (assert-eq? 5
				  (try-catch (rt-eval '(let ((a +)
							     (b 1)
							     (c 2)
							     (d 3))
							 (a (begin b c) d)))
					     (else => (lambda (e) e)))))

		     (make-test-case
		      "macros aren't expanded if they're shadowed"
		      (rt-expect 5
				 '((lambda (case) (case 2))
				   (lambda (lambda) (+ lambda 3)))))

		     (make-test-case
		      "No Reserved Identifiers"
		      (rt-expect 5
				 '((lambda (lambda define) (lambda define define))
				   (lambda (lambda define) (+ lambda 3)) 2)))

		     (make-test-case
		      "formals mustn't contain non-symbol"
		      (assert-eq? 'got-exception
				  (try-catch (rt-eval '((lambda (2) 'no-exception) 'dummy))
					     ((syntax-error) 'got-exception))))

		     (make-test-case
		      "varargs formals mustn't be non-symbol"
		      (assert-eq? 'got-exception
				  (try-catch (rt-eval '((lambda 2 'no-exception)))
					     ((syntax-error) 'got-exception))))

		     (make-test-case
		      "macros can expand to definitions (outer)"
		      (begin
			(rt-eval '(defmacro test-case-define-outer (v1 v2)
				    `(begin
				       (define ,v1 "hello")
				       (define ,v2 "world"))))
			(rt-eval '(test-case-define-outer xx yy))
			(rt-expect "helloworld"
				   '(string-append xx yy))))

		     (make-test-case
		      "macros can expand to definitions (inner)"
		      (begin
			(rt-eval '(defmacro test-case-define-inner (v1 v2)
				    `(begin
				       (define ,v1 "goodbye")
				       (define ,v2 "world"))))
			(rt-expect "goodbyeworld"
				   '(let ()
				      (test-case-define-inner xx yy)
				      (string-append xx yy)))))

		     (make-test-case
		      "r5rs pitfall 1.1"
		      (rt-expect 0
				 '(let ((cont #f))
				    (letrec ((x (call-with-current-continuation
						 (lambda (c) (set! cont c) 0)))
					     (y (call-with-current-continuation
						 (lambda (c) (set! cont c) 0))))
				      (if cont
					  (let ((c cont))
					    (set! cont #f)
					    (set! x 1)
					    (set! y 1)
					    (c 0))
					  (+ x y))))))

		     (make-test-case
		      "r5rs pitfall 1.2"
		      (rt-expect #t
				 '(letrec ((x (call-with-current-continuation list))
					   (y (call-with-current-continuation list)))
				    (cond ((procedure? x) (x (pair? y)))
					  ((procedure? y) (y (pair? x))))
				    (let ((x (car x)) (y (car y)))
				      (and (call-with-current-continuation x)
					   (call-with-current-continuation y)
					   (call-with-current-continuation x))))))

		     (make-test-case
		      "r5rs pitfall 1.3"
		      (rt-expect #t
				 '(letrec ((x (call-with-current-continuation
					       (lambda (c)
						 (list #T c)))))
				    (if (car x)
					((cadr x) (list #F (lambda () x)))
					(eq? x ((cadr x)))))))

		     (make-test-case
		      "r5rs pitfall 2.1"
		      (rt-expect 1
				 '(call-with-current-continuation (lambda (c) (0 (c 1))))))

		     (make-test-case
		      "r5rs pitfall 4.1"
		      (rt-expect '(x)
				 '((lambda lambda lambda) 'x)))

		     (make-test-case
		      "r5rs pitfall 4.2"
		      (rt-expect '(1 2 3)
				 '((lambda (begin) (begin 1 2 3)) (lambda lambda lambda))))

		     (make-test-case
		      "r5rs pitfall 4.3"
		      (rt-expect #f
				 '(let ((quote -)) (eqv? '1 1))))

		     (make-test-case
		      "r5rs pitfall 5.1"
		      (rt-expect #f
				 '(eq? #f '())))

		     (make-test-case
		      "r5rs pitfall 5.2"
		      (rt-expect #f
				 '(eqv? #f '())))

		     (make-test-case
		      "r5rs pitfall 5.3"
		      (rt-expect #f
				 '(equal? #f '())))

		     (make-test-case
		      "r5rs pitfall 6.1"
		      (rt-expect #f
				 '(eq? (string->symbol "f") (string->symbol "F"))))

		     (make-test-case
		      "r5rs pitfall 7.1"
		      (rt-expect 28
				 '(let ()
				    (define r #f)
				    (define a #f)
				    (define b #f)
				    (define c #f)
				    (define i 0)
				    (let () 
				      (set! r (+ 1 (+ 2 (+ 3 (call-with-current-continuation (lambda (k) (set! a k) 4))))
						 (+ 5 (+ 6 (call-with-current-continuation (lambda (k) (set! b k) 7))))))
				      (if (not c) 
					  (set! c a))
				      (set! i (+ i 1))
				      (case i
					((1) (a 5))
					((2) (b 8))
					((3) (a 6))
					((4) (c 4)))
				      r))))

		     (make-test-case
		      "r5rs pitfall 7.2"
		      (rt-expect 28
				 '(let ()
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
				      (case i
					((1) (b 8))
					((2) (a 5))
					((3) (b 7))
					((4) (c 4)))
				      r))))

		     (make-test-case
		      "r5rs pitfall 7.3"
		      (rt-expect '((-1 4 5 3)
				   (4 -1 5 3)
				   (-1 5 4 3)
				   (5 -1 4 3)
				   (4 5 -1 3)
				   (5 4 -1 3))
				 '(let ((k1 #f)
					(k2 #f)
					(k3 #f)
					(state 0))
				    (define (identity x) x)
				    (define (fn)
				      ((identity (if (= state 0)
						     (call-with-current-continuation
						      (lambda (k) (set! k1 k) +))
						     +))
				       (identity (if (= state 0)
						     (call-with-current-continuation
						      (lambda (k) (set! k2 k) 1))
						     1))
				       (identity (if (= state 0)
						     (call-with-current-continuation
						      (lambda (k) (set! k3 k) 2))
						     2))))
				    (define (check states)
				      (set! state 0)
				      (let* ((res '())
					     (r (fn)))
					(set! res (cons r res))
					(if (null? states)
					    res
					    (begin (set! state (car states))
						   (set! states (cdr states))
						   (case state
						     ((1) (k3 4))
						     ((2) (k2 2))
						     ((3) (k1 -)))))))
				    (map check '((1 2 3) (1 3 2) (2 1 3)
						 (2 3 1) (3 1 2) (3 2 1))))))

		     (make-test-case
		      "r5rs pitfall 7.4"
		      (rt-expect '(10 9 8 7 6 5 4 3 2 1 0)
				 '(let ((x '())
					(y 0))
				    (call-with-current-continuation 
				     (lambda (escape)
				       (let* ((yin ((lambda (foo) 
						      (set! x (cons y x))
						      (if (= y 10)
							  (escape x)
							  (begin
							    (set! y 0)
							    foo)))
						    (call-with-current-continuation
						     (lambda (bar) bar))))
					      (yang ((lambda (foo) 
						       (set! y (+ y 1))
						       foo)
						     (call-with-current-continuation
						      (lambda (baz) baz)))))
					 (yin yang)))))))

		     (make-test-case
		      "r5rs pitfall 8.1"
		      (rt-expect -1
				 '(let - ((n (- 1))) n)))

		     (make-test-case
		      "r5rs pitfall 8.2"
		      (rt-expect '(1 2 3 4 1 2 3 4 5)
				 '(let ((ls (list 1 2 3 4)))
				    (append ls ls '(5)))))

		     (make-test-case
		      "r5rs pitfall map-style test"
		      (rt-eval
		       '(let ((result 
			       (let ()
				 (define executed-k #f)
				 (define cont #f)
				 (define res1 #f)
				 (define res2 #f)
				 (set! res1 (map (lambda (x)
						   (if (= x 0)
						       (call-with-current-continuation
							(lambda (k) (set! cont k) 0))
						       0))
						 '(1 0 2)))
				 (if (not executed-k)           
				     (begin (set! executed-k #t) 
					    (set! res2 res1)
					    (cont 1)))
				 res2)))
			  (if (equal? result '(0 0 0))
			      (display "Map is call/cc safe, but probably not tail recursive or inefficient.")
			      (display "Map is not call/cc safe, but probably tail recursive and efficient."))
			  (newline))))

		     (make-test-case
		      "test cps begin regression bug"
		      (rt-expect 3
				 '(let loop () ; implicit begin-with-set!-head here.
				    (if #t
					(let* ((a (+ 1 1))
					       (b (+ 2 2)))
					  3)
					'foo))))

		     (make-test-case
		      "argument expr returns twice"
		      (rt-expect 0
				 '(let ((cont #f))
				    (let ((foo (lambda (x y)
						 (if cont
						     (let ((c cont))
						       (set! cont #f)
						       (set! x 1)
						       (set! y 1)
						       (c 0))
						     (+ x y)))))
				      (foo (call-with-current-continuation
					    (lambda (c) (set! cont c) 0))
					   (call-with-current-continuation
					    (lambda (c) (set! cont c) 0)))))))

		     (make-test-case
		      "cond with no body in a clause returns the test value"
		      (rt-expect 12
				 '(cond
				   ((+ 5 7))
				   (else 'nothing))))

		     (make-test-case
		      "arglist length 0 passed to varargs lambda works okay"
		      (rt-expect '()
				 '((lambda x x))))

		     (make-test-case
		      "arglist length 1 passed to varargs lambda works okay"
		      (rt-expect '(a)
				 '((lambda x x) 'a)))

		     (make-test-case
		      "arglist length 2 passed to varargs lambda works okay"
		      (rt-expect '(a b)
				 '((lambda x x) 'a 'b)))

		     )) ; end of newmoon runtime tests

  (test/text-ui
   (make-test-suite "all newmoon tests"
		    compiler-tests
		    runtime-tests)))
