(sys$install-binding '%define-global-variable 'global
		     (lambda (name value)
		       (sys$install-binding name 'global value)))

(define-syntax case-lambda
  (syntax-rules ()
    ((_ (formals body0 body ...) ...)
     (%case-lambda
      (lambda formals body0 body ...)
      ...))))

(let ()
  (define (lll . vs)
    vs)
  (display (lll 'arg0 'arg1 'arg2))
  (newline))

(define (run-loop)
  (define repeats 1000000)
  (do ((i repeats (- i 1)))
      ((= i 0))
    'nothing))

(define (run-loop1)
  (define (- x y)
    (%jvm-assemble '(x y) (x y)
      '(($ x)
	(invoke "sisc.util.Util" "num" "sisc.data.Quantity" ("sisc.data.Value") static)
	($ y)
	(invoke "sisc.util.Util" "num" "sisc.data.Quantity" ("sisc.data.Value") static)
	(invoke "sisc.data.Quantity" "sub" "sisc.data.Quantity" ("sisc.data.Quantity") virtual))))
  (define (= x y)
    (%jvm-assemble '(x y) (x y)
      '(($ x)
	(invoke "sisc.util.Util" "num" "sisc.data.Quantity" ("sisc.data.Value") static)
	($ y)
	(invoke "sisc.util.Util" "num" "sisc.data.Quantity" ("sisc.data.Value") static)
	(const 0)
	(invoke "sisc.data.Quantity" "comp" boolean ("sisc.data.Quantity" int) virtual)
	(invoke "sisc.util.Util" "truth" "sisc.data.SchemeBoolean" (boolean) static))))
  (define repeats 1000000)
  (do ((i repeats (- i 1)))
      ((= i 0))
    'nothing))

(define (run-loop1a)
  (define (- x . y)
    (%jvm-assemble '(x y) (x y)
      '(($ x)
	(invoke "sisc.util.Util" "num" "sisc.data.Quantity" ("sisc.data.Value") static)
	(store "sisc.data.Quantity" 4)
	($ y)
	(check-cast "sisc.data.Pair")
	(store "sisc.data.Pair" 5)

	(load "sisc.data.Pair" 5)
	(getstatic "sisc.data.EmptyList" "EMPTYLIST" "sisc.data.EmptyList")
	(obj-cond != <loop-top>)

	(getstatic "sisc.data.Quantity" "ZERO" "sisc.data.Quantity")
	(load "sisc.data.Quantity" 4)
	(invoke "sisc.data.Quantity" "sub" "sisc.data.Quantity" ("sisc.data.Quantity") virtual)
	(goto <done>)

	<loop-top>				;; <empty>
	(load "sisc.data.Quantity" 4)		;; acc
	(load "sisc.data.Pair" 5)		;; acc restlist
	(dup 1)					;; acc restlist restlist
	(getfield "sisc.data.Pair" "cdr" "sisc.data.Value")
	(check-cast "sisc.data.Pair")		;; acc restlist next
	(store "sisc.data.Pair" 5)		;; acc restlist
	(getfield "sisc.data.Pair" "car" "sisc.data.Value")
	(invoke "sisc.util.Util" "num" "sisc.data.Quantity" ("sisc.data.Value") static)
	(invoke "sisc.data.Quantity" "sub" "sisc.data.Quantity" ("sisc.data.Quantity") virtual)
						;; new-acc
	(load "sisc.data.Pair" 5)		;; new-acc next
	(getstatic "sisc.data.EmptyList" "EMPTYLIST" "sisc.data.EmptyList")
	(obj-cond == <done>)
						;; new-acc
	(store "sisc.data.Quantity" 4)		;; <empty>
	(goto <loop-top>)

	<done>)))
  (define (= x y)
    (%jvm-assemble '(x y) (x y)
      '(($ x)
	(invoke "sisc.util.Util" "num" "sisc.data.Quantity" ("sisc.data.Value") static)
	($ y)
	(invoke "sisc.util.Util" "num" "sisc.data.Quantity" ("sisc.data.Value") static)
	(const 0)
	(invoke "sisc.data.Quantity" "comp" boolean ("sisc.data.Quantity" int) virtual)
	(invoke "sisc.util.Util" "truth" "sisc.data.SchemeBoolean" (boolean) static))))
  (define repeats 1000000)
  (do ((i repeats (- i 1)))
      ((= i 0))
    'nothing))

(define (compiled-binary-- x y)
  (%jvm-assemble '(x y) (x y)
    '(($ x)
      (invoke "sisc.util.Util" "num" "sisc.data.Quantity" ("sisc.data.Value") static)
      ($ y)
      (invoke "sisc.util.Util" "num" "sisc.data.Quantity" ("sisc.data.Value") static)
      (invoke "sisc.data.Quantity" "sub" "sisc.data.Quantity" ("sisc.data.Quantity") virtual))))

(define (compiled-nary-- x . y)
  (%jvm-assemble '(x y) (x y)
    '(($ x)
      (invoke "sisc.util.Util" "num" "sisc.data.Quantity" ("sisc.data.Value") static)
      (store "sisc.data.Quantity" 4)
      ($ y)
      (check-cast "sisc.data.Pair")
      (store "sisc.data.Pair" 5)

      (load "sisc.data.Pair" 5)
      (getstatic "sisc.data.EmptyList" "EMPTYLIST" "sisc.data.EmptyList")
      (obj-cond != <loop-top>)

      (getstatic "sisc.data.Quantity" "ZERO" "sisc.data.Quantity")
      (load "sisc.data.Quantity" 4)
      (invoke "sisc.data.Quantity" "sub" "sisc.data.Quantity" ("sisc.data.Quantity") virtual)
      (goto <done>)

      <loop-top>				;; <empty>
      (load "sisc.data.Quantity" 4)		;; acc
      (load "sisc.data.Pair" 5)			;; acc restlist
      (dup 1) ;; acc restlist restlist
      (getfield "sisc.data.Pair" "cdr" "sisc.data.Value")
      (check-cast "sisc.data.Pair")		;; acc restlist next
      (store "sisc.data.Pair" 5)		;; acc restlist
      (getfield "sisc.data.Pair" "car" "sisc.data.Value")
      (invoke "sisc.util.Util" "num" "sisc.data.Quantity" ("sisc.data.Value") static)
      (invoke "sisc.data.Quantity" "sub" "sisc.data.Quantity" ("sisc.data.Quantity") virtual)
      ;; new-acc
      (load "sisc.data.Pair" 5)	;; new-acc next
      (getstatic "sisc.data.EmptyList" "EMPTYLIST" "sisc.data.EmptyList")
      (obj-cond == <done>)
      ;; new-acc
      (store "sisc.data.Quantity" 4) ;; <empty>
      (goto <loop-top>)

      <done>)))

(define compiled-cased--
  (case-lambda
   ((x)
    (%jvm-assemble '(x) (x)
      '((getstatic "sisc.data.Quantity" "ZERO" "sisc.data.Quantity")
	($ x)
	(invoke "sisc.util.Util" "num" "sisc.data.Quantity" ("sisc.data.Value") static)
	(invoke "sisc.data.Quantity" "sub" "sisc.data.Quantity" ("sisc.data.Quantity") virtual))))
   ((x y)
    (%jvm-assemble '(x y) (x y)
      '(($ x)
	(invoke "sisc.util.Util" "num" "sisc.data.Quantity" ("sisc.data.Value") static)
	($ y)
	(invoke "sisc.util.Util" "num" "sisc.data.Quantity" ("sisc.data.Value") static)
	(invoke "sisc.data.Quantity" "sub" "sisc.data.Quantity" ("sisc.data.Quantity") virtual))))
   ((x . y)
    (%jvm-assemble '(x y) (x y)
      '(($ y)
	(check-cast "sisc.data.Pair")
	(store "sisc.data.Pair" 4)
	($ x)
	(invoke "sisc.util.Util" "num" "sisc.data.Quantity" ("sisc.data.Value") static)

	<loop-top>				;; acc
	(load "sisc.data.Pair" 4)		;; acc restlist
	(dup 1)	;; acc restlist restlist
	(getfield "sisc.data.Pair" "cdr" "sisc.data.Value")
	(check-cast "sisc.data.Pair")		;; acc restlist next
	(store "sisc.data.Pair" 4)		;; acc restlist
	(getfield "sisc.data.Pair" "car" "sisc.data.Value")
	(invoke "sisc.util.Util" "num" "sisc.data.Quantity" ("sisc.data.Value") static)
	(invoke "sisc.data.Quantity" "sub" "sisc.data.Quantity" ("sisc.data.Quantity") virtual)
	;; new-acc
	(load "sisc.data.Pair" 4) ;; new-acc next
	(getstatic "sisc.data.EmptyList" "EMPTYLIST" "sisc.data.EmptyList")
	(obj-cond == <done>)
	;; new-acc
	(goto <loop-top>)

	<done>)))))

(define (run-loop1b)
  (define (= x y)
    (%jvm-assemble '(x y) (x y)
      '(($ x)
	(invoke "sisc.util.Util" "num" "sisc.data.Quantity" ("sisc.data.Value") static)
	($ y)
	(invoke "sisc.util.Util" "num" "sisc.data.Quantity" ("sisc.data.Value") static)
	(const 0)
	(invoke "sisc.data.Quantity" "comp" boolean ("sisc.data.Quantity" int) virtual)
	(invoke "sisc.util.Util" "truth" "sisc.data.SchemeBoolean" (boolean) static))))
  (define repeats 1000000)
  (do ((i repeats (compiled-binary-- i 1)))
      ((= i 0))
    'nothing))

(define (run-loop1c)
  (define (= x y)
    (%jvm-assemble '(x y) (x y)
      '(($ x)
	(invoke "sisc.util.Util" "num" "sisc.data.Quantity" ("sisc.data.Value") static)
	($ y)
	(invoke "sisc.util.Util" "num" "sisc.data.Quantity" ("sisc.data.Value") static)
	(const 0)
	(invoke "sisc.data.Quantity" "comp" boolean ("sisc.data.Quantity" int) virtual)
	(invoke "sisc.util.Util" "truth" "sisc.data.SchemeBoolean" (boolean) static))))
  (define repeats 1000000)
  (do ((i repeats (compiled-nary-- i 1)))
      ((= i 0))
    'nothing))

(define (run-loop1d)
  (define (= x y)
    (%jvm-assemble '(x y) (x y)
      '(($ x)
	(invoke "sisc.util.Util" "num" "sisc.data.Quantity" ("sisc.data.Value") static)
	($ y)
	(invoke "sisc.util.Util" "num" "sisc.data.Quantity" ("sisc.data.Value") static)
	(const 0)
	(invoke "sisc.data.Quantity" "comp" boolean ("sisc.data.Quantity" int) virtual)
	(invoke "sisc.util.Util" "truth" "sisc.data.SchemeBoolean" (boolean) static))))
  (define repeats 1000000)
  (do ((i repeats (compiled-cased-- i 1)))
      ((= i 0))
    'nothing))

(define-syntax define-inline
  (lambda (x)
    (syntax-case x ()
      ((_ (fnname arg ...) instr ...)
       (with-syntax (((actual ...) (generate-temporaries (syntax (arg ...)))))
	 (syntax
	  (define-syntax fnname
	    (syntax-rules ()
	      ((_ actual ...)
	       (%jvm-assemble '(arg ...) (actual ...)
		 '(instr ...)))))))))))

(define (run-loop2)
  (define-inline (- x y)
    ($ x)
    (invoke "sisc.util.Util" "num" "sisc.data.Quantity" ("sisc.data.Value") static)
    ($ y)
    (invoke "sisc.util.Util" "num" "sisc.data.Quantity" ("sisc.data.Value") static)
    (invoke "sisc.data.Quantity" "sub" "sisc.data.Quantity" ("sisc.data.Quantity") virtual))
  (define-inline (= x y)
    ($ x)
    (invoke "sisc.util.Util" "num" "sisc.data.Quantity" ("sisc.data.Value") static)
    ($ y)
    (invoke "sisc.util.Util" "num" "sisc.data.Quantity" ("sisc.data.Value") static)
    (const 0)
    (invoke "sisc.data.Quantity" "comp" boolean ("sisc.data.Quantity" int) virtual)
    (invoke "sisc.util.Util" "truth" "sisc.data.SchemeBoolean" (boolean) static))
  (define repeats 1000000)
  (do ((i repeats (- i 1)))
      ((= i 0))
    'nothing))
