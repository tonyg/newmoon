(%backend c
	  "#include <stdio.h>")

(define (x y)
  (define (v val) (w val))
  (define (w val)
    (%assemble (x) (val)
      (c "printf(\"display: %p\\n\", "x");")
      (scheme (display x))
      (dotnet ("// w assembly start")
	      ($ x)
	      ("// w assembly stop")))
    (if #f #f))
  (v "Hello, ")
  (v y)
  (v "!\n"))

(define (high-arity a b c d e f)
  (+ a b c d e f))

(define (la-va x . allrest)
  (cons x allrest))

(define (ha-va a b c d e f . allrest)
  (list allrest f e d c b a))

(x "world")
