(define (x y)
  (define (v val) (display val))
  (v "Hello, ")
  (v y)
  (v "!")
  (newline))

(x "world")
