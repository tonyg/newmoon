(define (even? x) (zero? (remainder x 2)))
(define (odd? x) (not (even? x)))
(define (abs x) (if (negative? x) (- x) x))
(define (modulo x y) (remainder x y)) ;; %%% incorrect definitions of modulo, remainder
(define (gcd a b) (error "Unimplemented: gcd"))
(define (lcm a b) (error "Unimplemented: lcm"))
(define (numerator x) x)
(define (denominator x) 1)
(define (floor x) x)
(define (ceiling x) x)
(define (truncate x) x)
(define (round x) x)
(define (rationalize x y) x)
(define (exp x) 1) ;; %%%
(define (log x) 0) ;; %%%
(define (sin x) 0) ;; %%%
(define (cos x) 0) ;; %%%
(define (tan x) 0) ;; %%%
(define (asin x) 0) ;; %%%
(define (acos x) 0) ;; %%%
(define (atan x . maybe-y) 0) ;; %%%
(define (sqrt x) (/ x 2)) ;; %%%
(define (expt x y) (* x y)) ;; %%%
(define (make-rectangular x y) x)
(define (make-polar x y) x)
(define (real-part x) x)
(define (imag-part x) 0)
(define (magnitude x) (abs x))
(define (angle x) 0)
(define (exact->inexact x) x)
(define (inexact->exact x) x)
(define (number->string x . maybe-radix) "")
(define (string->number x) 0)
