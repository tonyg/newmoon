(sys$install-binding '%define-global-variable 'global
		     (lambda (name value)
		       (sys$install-binding name 'global value)))

(define (display x)
  (%jvm-assemble '(x) (x)
    '((getstatic "java.lang.System" "out" "java.io.PrintStream")
      ($ x)
      (invoke "java.io.PrintStream" "print" void (object) virtual)
      (getstatic "sisc.data.SchemeVoid" "VOID" "sisc.data.SchemeVoid"))))

(define (newline)
  (%jvm-assemble '(x) (x)
    '((getstatic "java.lang.System" "out" "java.io.PrintStream")
      (invoke "java.io.PrintStream" "println" void () virtual)
      (getstatic "sisc.data.SchemeVoid" "VOID" "sisc.data.SchemeVoid"))))

(display '(1 2 3))
(newline)
