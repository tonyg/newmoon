"true"; exec env sisc --no-repl "$0"
;; Make sure the interpreter really is slower.
(define repeats 1000000)

(define t (time
	   (do ((i repeats (- i 1)))
	       ((= i 0))
	     'nothing)))
(write t)
(newline)
(let ((ms (caadr t)))
  (write (exact->inexact (/ repeats (/ ms 1000)))))
(newline)
