#!/usr/local/bin/mzscheme -r
; -*- scheme -*-

(require (lib "1.ss" "srfi"))

(load "psyntax-support-mz.scm")
(load "psyntax-support.scm")
(load "psyntax.pp")

(call-with-input-file "psyntax.scm"
  (lambda (p)
    (let loop ()
      (let ((x (read p)))
	(if (not (eof-object? x))
	    (begin
	      (write (sc-expand x))
	      (newline)
	      (newline)
	      (loop)))))))
