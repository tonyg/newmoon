((lambda ()
   (define (print x)
     (%assemble (x) (x)
       (scheme (display x))
       (dotnet ("// print assembly start")
	       ($ x)
	       (dup)
	       (call "void class [mscorlib]System.Console::Write(object)")
	       ("// print assembly stop"))))
   (print "Hello, ")
   (print "World!")
   (%assemble (result) ('dummy)
     (scheme (newline))
     (dotnet (call "void class [mscorlib]System.Console::WriteLine()")
	     ($ result)))))
