(emit-annotations #t)
(emit-debugging-symbols #t)
(repl-prompt ">>> ")
(stack-trace-on-error #t)

(load "../../bcel.scm")
(import bcel)

(let ((c (bcel-gen-class "Foo.Bar" "java.lang.Object" "generated-by-test"
			 '("ACC_PUBLIC" "ACC_SUPER") '())))
  (bcel-gen-field! c '("ACC_PUBLIC" "ACC_STATIC") 'int "xyzzy")
  (bcel-gen-method! c '("ACC_PUBLIC" "ACC_STATIC") 'void '() '() "<clinit>"
		    `((goto noprint)
		      (getstatic "java.lang.System" "out" "java.io.PrintStream")
		      (const "Hello there, world!")
		      (invoke "java.io.PrintStream" "println" void ("java.lang.String") virtual)
		      noprint
		      (const 34)
		      (null "java.lang.Object")
		      (null "java.lang.String")
		      (obj-cond != skip)
		      (getstatic "java.lang.System" "out" "java.io.PrintStream")
		      (const "Skipped.")
		      (invoke "java.io.PrintStream" "println" void ("java.lang.String") virtual)
		      skip
		      (putstatic "Foo.Bar" "xyzzy" int)
		      (return void))
		    '())
  (bcel-gen-method! c '("ACC_PUBLIC") 'void '() '() "<init>"
		    `((load "Foo.Bar" 0)
		      (invoke "java.lang.Object" "<init>" void () special)
		      (return void))
		    '())
  (bcel-gen-method! c '("ACC_PUBLIC") 'int '(int) '("arg") "mapper"
		    `((goto sw_begin)
		      ret1
		      (const 10)
		      (return int)
		      ret3
		      (const 30)
		      (return int)
		      ret2
		      (const 20)
		      (return int)
		      ret4
		      (const 40)
		      (return int)
		      sw_begin
		      (load int 1)
		      (tableswitch ret4
				   ((1 ret1)
				    (2 ret2)
				    (3 ret3))))
		    '())
  (bcel-gen-method! c '("ACC_PUBLIC") "java.lang.String" '() '() "toString"
		    `((const "Some Foo.Bar")
		      (return "java.lang.String"))
		    '())
  (bcel-dump-to-file c "Foo/Bar.class"))
