TARGETS = basic-library.dll r5rs-misc.dll r5rs-ports.dll r5rs-eval.dll

include ../../Makefile.global

basic-library.dll: basic-library.scm
	(cd ../../compiler; ./newmoon-mz -b -N "basic-library" -X ../lib/newmoon-lib/basic-library.scm)

%.dll: %.scm
	(cd ../../compiler; ./newmoon-mz -N "$*" ../lib/newmoon-lib/$<)
