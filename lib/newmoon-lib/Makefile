## TARGETS = basic-library.dll r5rs-misc.dll r5rs-ports.dll r5rs-eval.dll
TARGETS = basic-library.class

include ../../Makefile.global

basic-library.class: basic-library.scm
	(cd ../../compiler; ./newmoon-sisc $(NEWMOON_FLAGS) -b ../lib/newmoon-lib/basic-library.scm)
