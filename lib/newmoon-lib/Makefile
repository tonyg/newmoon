TARGETS = basic-library.$(SCMOEXT) r5rs-misc.$(SCMOEXT) r5rs-ports.$(SCMOEXT) r5rs-eval.$(SCMOEXT)

include ../../Makefile.global

basic-library.$(SCMOEXT): basic-library.scm basic-library-implementation.c
	(cd ../../compiler; \
	 ./newmoon-mz -b -N "basic-library" -X \
		-L ../lib/newmoon-lib/basic-library-implementation.c \
		../lib/newmoon-lib/basic-library.scm)
