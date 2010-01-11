TARGETS = basic-library.$(SCMOEXT) r5rs-misc.$(SCMOEXT) r5rs-ports.$(SCMOEXT) r5rs-eval.$(SCMOEXT)

include ../../Makefile.global

basic-library.$(SCMOEXT): basic-library.scm
	(cd ../../new-compiler; ./newmoon-mz -b -N "basic-library" -X ../lib/newmoon-lib/basic-library.scm)
