TARGETS = basic-library.dll r5rs-misc.dll r5rs-ports.dll r5rs-eval.dll

include ../../Makefile.global

basic-library.dll: basic-library.scm
	(cd ../../compiler/backend/dotnet; make NEWMOON_FLAGS="$(NEWMOON_FLAGS) -b" ../../../lib/newmoon-lib/basic-library.dll)
