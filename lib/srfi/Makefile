TARGETS=1.dll 9.dll 14.dll 13.dll

include ../../Makefile.global

%.dll: %.scm
	(cd ../../compiler; ./newmoon-mz -N "$*" ../lib/srfi/$<)
