TARGETS_PRECIOUS = psyntax.pp
TARGETS = newmoon.dll

include ../../Makefile.global

psyntax.pp: psyntax.scm
	./regen-psyntax.ss > tmp-$@
	mv tmp-$@ $@
