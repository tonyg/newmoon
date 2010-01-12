ifeq ($(shell uname -s),Darwin)
LIBTOOL=glibtool
else
LIBTOOL=libtool
endif

SOEXT=so

all: libnewmoon.$(SOEXT)

clean:
	rm -f libnewmoon.$(SOEXT)

libnewmoon.$(SOEXT): runtime.c
	$(LIBTOOL) --tag=CC --mode=link gcc -g -O0 -fomit-frame-pointer -fno-pic -c -o $@ $<
