ifeq ($(shell uname -s),Darwin)
LIBTOOL=glibtool
else
LIBTOOL=libtool
endif

all: libnewmoon.so

clean:
	rm -f libnewmoon.so

libnewmoon.so: runtime.c
	$(LIBTOOL) --tag=CC --mode=link gcc -g -O0 -fomit-frame-pointer -fno-pic -c -o $@ $<
