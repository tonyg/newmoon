all: loader module.so

clean:
	rm -f loader module.so
	rm -rf loader.dSYM module.so.dSYM .libs

loader: loader.c
	glibtool --tag=CC --mode=link \
		gcc -g -O0 -o loader loader.c

module.so: module.c
	glibtool --tag=CC --mode=link \
		gcc -g -O0 -dynamiclib -o module.so module.c