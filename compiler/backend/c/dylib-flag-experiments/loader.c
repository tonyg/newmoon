#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <stdint.h>

#include <dlfcn.h>

int main(int argc, char **argv) {
  void *h, *s;

  if (argc < 2) {
    fprintf(stderr, "usage: loader <module>\n");
    exit(1);
  }

  h = dlopen(argv[1], RTLD_LAZY | RTLD_LOCAL);
  if (h == NULL) {
    fprintf(stderr, "%s\n", dlerror());
    exit(1);
  }
  fprintf(stderr, "Opened %s\n", argv[1]);

  s = dlsym(h, "the_symbol");
  if (s == NULL) {
    fprintf(stderr, "Couldn't find the_symbol\n");
    exit(1);
  }
  fprintf(stderr, "Found the_symbol %p\n", s);

  fprintf(stderr, "Calling the_symbol\n");
  ((void (*)(void))s)();
  fprintf(stderr, "Called the_symbol\n");
  return 0;
}
