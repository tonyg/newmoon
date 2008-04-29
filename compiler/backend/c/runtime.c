#include <setjmp.h>

#include "newmoon-code.h"

static unsigned int stack_limit_bytes = 256 * 1024;
static jmp_buf toplevel_jmp;
oop *gc_limit;

void __attribute__((noreturn)) gc_stack_collector(void *f, ...) {
  exit(11);
}

static __attribute__((noreturn)) void toplevel_k(constant_closure_env *self,
						 int argc,
						 oop result)
{
  longjmp(toplevel_jmp, 1);
}

box *lookup_global(char const *name) {
  return NULL;
}

oop intern(char const *str) {
  return NULL;
}

void registerroots(int root_count, ...) {
}

int newmoon_main(int argc,
		 char const *argv,
		 void (*initGlobals)(void),
		 void (*startup)(continuation))
{
  initGlobals();
  if (setjmp(toplevel_jmp) == 0) {
    oop base_oop;

    gc_limit = &base_oop - (stack_limit_bytes / sizeof(oop));

    allocenv(constant_closure_env, 0, toplevel_k, toplevel_k_oop);
    startup(toplevel_k_oop);
  }
  return 0;
}
