#include <setjmp.h>

#include "newmoon-code.h"

static jmp_buf toplevel_jmp;

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
    allocenv(constant_closure_env, 0, toplevel_k, toplevel_k_oop);
    startup(toplevel_k_oop);
  }
  return 0;
}
