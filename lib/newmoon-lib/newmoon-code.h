#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <alloca.h>

typedef void *oop;
typedef void __attribute__((noreturn)) (*newmoon_code)(void *, ...);
typedef struct {
  newmoon_code code;
} closure;
typedef oop continuation;
typedef struct {
  oop value;
} box;
typedef struct {
  oop car;
  oop cdr;
} pair;
typedef struct {
  double value;
} floatholder;
typedef struct {
  char const *data;
} stringholder;

#define TAG(x, v)	((oop) (((x) << 2) | ((v) & 3)))
#define DETAG(x)	(((int) (x)) >> 2)

#define tuple
#define defglobal(n) static box *global__ ## n = NULL
#define defliteral(n) static oop n = NULL
#define globalget(n) (global__ ## n->value)
#define globalset(n,v) (global__ ## n->value = (v))
#define callfun(f, arity, ...) (((closure *) (f))->code(f, arity, __VA_ARGS__))
#define directcall(fn, fv, arity, ...) fn(fv, arity, __VA_ARGS__)
#define envref(e, member) ((e)->member)
#define allocenv(env_ty, codeptr, varname) \
  env_ty storage__ ## varname; \
  env_ty *varname = & storage__ ## varname; \
  varname->code = (newmoon_code) codeptr;
#define storeenv(varname, member, exp) (varname->member = (exp))
#define cast_to_oop(x) ((oop) (x))
#define installbox(var) \
  box box__ ## var; \
  box__ ## var.value = var; \
  var = (oop) &box__ ## var;
#define setbox(var, val) (((box *) var)->value = (val))
#define settemp(var, val) var = val
#define conditional(test, t, f) if (test) { t; } else { f; }
#define defstorage(varname, type, initialiser) type varname = initialiser
#define mkpair(a, d) { a, d }
#define deftemp(varname, e) oop varname = e
#define addressof(x) ((oop) &(x))
#define initglobal(v, name) global__ ## v = lookup_global(name);
#define mknull() TAG(0, 3)
#define mkvoid() TAG(1, 3)
#define mktrue() TAG(2, 3)
#define mkfalse() TAG(3, 3)
#define mkchar(x) TAG(x, 2)
#define litint(i) TAG(i, 1)
#define mkstr(s) { s }
/* FIXME: %%% correct initialisation for pairs below */
#define extractvarargs(varname, mandatoryargc, lastrealarg)	\
  {								\
    va_list the_va_list;					\
    int the_va_list_counter;					\
    va_start(the_va_list, lastrealarg);				\
    for (the_va_list_counter = argc - mandatoryargc - 1;	\
	 the_va_list_counter >= 0;				\
	 the_va_list_counter--)					\
      {								\
	pair *p = alloca(sizeof(pair));				\
	oop a = va_arg(the_va_list, oop);			\
	p->car = a;						\
	p->cdr = varname;					\
	varname = p;						\
      }								\
    va_end(the_va_list);					\
  }
#define scheme_boolean(v) ((v) ? mktrue() : mkfalse())

extern oop unimplemented_gen_asm;
extern box *lookup_global(char const *name);
extern oop intern(char const *str);
