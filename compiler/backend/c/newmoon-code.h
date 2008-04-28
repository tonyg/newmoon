#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <alloca.h>

typedef void *oop;
typedef oop continuation;

typedef struct {
  oop gc_info;
} object_header;

typedef void __attribute__((noreturn)) (*newmoon_code)(void *, ...);
typedef struct {
  object_header header;
  newmoon_code code;
} closure;

typedef closure constant_closure_env;

typedef struct {
  object_header header;
  oop value;
} box;

typedef struct {
  object_header header;
  oop car;
  oop cdr;
} pair;

typedef struct {
  object_header header;
  double value;
} floatholder;

typedef struct {
  object_header header;
  char data[0];
} binary;

typedef struct {
  object_header header;
  oop data[0];
} vector;

#define TAG(x, v)	((oop) (((x) << 2) | ((v) & 3)))
#define DETAG(x)	(((int) (x)) >> 2)
#define GETTAG(x)	(((int) (x)) & 3)

#define istagged(x)	(GETTAG(x) != 0)
#define isoop(x)	(GETTAG(x) == 0)

#define isint(x)	(GETTAG(x) == TAG_INT)

#define MAKE_GC_INFO(type, length, flag) TAG(((length) << 8) | ((type) & 255), flag)

#define init_object_header(h, type, length) h.gc_info = MAKE_GC_INFO(type, length, 1)
#define init_binary_header(h, type, length) h.gc_info = MAKE_GC_INFO(type, length, 2)

typedef enum {
  TYPE_INVALID = 0,
  TYPE_CLOSURE,
  TYPE_BOX,
  TYPE_PAIR,
  TYPE_FLOAT,
  TYPE_BINARY,
  TYPE_SYMBOL,
  TYPE_VECTOR,
  TYPE_RECORD,
} object_type;

typedef enum {
  TAG_OOP = 0,
  TAG_INT = 1,
  TAG_CHAR = 2,
  TAG_SPECIAL = 3
} tag_enum;

#define defglobal(n) static box *global__ ## n = NULL
#define defliteral(n) static oop n = NULL
#define globalget(n) (global__ ## n->value)
#define globalset(n,v) (global__ ## n->value = (v))
#define callfun(f, arity, ...) (((closure *) (f))->code(f, arity, __VA_ARGS__))
#define directcall(fn, fv, arity, ...) fn(fv, arity, __VA_ARGS__)
#define envref(e, member) ((e)->member)
#define allocenv(env_ty, length, codeptr, varname)	\
  env_ty storage__ ## varname;				     \
  env_ty *varname = & storage__ ## varname;		     \
  init_object_header(varname->header, TYPE_CLOSURE, length); \
  varname->code = (newmoon_code) codeptr;
#define storeenv(varname, member, exp) (varname->member = (exp))
#define cast_to_oop(x) ((oop) (x))
#define installbox(var)				\
  box box__ ## var;					\
  init_object_header(box__ ## var.header, TYPE_BOX, 1);	\
  box__ ## var.value = var;				\
  var = (oop) &box__ ## var;
#define setbox(var, val) (((box *) (var))->value = (val))
#define getbox(var) (((box *) (var))->value)
#define settemp(var, val) var = val
#define conditional(test, t, f)			\
  if (test) { goto conditional__ ## t; }	\
  else { goto conditional__ ## f; }
#define deflabel(l) conditional__ ## l:
#define emptystmt()
#define defstorage(varname, type, initialiser) type varname = initialiser
#define mkpair(a, d) { MAKE_GC_INFO(TYPE_PAIR, 2, 1), a, d }
#define deftemp(varname, e) oop varname = e
#define addressof(x) ((oop) &(x))
#define initglobal(v, name) global__ ## v = lookup_global(name);
#define mknull() TAG(0, TAG_SPECIAL)
#define mkvoid() TAG(1, TAG_SPECIAL)
#define mktrue() TAG(2, TAG_SPECIAL)
#define mkfalse() TAG(3, TAG_SPECIAL)
#define mkchar(x) TAG(x, TAG_CHAR)
#define litint(i) TAG(i, TAG_INT)
#define mkveclike(typetag, size) ({				\
      int len = (size);						\
      vector *v = alloca(sizeof(vector) + len * sizeof(oop));	\
      init_object_header(v->header, typetag, len);		\
      memset(&v->data[0], 0, len * sizeof(oop));		\
      (oop) v; 							\
    })
#define mkrec(size) mkveclike(TYPE_RECORD, size)
#define mkvec(size) mkveclike(TYPE_RECORD, size)
#define defbinary(name, len, initbytes)			  \
  binary *name = ({					  \
      binary *_tmp = alloca(sizeof(binary) + len);	  \
      init_binary_header(_tmp->header, TYPE_BINARY, len); \
      memcpy(&(_tmp->data[0]), initbytes, len);		  \
      _tmp;						  \
    });
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
#define safe_c_string(x) ({				\
      oop s = (x);					\
      int slen;						\
      char *buffer;					\
      if (!isbinary(s)) { wrong_type(-1); }		\
      slen = oop_len(s);				\
      buffer = alloca(slen);				\
      memcpy(buffer, &(((binary *) s)->data[0]), slen); \
      buffer[slen] = '\0';				\
      buffer;						\
    })
#define safe_c_int(x) ({			\
      oop i = (x);				\
      if (!isint(i)) { wrong_type(-1); }	\
      DETAG(i);					\
    })

extern oop apply_oop;
extern oop scheme_current_output_port;
extern box *lookup_global(char const *name);
extern oop intern(char const *str);
extern oop intern_any(oop x);
extern oop gensym(char const *prefix);
extern void wrong_type(int arg_index);
extern void bad_index();
extern void die(char const *message);
extern void scheme_error(char const *message);
extern void scheme_posix_error(char const *message, int posix_errno);
extern oop symbol_name(oop s);
extern oop vector_to_list(oop v);
extern oop string_split_by_chars(oop s, oop cs);
extern oop string_join(oop strs, oop sepstr);
extern oop scheme_display(oop x, oop p);
extern oop scheme_newline(oop p);
extern int newmoon_main(int argc,
			char const *argv,
			void (*initGlobals)(void),
			void (*startup)(continuation));
