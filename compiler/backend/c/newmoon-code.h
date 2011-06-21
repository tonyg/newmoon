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

typedef struct {
  object_header header;
  oop name;
} symbol;

#define TAG(x, v)	((oop) (((x) << 2) | ((v) & 3)))
#define DETAG(x)	(((intptr_t) (x)) >> 2)
#define GETTAG(x)	(((intptr_t) (x)) & 3)

#define istagged(x)	(GETTAG(x) != 0)
#define isoop(x)	(GETTAG(x) == 0 && (x) != NULL)

#define isint(x)	(GETTAG(x) == TAG_INT)

#define MAKE_GC_INFO(type, length, flag) TAG(((length) << 8) | ((type) & 255), flag)

#define init_object_header(h, type, length) h.gc_info = MAKE_GC_INFO(type, length, 1)
#define init_binary_header(h, type, length) h.gc_info = MAKE_GC_INFO(type, length, 2)

#define oop_len(h)	(DETAG(((object_header *)(h))->gc_info) >> 8)
#define oop_type(h)	(DETAG(((object_header *)(h))->gc_info) & 255)

#define is_oop_of_type(x,t)	(isoop(x) && oop_type(x) == t)
#define ispair(x)	is_oop_of_type(x, TYPE_PAIR)
#define isvector(x)	is_oop_of_type(x, TYPE_VECTOR)
#define isrecord(x)	is_oop_of_type(x, TYPE_RECORD)
#define isbinary(x)	is_oop_of_type(x, TYPE_BINARY)
#define issymbol(x)	is_oop_of_type(x, TYPE_SYMBOL)

#define isnil(x)	((x) == mknull())

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
#define globalbox(n) (global__ ## n)
#define globalget(n) (global__ ## n->value)
#define globalset(n,v) (global__ ## n->value = (v))
#define stackcheckdispatch(f) ({ oop top_; (&top_ < gc_limit) ? gc_stack_collector : (f);})
#define checkedcallfun(f, arity, ...) \
             (stackcheckdispatch(((closure *) (f))->code)(f, arity, __VA_ARGS__))
#define callfun(f, arity, ...) ((((closure *) (f))->code)(f, arity, __VA_ARGS__))
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
#define conditional(test, t, f)				\
  if ((test) != mkfalse()) { goto conditional__ ## t; }	\
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
#define mkvec(size) mkveclike(TYPE_VECTOR, size)
#define defbinary(name, len, initbytes)			  \
  binary *name = ({					  \
      binary *_tmp = alloca(sizeof(binary) + len);	  \
      init_binary_header(_tmp->header, TYPE_BINARY, len); \
      memcpy(&(_tmp->data[0]), initbytes, len);		  \
      _tmp;						  \
    });
#define extractsingleactual(complainer, arity, name)			\
  {									\
    if (isnil(actuals) || !ispair(actuals)) {				\
      complainer(original_actuals, arity);				\
    }									\
    name = ((pair *) actuals)->car;					\
    actuals = ((pair *) actuals)->cdr;					\
  }
#define extractrestactual(complainer, arity, name)			\
  {									\
    name = actuals;							\
  }
#define invokeapplyhook(anchorname, varlambdaname)	\
  varlambdaname(self, anchorname)
#define checkfixedarity(arity, anchorname, varlambdaname)	\
  {								\
    if (argc == -1) invokeapplyhook(anchorname, varlambdaname);	\
    if (argc != arity) wrong_fixed_argc(argc, arity);		\
  }
#define extractvarargs(varname, mandatoryargc, anchorname, varlambdaname, lastrealarg) \
  {									\
    va_list the_va_list;						\
    int the_va_list_counter;						\
    if (argc == -1) invokeapplyhook(anchorname, varlambdaname);		\
    if (argc == -2) {							\
      va_start(the_va_list, lastrealarg);				\
      varname = va_arg(the_va_list, oop);				\
      va_end(the_va_list);						\
    } else {								\
      pair *prev = NULL;						\
      if (argc < mandatoryargc) {					\
	wrong_variable_argc(argc, mandatoryargc);			\
      }									\
      va_start(the_va_list, lastrealarg);				\
      for (the_va_list_counter = argc - mandatoryargc - 1;		\
	   the_va_list_counter >= 0;					\
	   the_va_list_counter--)					\
	{								\
	  pair *p = alloca(sizeof(pair));				\
	  oop a = va_arg(the_va_list, oop);				\
	  p->header.gc_info = MAKE_GC_INFO(TYPE_PAIR, 2, 1);		\
	  p->car = a;							\
	  p->cdr = mknull();						\
	  if (prev == NULL) {						\
	    varname = p;						\
	  } else {							\
	    prev->cdr = p;						\
	  }								\
	  prev = p;							\
	}								\
      va_end(the_va_list);						\
    }									\
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
extern void newmoontrace(char const *assemblyname, char const *functionname);
extern void __attribute__((noreturn)) wrong_type(int arg_index);
extern void __attribute__((noreturn)) bad_index(void);
extern void __attribute__((noreturn)) wrong_fixed_argc(int argc, int arity);
extern void __attribute__((noreturn)) wrong_variable_argc(int argc, int arity);
extern void __attribute__((noreturn)) wrong_fixed_argc_apply(oop args, int arity);
extern void __attribute__((noreturn)) wrong_variable_argc_apply(oop args, int arity);
extern void __attribute__((noreturn)) die(char const *message);
extern void __attribute__((noreturn)) scheme_error(char const *message);
extern void __attribute__((noreturn)) scheme_posix_error(char const *message, int posix_errno);
extern void *raw_alloc(size_t size_bytes);
extern pair *raw_cons(oop a, oop d);
extern oop symbol_name(oop s);
extern oop vector_to_list(oop v);
#define scheme_current_output_port mknull()
extern oop scheme_display(oop x, oop p);
extern oop scheme_newline(oop p);
extern oop load_module(char const *name);
extern int newmoon_main(int argc,
			char const *argv,
			void (*initGlobals)(void),
			__attribute__((noreturn)) void (*startup)(oop,int,continuation));
extern void registerroots(int root_count, ...);

extern oop *gc_limit;
extern void __attribute__((noreturn)) gc_stack_collector(void *, ...);

/*
  Calling convention:

  If varargs:
      If argc >= expected argc, collect rest list from va_list and run as per normal.
      If argc == -1, return pointer for apply hook.
      Otherwise, complain atleast_argc().
  If not varargs:
      If argc == expected argc, run as per normal.
      If argc == -1, return pointer for apply hook.
      Otherwise, complain exactly_argc().

  The apply hook is a function of signature

  void __attribute__((noreturn)) (*newmoon_apply_hook)(void *, oop restlist);

  which reads values out of the restlist, supplying them as real
  parameters to the non-apply-hook version of the function.

  For varargs functions, there will be three forms of the function:
   - a normal entry-point, which has "..." on the end of the formals
   - an apply-hook, which converts a restlist to the third form
   - an internal function, which has an oop restlist as the final formal

  For normal functions, there will be just the two, and the apply hook
  will delegate to the normal form of the function.
*/
