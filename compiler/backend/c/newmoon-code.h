#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <alloca.h>
#include <stdint.h>
#include <stdio.h>

#define DISABLE_NEWMOONTRACE 1

typedef void *oop;
typedef oop continuation;

typedef struct heap {
  uint8_t *base;
  uint8_t *ptr;
  size_t alloced;
  size_t gc_threshold;
  size_t size;
  size_t double_threshold;
  int double_next_collection;
} heap;

typedef struct {
  oop gc_info;
} object_header;

typedef void __attribute__((noreturn)) (*newmoon_code)(void *, ...);
typedef struct {
  object_header header;
  newmoon_code code;
  oop environment[0];
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
#define isspecial(x)	(GETTAG(x) == TAG_SPECIAL)

#define MAKE_GC_INFO(type, length, flag) TAG(((length) << 8) | ((type) & 255), flag)

typedef enum {
  FLAG_FORWARDING = 0, /* used to indicate a copied pointer during GC */
  FLAG_BINARY = 1,     /* a simple binary value */
  FLAG_ORDINARY = 2,   /* an ordinary oop-based object (closures are special!) */
  FLAG_MUTATED = 3,    /* an oop-based object that has been through the write barrier */
} gc_info_flag;

#define init_object_header(h, type, length) h.gc_info = MAKE_GC_INFO(type, length, FLAG_ORDINARY)
#define init_binary_header(h, type, length) h.gc_info = MAKE_GC_INFO(type, length, FLAG_BINARY)

#define oop_len(h)	(DETAG(((object_header *)(h))->gc_info) >> 8)
#define oop_type(h)	(DETAG(((object_header *)(h))->gc_info) & 255)

#define is_oop_of_type(x,t)	(isoop(x) && oop_type(x) == t)
#define ispair(x)	is_oop_of_type(x, TYPE_PAIR)
#define isvector(x)	is_oop_of_type(x, TYPE_VECTOR)
#define isrecord(x)	is_oop_of_type(x, TYPE_RECORD)
#define isbinary(x)	is_oop_of_type(x, TYPE_BINARY)
#define issymbol(x)	is_oop_of_type(x, TYPE_SYMBOL)
#define isclosure(x)	is_oop_of_type(x, TYPE_CLOSURE)

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

typedef enum {
  SPECIAL_NULL = 0,
  SPECIAL_VOID = 1,
  SPECIAL_TRUE = 2,
  SPECIAL_FALSE = 3,
  SPECIAL_EOF = 4
} special_enum;

#define is_in_heap(p, h)			\
  ((((uint8_t *) (p)) >= (h).base) &&	\
   (((uint8_t *) (p)) < (h).ptr))

#define defglobal(n) static box *global__ ## n = NULL
#define defliteral(n) static oop n = NULL
#define globalbox(n) (global__ ## n)
#define globalboxaddr(n) (&globalbox(n))
#define globalget(n) (global__ ## n->value)
#define globalset(n,v) setbox(global__ ## n, v)
#define stackcheckdispatch(f)			\
  ({						\
    oop top_;					\
    (&top_ < gc_nursery_limit)			\
      ? (newmoon_code) gc_stack_collector	\
      : (f);					\
  })
#define writebarrier(p)				\
  ({						\
    if (is_in_heap(p, active_heap))		\
      add_to_write_barrier(p);			\
  })
#define closurecode(f)							\
  (isclosure(f) ? ((closure *) (f))->code : (newmoon_code) call_to_non_procedure)
#define checkedcallfun(f, arity, ...)				\
  (stackcheckdispatch(closurecode(f))(f, arity, __VA_ARGS__))
#define callfun(f, arity, ...)			\
  (closurecode(f)(f, arity, __VA_ARGS__))
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
#define setbox(var, val) \
  ({ box *box_ = ((box *) (var)); writebarrier(box_); box_->value = (val); })
#define getbox(var) (((box *) (var))->value)
#define settemp(var, val) var = val
#define tempaddr(var) (&(var))
#define conditional(test, t, f)				\
  if ((test) != mkfalse()) { goto conditional__ ## t; }	\
  else { goto conditional__ ## f; }
#define deflabel(l) conditional__ ## l:
#define emptystmt()
#define defstorage(varname, type, initialiser) type varname = initialiser
#define mkpair(a, d) { MAKE_GC_INFO(TYPE_PAIR, 2, FLAG_ORDINARY), a, d }
#define deftemp(varname, e) oop varname = e
#define addressof(x) ((oop) &(x))
#define initglobal(v, name) global__ ## v = lookup_global(name, strlen(name));
#define mknull() TAG(SPECIAL_NULL, TAG_SPECIAL)
#define mkvoid() TAG(SPECIAL_VOID, TAG_SPECIAL)
#define mktrue() TAG(SPECIAL_TRUE, TAG_SPECIAL)
#define mkfalse() TAG(SPECIAL_FALSE, TAG_SPECIAL)
#define mkeof() TAG(SPECIAL_EOF, TAG_SPECIAL)
#define mkchar(x) TAG(x, TAG_CHAR)
#define litint(i) TAG(i, TAG_INT)
#define mkveclike(typetag, size) ({					\
      intptr_t len = (intptr_t) (size);					\
      vector *v = alloca(sizeof(vector) + len * sizeof(oop));		\
      int vi;								\
      init_object_header(v->header, typetag, len);			\
      for (vi = 0; vi < len; vi++) {					\
	v->data[vi] = mkvoid();						\
      }									\
      (oop) v;								\
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
#define defbinaryset(name, len, initbyte)		  \
  binary *name = ({					  \
      binary *_tmp = alloca(sizeof(binary) + len);	  \
      init_binary_header(_tmp->header, TYPE_BINARY, len); \
      memset(&(_tmp->data[0]), initbyte, len);		  \
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
      varname = mknull();						\
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
	  *p = (pair) mkpair(a, mknull());				\
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

extern heap active_heap;

extern box *lookup_global(char const *name, size_t len);
extern oop intern(char const *str, size_t len);
extern oop gensym(char const *prefix);
#if DISABLE_NEWMOONTRACE
#define newmoontrace(assemblyname, functionname)
#else
extern void newmoontrace(char const *assemblyname, char const *functionname);
#endif
extern void __attribute__((noreturn)) wrong_type(int arg_index);
extern void __attribute__((noreturn)) bad_index(void);
extern void __attribute__((noreturn)) wrong_fixed_argc(int argc, int arity);
extern void __attribute__((noreturn)) wrong_variable_argc(int argc, int arity);
extern void __attribute__((noreturn)) wrong_fixed_argc_apply(oop args, int arity);
extern void __attribute__((noreturn)) wrong_variable_argc_apply(oop args, int arity);
extern void __attribute__((noreturn)) die(char const *message);
extern void __attribute__((noreturn)) scheme_posix_error(char const *message, int posix_errno);
extern void __attribute__((noreturn)) call_to_non_procedure(oop receiver, int argc, ...);
extern void *raw_alloc(size_t size_bytes);
extern pair *raw_cons(oop a, oop d);
extern void add_to_write_barrier(oop p);
extern oop symbol_name(oop s);
extern oop vector_to_list(oop v);
extern void scheme_display(FILE *f, oop x);
extern oop load_module(char const *name);

extern int newmoon_argc;
extern char const **newmoon_argv;
extern int newmoon_main(int argc,
			char const **argv,
			void (*initGlobals)(void),
			__attribute__((noreturn)) void (*startup)(oop,int,continuation));

extern oop *gc_nursery_base;
extern oop *gc_nursery_limit;
extern void registerroots(int root_count, ...);
extern void __attribute__((noreturn)) gc_stack_collector(oop receiver, int argc, ...);

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
