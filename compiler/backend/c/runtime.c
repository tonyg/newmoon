#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <stdint.h>

#include <setjmp.h>
#include <stdarg.h>
#include <dlfcn.h>
#include <errno.h>

#include "newmoon-code.h"

typedef struct rootvec {
  size_t length;
  struct rootvec *next;
  oop roots[0];
} rootvec;

static unsigned int stack_limit_bytes = 256 * 1024;
static jmp_buf toplevel_jmp;
oop *gc_limit; /* nursery limit (on the stack) */

static rootvec *roots = NULL;

static size_t heapsize;
static size_t heapalloced;
static uint8_t *heapbase;
static uint8_t *heapptr;

#define INITIAL_SYMTAB_LEN 409
static vector *symtab;

static oop globals = mknull();

void newmoontrace(char const *assemblyname, char const *functionname) {
  fprintf(stderr, "%s::%s\n", assemblyname, functionname);
}

void __attribute__((noreturn)) wrong_type(int arg_index) {
  fprintf(stderr, "Wrong argument type in index %d\n", arg_index);
  exit(1);
}

void __attribute__((noreturn)) wrong_fixed_argc(int argc, int arity) {
  fprintf(stderr, "Expected exactly %d arguments, got %d\n", arity, argc);
  exit(1);
}

void __attribute__((noreturn)) wrong_variable_argc(int argc, int arity) {
  fprintf(stderr, "Expected at least %d arguments, got %d\n", arity, argc);
  exit(1);
}

void __attribute__((noreturn)) wrong_fixed_argc_apply(oop args, int arity) {
  /* TODO: print args */
  fprintf(stderr, "Expected exactly %d arguments\n", arity);
  exit(1);
}

void __attribute__((noreturn)) wrong_variable_argc_apply(oop args, int arity) {
  /* TODO: print args */
  fprintf(stderr, "Expected at least %d arguments\n", arity);
  exit(1);
}

void __attribute__((noreturn)) bad_index(void) {
  die("Vector index out-of-range");
}

void __attribute__((noreturn)) die(char const *message) {
  fprintf(stderr, "FATAL: %s\n", message);
  exit(1);
}

void __attribute__((noreturn)) scheme_error(char const *message) {
  die(message);
}

void __attribute__((noreturn)) scheme_posix_error(char const *message, int posix_errno) {
  errno = posix_errno;
  perror(message);
  exit(1);
}

oop symbol_name(oop sym) {
  if (!issymbol(sym)) die("symbol_name: not a symbol");
  return ((symbol *) sym)->name;
}

oop gensym(char const *prefix) {
  static int counter = 14641;
  char *buf = alloca(strlen(prefix) + 20);
  oop result;
  sprintf(buf, "%s%d", prefix, counter++);
  return intern(buf, strlen(buf));
}

static void init_gc(void) {
  heapsize = 1024 * 1024;
  heapalloced = 0;
  heapbase = malloc(heapsize);
  heapptr = heapbase;
}

static void double_heap(void) {
  uint8_t *newbase;

  heapsize <<= 1;

  newbase = realloc(heapbase, heapsize);
  if (newbase == NULL) {
    die("Could not double heap");
  }
  if (newbase != heapbase) {
    /* Edgy. */
    die("Could not resize heap in place");
  }

  heapbase = newbase;
  heapptr = heapbase + heapalloced;
}

void *raw_alloc(size_t size_bytes) {
  size_bytes += sizeof(oop) - 1;
  size_bytes &= ~(sizeof(oop) - 1); /* align to pointer size */

  while (heapalloced + size_bytes > heapsize) {
    double_heap();
  }

  {
    void *ptr = heapptr;
    heapalloced += size_bytes;
    heapptr = heapbase + heapalloced;
    return ptr;
  }
}

static void init_symtab(void) {
  int i;
  symtab = raw_alloc(sizeof(vector) + INITIAL_SYMTAB_LEN * sizeof(oop));
  init_object_header(symtab->header, TYPE_VECTOR, INITIAL_SYMTAB_LEN);
  for (i = 0; i < INITIAL_SYMTAB_LEN; i++) {
    symtab->data[i] = mknull();
  }
}

pair *raw_cons(oop a, oop d) {
  pair *p = raw_alloc(sizeof(pair));
  *p = (pair) mkpair(a, d);
  return p;
}

static box *raw_box(oop v) {
  box *b = raw_alloc(sizeof(box));
  init_object_header(b->header, TYPE_BOX, 1);
  b->value = v;
  return b;
}

void __attribute__((noreturn)) gc_stack_collector(void *f, ...) {
  exit(11);
}

box *lookup_global(char const *name, size_t len) {
  oop namesym = intern(name, len);
  pair *p = (pair *) globals;

  while (p != mknull()) {
    pair *entry = (pair *) p->car;
    if (entry->car == namesym) {
      return (box *) entry->cdr;
    }
    p = p->cdr;
  }

  {
    box *entry_box = raw_box(mkvoid());
    globals = raw_cons(raw_cons(namesym, entry_box), globals);
    return entry_box;
  }
}

oop vector_to_list(oop v) {
  int i;
  oop result = mknull();
  fprintf(stderr, "vector_to_list %p\n", v);
  if (!isvector(v)) die("vector_to_list: not a vector");
  for (i = oop_len(v) - 1; i >= 0; i--) {
    result = raw_cons(((vector *) v)->data[i], result);
  }
  return result;
}

/* Courtesy of http://eternallyconfuzzled.com/tuts/algorithms/jsw_tut_hashing.aspx */
static uint32_t fnv_hash(void const *data, size_t length) {
  uint8_t const *p = data;
  uint32_t h = 2166136261;
  size_t i;
  for (i = 0; i < length; i++)
    h = (h * 16777619) ^ p[i];
  return h;
}

oop intern(char const *str, size_t len) {
  size_t current_table_width = oop_len(symtab);
  uint32_t probe = fnv_hash(str, len) % current_table_width;

  {
    pair *p = symtab->data[probe];
    while (p != mknull()) {
      symbol *sym = (symbol *) p->car;
      binary *name = sym->name;
      if ((oop_len(name) == len)
	  && !memcmp(name->data, str, len)) {
	return sym;
      }
      p = p->cdr;
    }
  }

  {
    binary *name = raw_alloc(sizeof(binary) + len);
    symbol *sym = raw_alloc(sizeof(symbol));

    init_binary_header(name->header, TYPE_BINARY, len);
    memcpy(name->data, str, len);

    init_object_header(sym->header, TYPE_SYMBOL, 1);
    sym->name = name;

    symtab->data[probe] = raw_cons(sym, symtab->data[probe]);
    return sym;
  }
}

oop scheme_display(oop x, oop p) {
  if (isint(x)) {
    printf("%ld", (long) DETAG(x));
  } else if (isnil(x)) {
    printf("()");
  } else if (ispair(x)) {
    printf("(");
    scheme_display(((pair *) x)->car, p);
    printf(" . ");
    scheme_display(((pair *) x)->cdr, p);
    printf(")");
  } else if (issymbol(x)) {
    printf("%s", ((binary *) symbol_name(x))->data);
  } else if (isbinary(x)) {
    printf("%s", ((binary *) x)->data);
  } else if (isvector(x)) {
    int i;
    int want_space = 0;
    printf("#(");
    for (i = 0; i < oop_len(x); i++) {
      if (want_space) printf(" ");
      scheme_display(((vector *) x)->data[i], p);
      want_space = 1;
    }
    printf(")");
  } else {
    printf("#<unknown>");
  }
  return mkvoid();
}

extern oop scheme_newline(oop p) {
  printf("\n");
  return mkvoid();
}

void registerroots(int root_count, ...) {
  rootvec *rv = malloc(sizeof(rootvec) + root_count * sizeof(oop));
  va_list vl;
  int i;

  rv->length = root_count;
  rv->next = roots;
  va_start(vl, root_count);
  for (i = 0; i < root_count; i++) {
    rv->roots[i] = va_arg(vl, oop);
  }
  va_end(vl);
  roots = rv;
}

typedef void (*initGlobals_fn)(void);
typedef __attribute__((noreturn)) void (*startup_fn)(oop,int,continuation);
static struct bootmod {
  struct bootmod *next;
  char const *name;
  initGlobals_fn initGlobals;
  startup_fn startup;
} *bootmods = NULL;

static void push_bootmod_def(char const *name, initGlobals_fn ig, startup_fn s) {
  struct bootmod *bm = malloc(sizeof(struct bootmod));
  fprintf(stderr, "Adding boot module definition %s with %p/%p\n", name, ig, s);
  bm->next = bootmods;
  bm->name = name;
  bm->initGlobals = ig;
  bm->startup = s;
  bootmods = bm;
}

static int load_module_internal(char const *name, struct bootmod *bm) {
  void *h, *ig, *s;

  fprintf(stderr, "Opening boot module module %s...\n", name);
  h = dlopen(name, RTLD_LAZY | RTLD_LOCAL);
  if (h == NULL) {
    fprintf(stderr, "%s\n", dlerror());
    return ENOENT;
  }

  ig = dlsym(h, "InitGlobals");
  if (ig == NULL) {
    die("Couldn't find InitGlobals symbol");
  }
  s = dlsym(h, "Startup");
  if (s == NULL) {
    die("Couldn't find Startup symbol");
  }
  //dlclose(h);

  bm->name = name;
  bm->initGlobals = ig;
  bm->startup = s;
  return 0;
}

static void push_bootmod(char const *name) {
  char *fullname;
  void *h, *ig, *s;
  struct bootmod bm;

  fullname = malloc(strlen(name) + 4); /* ".so\0" */
  strcpy(fullname, name);
  strcat(fullname, ".so");
  if (load_module_internal(fullname, &bm) != 0) {
    die("Couldn't open boot module");
  }
  push_bootmod_def(fullname, bm.initGlobals, bm.startup);
}

int try_load_module(char const *name, char const *extension, struct bootmod *bm) {
  char const *dotpos = strrchr(name, '.');
  char *buf;
  if (dotpos == NULL) {
    dotpos = name + strlen(name);
  }
  buf = alloca(dotpos - name + strlen(extension) + 1);
  memcpy(buf, name, dotpos - name);
  strcpy(buf + (dotpos - name), extension);
  return load_module_internal(buf, bm);
}

oop load_module(char const *name) {
  struct bootmod bm;
  if (try_load_module(name, ".so", &bm) == 0) goto loaded;
  if (try_load_module(name, ".dylib", &bm) == 0) goto loaded;
  if (load_module_internal(name, &bm) == 0) goto loaded;
  fprintf(stderr, "load_module: cannot load '%s'\n", name);
  exit(1);
 loaded:
  bm.initGlobals();
  {
    constant_closure_env *cl = raw_alloc(sizeof(constant_closure_env));
    init_object_header(cl->header, TYPE_CLOSURE, 0);
    cl->code = (newmoon_code) bm.startup;
    return cl;
  }
}

static __attribute__((noreturn)) void toplevel_k(constant_closure_env *self,
						 int argc,
						 oop result)
{
  if (bootmods != NULL) {
    struct bootmod *bm = bootmods;
    startup_fn startup = bm->startup;
    bootmods = bm->next;
    fprintf(stderr, "Booting module %s...\n", bm->name);
    free(bm);
    startup(NULL, 1, self);
  } else {
    longjmp(toplevel_jmp, 1);
  }
}

int newmoon_main(int argc,
		 char const *argv,
		 initGlobals_fn initGlobals,
		 startup_fn startup)
{
  init_gc();
  init_symtab();

  push_bootmod_def("(main)", initGlobals, startup);
  push_bootmod("basic-library");

  {
    struct bootmod *bm = bootmods;
    while (bm != NULL) {
      fprintf(stderr, "Initialising globals for %s\n", bm->name);
      bm->initGlobals();
      bm = bm->next;
    }
  }

  if (setjmp(toplevel_jmp) == 0) {
    oop base_oop;

    gc_limit = &base_oop - (stack_limit_bytes / sizeof(oop));

    allocenv(constant_closure_env, 0, toplevel_k, toplevel_k_oop);
    toplevel_k(toplevel_k_oop, 1, mkvoid());
  }
  return 0;
}
