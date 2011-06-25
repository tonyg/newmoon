#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <stdint.h>

#include <assert.h>
#include <setjmp.h>
#include <stdarg.h>
#include <dlfcn.h>
#include <errno.h>

#include "newmoon-code.h"

#define INITIAL_HEAP_SIZE (128 * 1024)
#define INITIAL_NURSERY_SIZE (512 * 1024)
#define GC_NOISE_LEVEL 0

typedef struct rootvec {
  size_t length;
  struct rootvec *next;
  oop *roots[0];
} rootvec;

#define BARRIER_PAGE_MAX ((4096 / sizeof(oop) * 2) - 2)
typedef struct barrier_page {
  size_t count;
  struct barrier_page *next_page;
  oop saved[BARRIER_PAGE_MAX];
} barrier_page;

static unsigned int stack_limit_bytes = INITIAL_NURSERY_SIZE;
static oop nursery_reentry_closure;
static oop nursery_reentry_args;
static sigjmp_buf toplevel_jmp;
oop *gc_nursery_base; /* nursery base (at the top of the stack on x86) */
oop *gc_nursery_limit; /* nursery limit (at the bottom of the stack on x86) */

static rootvec *roots = NULL;
static barrier_page *barrier = NULL;

heap active_heap;

#define INITIAL_SYMTAB_LEN 409
static vector *symtab;

static oop globals;

#undef newmoontrace
void newmoontrace(char const *assemblyname, char const *functionname) {
  fflush(NULL);
  fprintf(stderr, "%s::%s\n", assemblyname, functionname);
}

void __attribute__((noreturn)) wrong_type(int arg_index) {
  fflush(NULL);
  fprintf(stderr, "Wrong argument type in index %d\n", arg_index);
  exit(1);
}

void __attribute__((noreturn)) wrong_fixed_argc(int argc, int arity) {
  fflush(NULL);
  fprintf(stderr, "Expected exactly %d arguments, got %d\n", arity, argc);
  exit(1);
}

void __attribute__((noreturn)) wrong_variable_argc(int argc, int arity) {
  fflush(NULL);
  fprintf(stderr, "Expected at least %d arguments, got %d\n", arity, argc);
  exit(1);
}

void __attribute__((noreturn)) wrong_fixed_argc_apply(oop args, int arity) {
  /* TODO: print args */
  fflush(NULL);
  fprintf(stderr, "Expected exactly %d arguments\n", arity);
  exit(1);
}

void __attribute__((noreturn)) wrong_variable_argc_apply(oop args, int arity) {
  /* TODO: print args */
  fflush(NULL);
  fprintf(stderr, "Expected at least %d arguments\n", arity);
  exit(1);
}

void __attribute__((noreturn)) bad_index(void) {
  die("Vector index out-of-range");
}

void __attribute__((noreturn)) die(char const *message) {
  fflush(NULL);
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

void __attribute__((noreturn)) call_to_non_procedure(oop receiver, int argc, ...) {
  fflush(NULL);
  fprintf(stderr, "Call to non-procedure (argc == %d)\n", argc);
  printf("Receiver: ");
  scheme_display(receiver, NULL);
  printf("\n");
  fflush(NULL);
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
  active_heap.size = INITIAL_HEAP_SIZE;
  active_heap.gc_threshold = active_heap.size * 3 / 4;
  active_heap.double_threshold = active_heap.size / 2;
  active_heap.double_next_collection = 0;
  active_heap.alloced = 0;
  active_heap.base = malloc(active_heap.size);
  active_heap.ptr = active_heap.base;

  barrier = malloc(sizeof(barrier_page));
  barrier->count = 0;
  barrier->next_page = NULL;

  globals = mknull();

  registerroots(1, &globals);
}

static inline int heap_needs_gc(heap *h) {
  return h->alloced > h->gc_threshold;
}

void *raw_alloc(size_t size_bytes) {
  size_bytes += sizeof(oop) - 1;
  size_bytes &= ~(sizeof(oop) - 1); /* align to pointer size */

  if (heap_needs_gc(&active_heap)) {
    gc_nursery_limit = gc_nursery_base; /* signal that a GC is required */
    if (active_heap.alloced > active_heap.size) {
      die("Out of memory");
    }
  }

  {
    void *ptr = active_heap.ptr;
    active_heap.alloced += size_bytes;
    active_heap.ptr = active_heap.base + active_heap.alloced;
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

void add_to_write_barrier(oop p) {
  object_header *o = (object_header *) p;
  int flag = GETTAG(o->gc_info);
  size_t index;

  if (flag == FLAG_MUTATED) {
    return;
  }

  assert(flag == FLAG_ORDINARY); /* it is an error to put anything else through the barrier */
  assert(oop_type(p) != TYPE_CLOSURE); /* closures are a special shape and immutable anyway */

  index = barrier->count;
  if (index == BARRIER_PAGE_MAX) {
    barrier_page *newpage = malloc(sizeof(barrier_page));
    index = newpage->count = 0;
    newpage->next_page = barrier;
    barrier = newpage;
  }
  barrier->count++;
  barrier->saved[index] = p;

  o->gc_info = TAG(DETAG(o->gc_info), FLAG_MUTATED);
}

static box *raw_box(oop v) {
  box *b = raw_alloc(sizeof(box));
  init_object_header(b->header, TYPE_BOX, 1);
  b->value = v;
  return b;
}

static void dump_heapstats(char const *heapname, heap *heap) {
#if GC_NOISE_LEVEL > 0
  fprintf(stderr, "%s heap size %lu alloced %lu base %p\n",
	  heapname,
	  heap->size,
	  heap->alloced,
	  heap->base);
#endif
}

static oop gc_copy(oop p); /* forward */
static void gc_copy_array(oop *v, size_t count) {
  int i;
  for (i = 0; i < count; i++) {
    v[i] = gc_copy(v[i]);
  }
}

static oop gc_copy(oop p) {
  object_header *o = (object_header *) p;
  int flag;
  size_t len;

#if GC_NOISE_LEVEL > 2
  fprintf(stderr, "gc_copy(%p)\n", p);
#endif

  if (istagged(p)) {
    return p;
  }

  if (is_in_heap(p, active_heap)) {
    return p;
  }

  flag = GETTAG(o->gc_info);
  len = oop_len(p);

  switch (flag) {
    case FLAG_FORWARDING:
      return o->gc_info;
    case FLAG_BINARY: {
      binary *source = (binary *) p;
      binary *target = raw_alloc(sizeof(binary) + len);
      init_binary_header(target->header, TYPE_BINARY, len);
      memcpy(target->data, source->data, len);
      o->gc_info = target;
      return o->gc_info;
    }
    case FLAG_ORDINARY:
      if (oop_type(p) == TYPE_CLOSURE) {
	closure *source = (closure *) p;
	closure *target = raw_alloc(sizeof(closure) + (len * sizeof(oop)));
	init_object_header(target->header, TYPE_CLOSURE, len);
	target->code = source->code;
	memcpy(target->environment, source->environment, len * sizeof(oop));
	o->gc_info = target;
	gc_copy_array(target->environment, len);
	return o->gc_info;
      }
      /* else fall through */
    case FLAG_MUTATED: {
      /* we know this can't be a closure because of the check in add_to_write_barrier */
      vector *source = (vector *) p;
      vector *target = raw_alloc(sizeof(vector) + (len * sizeof(oop)));
      init_object_header(target->header, oop_type(p), len);
      memcpy(target->data, source->data, len * sizeof(oop));
      o->gc_info = target;
      gc_copy_array(target->data, len);
      return o->gc_info;
    }
  }

  assert(0); /* notreached */
}

static void alloc_new_arena(void) {
  if (active_heap.double_next_collection) {
    active_heap.size <<= 1;
    active_heap.gc_threshold <<= 1;
    active_heap.double_threshold <<= 1;
    active_heap.double_next_collection = 0;
  }
  active_heap.alloced = 0;
  active_heap.base = malloc(active_heap.size);
  active_heap.ptr = active_heap.base;
}

static void dealloc_heap(heap *heap) {
#if GC_NOISE_LEVEL > 0
  fprintf(stderr, "dealloc_heap %p of size %lu\n",
	  heap->base,
	  heap->size);
#endif
  free(heap->base);
}

void __attribute__((noreturn)) gc_stack_collector_no_varlambda() {
  assert(0);
}

void __attribute__((noreturn)) gc_stack_collector(oop self, int argc, ...) {
  /* Note that self is really the function that we were about to call
     when we were interrupted by the need to GC! */

  oop arglist = mkvoid();
  int switching_main_heaps;
  heap old_heap; /* only initialized and used if switching_main_heaps != 0 */

  fflush(NULL);
  dump_heapstats("main", &active_heap);

  switching_main_heaps = heap_needs_gc(&active_heap);
  if (switching_main_heaps) {
    old_heap = active_heap;
    alloc_new_arena();
  }

#if GC_NOISE_LEVEL > 1
  fprintf(stderr, "gc_stack_collector IN  receiver %p with %d args\n", self, argc);
  fprintf(stderr, "bottom of stack %p, limit %p, top %p\n",
	  &arglist, gc_nursery_limit, gc_nursery_base);
#endif
  extractvarargs(arglist, 0, argc, gc_stack_collector_no_varlambda, argc);

  while (1) {
    int i;
    for (i = 0; i < barrier->count; i++) {
      vector *p = (vector *) barrier->saved[i];
      int flag = GETTAG(p->header.gc_info);
#if GC_NOISE_LEVEL > 2
      fprintf(stderr, "barrier %d %p flag %d count %ld type %ld\n",
	      i, p, flag, oop_len(p), oop_type(p));
#endif
      if (flag == FLAG_FORWARDING) {
	/* it has already been copied */
	continue;
      }
      assert(flag == FLAG_MUTATED);
      gc_copy_array(p->data, oop_len(p));
      p->header.gc_info = TAG(DETAG(p->header.gc_info), FLAG_ORDINARY);
    }
    if (barrier->next_page == NULL) {
      barrier->count = 0;
      break;
    } else {
      barrier_page *tmp_page = barrier->next_page;
      free(barrier);
      barrier = tmp_page;
    }
  }

  {
    rootvec *rv = roots;
    while (rv != NULL) {
      int i;
      for (i = 0; i < rv->length; i++) {
	oop *rptr = rv->roots[i];
	*rptr = gc_copy(*rptr);
      }
      rv = rv->next;
    }
  }

  if (switching_main_heaps) {
    /* Copy the symbol table over. */
    vector *oldtab = symtab;
    int i;
    init_symtab();
    for (i = 0; i < INITIAL_SYMTAB_LEN; i++) {
      pair *p = oldtab->data[i];
      while (p != mknull()) {
	symbol *sym = (symbol *) p->car;
	int sym_flag = GETTAG(sym->header.gc_info);
	assert(!is_in_heap(p, active_heap));
	if (sym_flag == FLAG_FORWARDING) {
	  /* It's a live symbol. */
	  assert(is_in_heap(sym->header.gc_info, active_heap));
	  sym = sym->header.gc_info;
	  symtab->data[i] = raw_cons(sym, symtab->data[i]);
	}
	p = p->cdr;
      }
    }
  }

  self = gc_copy(self);
  arglist = gc_copy(arglist);

#if GC_NOISE_LEVEL > 1
  fprintf(stderr, "gc_stack_collector OUT receiver %p with %d args\n", self, argc);
#endif

  if (switching_main_heaps) {
    dealloc_heap(&old_heap);

    if (active_heap.alloced > active_heap.double_threshold) {
#if GC_NOISE_LEVEL > 0
      fprintf(stderr, "main heap has %lu bytes alloced, which is above double threshold %lu\n",
	      active_heap.alloced,
	      active_heap.double_threshold);
#endif
      active_heap.double_next_collection = 1;
    }
  }

  nursery_reentry_closure = self;
  nursery_reentry_args = arglist;
  siglongjmp(toplevel_jmp, 2);
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
  if (!isvector(v)) die("vector_to_list: not a vector");
  for (i = oop_len(v) - 1; i >= 0; i--) {
    result = raw_cons(((vector *) v)->data[i], result);
  }
  return result;
}

/* Courtesy of http://eternallyconfuzzled.com/tuts/algorithms/jsw_tut_hashing.aspx */
static uint32_t fnv_hash(void const *data, size_t length) {
  uint8_t const *p = data;
  uint32_t h = 0x811c9dc5;
  size_t i;
  for (i = 0; i < length; i++)
    h = (h * 0x1000193) ^ p[i];
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
    printf("%.*s",
	   (int) oop_len(symbol_name(x)),
	   ((binary *) symbol_name(x))->data);
  } else if (isbinary(x)) {
    printf("%.*s", 
	   (int) oop_len(x),
	   ((binary *) x)->data);
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
    rv->roots[i] = va_arg(vl, oop *);
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
  fflush(NULL);
  /* fprintf(stderr, "Adding boot module definition %s with %p/%p\n", name, ig, s); */
  bm->next = bootmods;
  bm->name = name;
  bm->initGlobals = ig;
  bm->startup = s;
  bootmods = bm;
}

static int load_module_internal(char const *name, struct bootmod *bm) {
  void *h, *ig, *s;

  fflush(NULL);
  /* fprintf(stderr, "Opening boot module %s...\n", name); */
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
    fflush(NULL);
    /* fprintf(stderr, "Booting module %s...\n", bm->name); */
    free(bm);
    startup(NULL, 1, self);
  } else {
    siglongjmp(toplevel_jmp, 1);
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
      fflush(NULL);
      /* fprintf(stderr, "Initialising globals for %s\n", bm->name); */
      bm->initGlobals();
      bm = bm->next;
    }
  }

  switch (sigsetjmp(toplevel_jmp, 0)) {
    case 0: {
      oop base_oop;

      gc_nursery_base = &base_oop;
      gc_nursery_limit = &base_oop - (stack_limit_bytes / sizeof(oop));

      allocenv(constant_closure_env, 0, toplevel_k, toplevel_k_oop);
      toplevel_k(toplevel_k_oop, 1, mkvoid());
      assert(0); /* we never return directly, using siglongjmp instead */
    }

    case 1:
      /* here's where the toplevel_k ends up when the whole program is over */
      break;

    case 2: {
      /* here's where we end up on a minor GC */
      oop base_oop;

      gc_nursery_base = &base_oop;
      gc_nursery_limit = &base_oop - (stack_limit_bytes / sizeof(oop));

      callfun(nursery_reentry_closure, -1, nursery_reentry_args);
      assert(0); /* we never return directly here, either */
    }
  }
  return 0;
}
