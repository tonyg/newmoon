#include "newmoon-code.h"

#include <stdio.h>
#include <stdarg.h>

oop defineGlobal(oop name, oop kind, oop value) {
  binary *s = (binary *) symbol_name(name);
  box *b = lookup_global(s->data, oop_len(s));
  setbox(b, value);
  return b;
}

static void __attribute__((noreturn)) apply_implementation(closure *self,
							   int argc,
							   oop k,
							   oop f,
							   ...)
{
  die("apply");
}
closure apply_closure = (closure) {
  MAKE_GC_INFO(TYPE_CLOSURE, 0, 1),
  (newmoon_code) apply_implementation
};
oop apply_oop = &apply_closure;

oop string_split_by_chars(oop s, oop charstr) {
  oop result = mknull();
  size_t limit;
  pair *prev = NULL;
  char *separators;
  size_t separators_len;
  size_t tokstart;
  size_t pos;
  char *sdata;

  if (!isbinary(s)) die("string_split_by_chars: string is not a binary");
  if (!isbinary(charstr)) die("string_split_by_chars: separators is not a binary");

  separators = ((binary *) charstr)->data;
  separators_len = oop_len(charstr);

  sdata = ((binary *) s)->data;
  tokstart = pos = 0;
  limit = oop_len(s);

  while (pos < limit) {
    /* Find the first separator */
    while (pos < limit && memchr(separators, sdata[pos], separators_len) == NULL) pos++;
    if (tokstart != pos) {
      binary *tok = raw_alloc(sizeof(binary) + (pos - tokstart));
      pair *p = raw_cons(tok, mknull());
      init_binary_header(tok->header, TYPE_BINARY, (pos - tokstart));
      memcpy(tok->data, &sdata[tokstart], pos - tokstart);
      if (prev == NULL) {
	result = p;
      } else {
	prev->cdr = p;
      }
      prev = p;
    }
    /* Find the first nonseparator */
    while (pos < limit && memchr(separators, sdata[pos], separators_len) != NULL) pos++;
    tokstart = pos;
  }

  return result;
}

oop string_join(oop strlist, oop sepstr) {
  size_t total_length = 0;
  size_t sep_length;
  int needsep = 0;
  oop pos;
  binary *result;
  char *c;

  if (!isbinary(sepstr)) die("string_join: sepstr is not a binary");
  sep_length = oop_len(sepstr);
  for (pos = strlist; ispair(pos); pos = ((pair *) pos)->cdr) {
    oop s = ((pair *) pos)->car;
    if (!isbinary(s)) die("string_join: element of strlist is not a binary");
    if (needsep) total_length += sep_length;
    total_length += oop_len(s);
    needsep = 1;
  }
  if (!isnil(pos)) die("string_join: strlist is not a list");
  result = raw_alloc(sizeof(binary) + total_length);
  init_binary_header(result->header, TYPE_BINARY, total_length);
  c = result->data;
  needsep = 0;
  for (pos = strlist; ispair(pos); pos = ((pair *) pos)->cdr) {
    oop s = ((pair *) pos)->car;
    if (needsep) {
      memcpy(c, ((binary *) sepstr)->data, sep_length);
      c += sep_length;
    }
    memcpy(c, ((binary *) s)->data, oop_len(s));
    c += oop_len(s);
    needsep = 1;
  }
  return result;
}

oop numeric_plus(oop a, oop b) {
  intptr_t v = DETAG(a) + DETAG(b);
  return litint(v);
}

oop numeric_minus(oop a, oop b) {
  intptr_t v = DETAG(a) - DETAG(b);
  return litint(v);
}

int numeric_equality(oop a, oop b) {
  return DETAG(a) == DETAG(b);
}

int numeric_lt(oop a, oop b) {
  return DETAG(a) < DETAG(b);
}

int numeric_gt(oop a, oop b) {
  return DETAG(a) > DETAG(b);
}

oop basic_library_load_module(oop name) {
  if (!isbinary(name)) die("basic_library_load_module: not a string");
  return load_module(((binary *) name)->data);
}
