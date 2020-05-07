#ifndef TI_LISP_BUILTIN_H
#define TI_LISP_BUILTIN_H

#include <stdint.h>

enum value_type {
  TYPE_INTEGER = 0,
  TYPE_CHAR = 1,
  TYPE_STRING = 2,
  TYPE_CONS = 3,
  TYPE_BOOL = 4,
  TYPE_FUNC = 5,
};

struct value_t {
  uint8_t type;

  union {
    int64_t int_value;
    char bool_value;
    char char_value;

    struct {
      char *data;
      uint64_t size;
    } string_value;

    struct {
      struct value_t *car;
      struct value_t *cdr;
    } cons_value;

    struct {
      void *addr;
      uint8_t min_arg, max_arg;
    } func_value;
  } value;
};

struct value_t *display(struct value_t *);

#endif /* end of include guard: TI_LISP_BUILTIN_H */
