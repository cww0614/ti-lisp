#ifndef TI_LISP_BUILTIN_H
#define TI_LISP_BUILTIN_H

#include <cstdint>
#include <cstring>

extern "C" {

enum value_type {
  TYPE_INTEGER = 0,
  TYPE_CHAR = 1,
  TYPE_STRING = 2,
  TYPE_CONS = 3,
  TYPE_BOOL = 4,
  TYPE_FUNC = 5,
  TYPE_SYMBOL = 6,
};

struct string_struct{
  char *data;
  uint64_t size;
};

struct value_t {
  uint64_t type;

  union {
    int64_t int_value;
    char bool_value;
    char char_value;

    string_struct string_value;

    struct {
      char *data;
      uint64_t size;
    } symbol_value;

    struct {
      struct value_t *car;
      struct value_t *cdr;
    } cons_value;

    struct {
      void *addr;
      void *access_link;
      uint8_t min_arg, max_arg;
    } func_value;
  } value;
};

value_t *display(const value_t *);

void check_type(const value_t *, uint64_t);
void check_func(const value_t *, uint8_t);

value_t *cpp_add(const value_t *value_1, const value_t *value_2);
value_t *cpp_mult(const value_t *value_1, const value_t *value_2);
value_t *cpp_div(const value_t *value_1, const value_t *value_2);
value_t *cpp_subtract(const value_t *value_1, const value_t *value_2);

value_t *cpp_equal(const value_t *value_1, const value_t *value_2);
value_t *cpp_less_than(const value_t *value_1, const value_t *value_2);
value_t *cpp_more_than(const value_t *value_1, const value_t *value_2);
value_t *cpp_leq(const value_t *value_1, const value_t *value_2);
value_t *cpp_geq(const value_t *value_1, const value_t *value_2);
value_t *cpp_concat(const value_t *value_1, const value_t *value_2);
}

#endif /* end of include guard: TI_LISP_BUILTIN_H */
