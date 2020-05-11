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
  TYPE_FLOAT = 7,
};

struct string_struct {
  char *data;
  uint64_t size;
};

struct value_t {
  uint64_t type;

  union {
    int64_t int_value;
    double real_value;
    bool bool_value;
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

value_t *display(const void *, const value_t *);

void check_type(const value_t *, uint64_t);
void check_func(const value_t *, uint8_t);

value_t *cpp_add(const void *, const value_t *value_1, const value_t *value_2);
value_t *cpp_mult(const void *, const value_t *value_1, const value_t *value_2);
value_t *cpp_div(const void *, const value_t *value_1, const value_t *value_2);
value_t *cpp_subtract(const void *, const value_t *value_1,
                      const value_t *value_2);

value_t *cpp_equal(const void *, const value_t *value_1,
                   const value_t *value_2);
value_t *cpp_less_than(const void *, const value_t *value_1,
                       const value_t *value_2);
value_t *cpp_more_than(const void *, const value_t *value_1,
                       const value_t *value_2);
value_t *cpp_leq(const void *, const value_t *value_1, const value_t *value_2);
value_t *cpp_geq(const void *, const value_t *value_1, const value_t *value_2);

value_t *is_integer(const void *, const value_t *value);
value_t *is_char(const void *, const value_t *value);
value_t *is_float(const void *, const value_t *value);
value_t *is_string(const void *, const value_t *value);
value_t *is_cons(const void *, const value_t *value);
value_t *is_bool(const void *, const value_t *value);
value_t *is_function(const void *, const value_t *value);
value_t *is_symbol(const void *, const value_t *value);
value_t *is_nil(const void *, const value_t *value);

value_t *cpp_concat(const void *, const value_t *value_1,
                    const value_t *value_2);

value_t* cpp_cons(const void *, const value_t *value_1, const value_t *value_2);
value_t* cpp_car(const void *, const value_t *value1);
value_t* cpp_cdr(const void *, const value_t *value1);
void display_helper(const value_t *value, bool lst);

void *GC_malloc(size_t size);
}

#endif /* end of include guard: TI_LISP_BUILTIN_H */
