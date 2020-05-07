#include "builtins.h"
#include <stdio.h>

struct value_t *display(struct value_t *value) {
  if (value->type == TYPE_INTEGER) {
    printf("%d\n", value->value.int_value);
  }

  return NULL;
}
