#include "tilisp.h"
#include "helper.h"

const char *value_type_name(const value_t *value) {
  if (value == nullptr) {
    return "nil";
  }

  return type_name(value->type);
}

const char *type_name(uint8_t type) {
  switch (type) {
  case TYPE_INTEGER:
    return "integer";
  case TYPE_CHAR:
    return "char";
  case TYPE_STRING:
    return "string";
  case TYPE_CONS:
    return "cons";
  case TYPE_BOOL:
    return "boolean";
  case TYPE_FUNC:
    return "function";
  default:
    return "unknown";
  }
}

