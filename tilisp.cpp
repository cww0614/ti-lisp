#include "tilisp.h"
#include <iostream>

value_t *display(const value_t *value) {
  if (value->type == TYPE_INTEGER) {
    std::cout << value->value.int_value << std::endl;
  }

  return nullptr;
}

static const char *type_name(uint8_t type) {
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

static const char *value_type_name(const value_t *value) {
  if (value == nullptr) {
    return "nil";
  }

  return type_name(value->type);
}

void check_type(const value_t *value, uint8_t expected_type) {
  if (value == nullptr || value->type != expected_type) {
    std::cout << "Exepcted variable to be a " << type_name(expected_type)
              << ", but got a " << value_type_name(value) << std::endl;
    exit(1);
  }
}
