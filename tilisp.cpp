#include <iostream>

#include "tilisp.h"
#include "helper.h"

value_t *display(const value_t *value) {
  if (value->type == TYPE_INTEGER) {
    std::cout << value->value.int_value << std::endl;
  }

  return nullptr;
}

void check_type(const value_t *value, uint8_t expected_type) {
  if (value == nullptr || value->type != expected_type) {
    std::cout << "Exepcted variable to be a " << type_name(expected_type)
              << ", but got a " << value_type_name(value) << std::endl;
    exit(1);
  }
}

void check_func(const value_t *value, uint8_t arg_num) {
  check_type(value, TYPE_FUNC);

  uint8_t min = value->value.func_value.min_arg;
  uint8_t max = value->value.func_value.max_arg;
  if (arg_num < min || arg_num > max) {
    std::cout << "Function expects to take ";

    if (min == max) {
      std::cout << min;
    } else {
      std::cout << min << " to " << max;
    }

    std::cout << " arguments, but got " << arg_num;
    exit(1);
  }
}
