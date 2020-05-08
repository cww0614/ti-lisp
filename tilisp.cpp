#include <iostream>

#include "tilisp.h"
#include "helper.h"

value_t *display(const value_t *value) {
  if (value->type == TYPE_INTEGER) {
    std::cout << value->value.int_value << std::endl;
  }
  return nullptr;
}

// Assumes semant has already filter out pairs which don't have the same type
// though this hasn't been implemented yet
value_t *cpp_add(const value_t *value_1, const value_t *value_2) {
    if (value_1->type == TYPE_INTEGER) {
        /* int int_value = (value_1->value).int_value + (value_1->value).int_value; */
        int tmp1 = (value_1->value).int_value;
        int tmp2 = (value_2->value).int_value;

        value_t *output = new value_t;
        output->type = TYPE_INTEGER;
        output->value.int_value = tmp1+tmp2;
        return output;
    }
    else{
        std::cout << "Not implemented" << std::endl;
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
      std::cout << (int)min;
    } else {
      std::cout << (int)min << " to " << (int)max;
    }

    std::cout << " arguments, but got " << arg_num;
    exit(1);
  }
}
