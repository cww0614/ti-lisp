#include <iostream>

#include "tilisp.h"
#include "helper.h"

value_t *display(const value_t *value) {
  switch (value->type)
  {
  case TYPE_INTEGER:
    std::cout << value->value.int_value << std::endl;
    break;
  case TYPE_STRING:
    std::cout << value->value.string_value.data << std::endl;
    break;
  case TYPE_CHAR:
    std::cout << value->value.char_value << std::endl;
    break;
  case TYPE_BOOL:
    std::cout << (value->value.bool_value? "true": "false") << std::endl;
    break;
  default:
    std::cout << "'display' not implemented for this type." << std::endl;
    break;
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
        std::cout << "+ not implemented for this type" << std::endl;
        exit(1);
    }
    return nullptr;
}

value_t *cpp_mult(const value_t *value_1, const value_t *value_2) {
    if (value_1->type == TYPE_INTEGER) {
        /* int int_value = (value_1->value).int_value + (value_1->value).int_value; */
        int tmp1 = (value_1->value).int_value;
        int tmp2 = (value_2->value).int_value;

        value_t *output = new value_t;
        output->type = TYPE_INTEGER;
        output->value.int_value = tmp1*tmp2;
        return output;
    }
    else{
        std::cout << "* not implemented for this type" << std::endl;
        exit(1);
    }
    return nullptr;
}

value_t *cpp_div(const value_t *value_1, const value_t *value_2) {
    if (value_1->type == TYPE_INTEGER) {
        /* int int_value = (value_1->value).int_value + (value_1->value).int_value; */
        int tmp1 = (value_1->value).int_value;
        int tmp2 = (value_2->value).int_value;

        value_t *output = new value_t;
        output->type = TYPE_INTEGER;
        output->value.int_value = tmp1 / tmp2;
        return output;
    }
    else{
        std::cout << "/ not implemented for this type" << std::endl;
        exit(1);
    }
    return nullptr;
}

value_t *cpp_subtract(const value_t *value_1, const value_t *value_2) {
    if (value_1->type == TYPE_INTEGER) {
        /* int int_value = (value_1->value).int_value + (value_1->value).int_value; */
        int tmp1 = (value_1->value).int_value;
        int tmp2 = (value_2->value).int_value;

        value_t *output = new value_t;
        output->type = TYPE_INTEGER;
        output->value.int_value = tmp1 - tmp2;
        return output;
    }
    else{
        std::cout << "/ not implemented for this type" << std::endl;
        exit(1);
    }
    return nullptr;
}

// Logical operations.

value_t *cpp_equal(const value_t *value_1, const value_t *value_2) {
  value_t *res = new value_t;
  res->type = TYPE_BOOL;
  switch (value_1->type){
    case TYPE_INTEGER:
    {
      int v1int = (value_1->value).int_value;
      int v2int = (value_2->value).int_value;
      res->value.bool_value = (v1int == v2int);
    }
      break;
    case TYPE_BOOL:
    {
      bool v1bool = (value_1->value).bool_value;
      bool v2bool = (value_2->value).bool_value;
      res->value.bool_value = (v1bool == v2bool);
    }
      break;
    case TYPE_STRING:
    {
      const char* v1 = (value_1->value).string_value.data;
      const char* v2 = (value_2->value).string_value.data;
      res->value.bool_value = (strcmp(v1, v2) == 0);
    }
      break;
    default:
      std::cout << "'=' not implemented for this type" << std::endl;
      res = nullptr;
      exit(1);
  }
  return res;
}

value_t *cpp_less_than(const value_t *value_1, const value_t *value_2) {
  value_t *res = new value_t;
  res->type = TYPE_BOOL;
  switch (value_1->type){
    case TYPE_INTEGER:
    {
      int v1int = (value_1->value).int_value;
      int v2int = (value_2->value).int_value;
      res->value.bool_value = (v1int < v2int);
    }
    break;
    case TYPE_STRING:
    {
      uint64_t l1 = (value_1->value).string_value.size;
      uint64_t l2 = (value_2->value).string_value.size;
      res->value.bool_value = (l1 < l2);
    }
    break;
    default:
      std::cout << "'<' not implemented for this type" << std::endl;
      res = nullptr;
      exit(1);
  }
  return res;
}

value_t *cpp_more_than(const value_t *value_1, const value_t *value_2) {
  value_t *res = new value_t;
  res->type = TYPE_BOOL;
  switch (value_1->type){
    case TYPE_INTEGER:
    {
      int v1int = (value_1->value).int_value;
      int v2int = (value_2->value).int_value;
      res->value.bool_value = (v1int > v2int);
    }
    break;
    case TYPE_STRING:
    {
      uint64_t l1 = (value_1->value).string_value.size;
      uint64_t l2 = (value_2->value).string_value.size;
      res->value.bool_value = (l1 > l2);
    }
    break;
    default:
      std::cout << "'>' not implemented for this type" << std::endl;
      res = nullptr;
      exit(1);
  }
  return res;
}

value_t *cpp_leq(const value_t *value_1, const value_t *value_2) {
  value_t *res = new value_t;
  res->type = TYPE_BOOL;
  switch (value_1->type){
    case TYPE_INTEGER:
    {
      int v1int = (value_1->value).int_value;
      int v2int = (value_2->value).int_value;
      res->value.bool_value = (v1int <= v2int);
    }
      break;

    default:
      std::cout << "'<' not implemented for this type" << std::endl;
      res = nullptr;
      exit(1);
  }
  return res;
}

value_t *cpp_geq(const value_t *value_1, const value_t *value_2) {
  value_t *res = new value_t;
  res->type = TYPE_BOOL;
  switch (value_1->type){
    case TYPE_INTEGER:
    {
      int v1int = (value_1->value).int_value;
      int v2int = (value_2->value).int_value;
      res->value.bool_value = (v1int >= v2int);
    }
      break;

    default:
      std::cout << "'>' not implemented for this type" << std::endl;
      res = nullptr;
      exit(1);
  }
  return res;
}

// Everything else
value_t *cpp_concat(const value_t *value_1, const value_t *value_2) {
    if (value_1->type == TYPE_STRING){
        string_struct tmp1 = (value_1->value).string_value;
        string_struct tmp2 = (value_2->value).string_value;
        string_struct tmp3;
        tmp3.size = tmp1.size + tmp2.size;
        tmp3.data = concat_string_struct(tmp1,tmp2);

        value_t *output = new value_t;
        output->type = TYPE_STRING;
        output->value.string_value = tmp3;
        return output;
    }
    else{
        std::cout << "++ not implemented for this type" << std::endl;
        exit(1);
    }
}

// Checking functions during irgen.
void check_type(const value_t *value, uint64_t expected_type) {
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
