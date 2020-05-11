#ifndef HELPER_H
#define HELPER_H

#include "tilisp.h"

const char *value_type_name(const value_t *value);
const char *type_name(uint8_t type);
char* concat_string_struct(string_struct tmp1, string_struct tmp2);
void assert_not_nil(const value_t *value);
void check_if_cons(value_t* value);

#endif
