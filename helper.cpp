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

char* concat_string_struct(string_struct tmp1, string_struct tmp2){
    char* concat_string = (char*)GC_malloc(tmp1.size+tmp2.size);
    int idx = 0;
    for (; idx < tmp1.size; ++idx){
        concat_string[idx] = tmp1.data[idx];
    }
    for(int i = 0; i < tmp2.size; ++i){
        concat_string[idx+i] = tmp2.data[i];
    }
    return concat_string;
}
