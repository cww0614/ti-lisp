#include <stdlib.h>

void* GC_malloc(size_t size) {
  return malloc(size);
}
