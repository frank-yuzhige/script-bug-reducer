#include "2.h"
#include "5.h"
#include <stdio.h>

int f2(void) {
  printf("2 depends on 5\n");
  f5();
  return 2;
}