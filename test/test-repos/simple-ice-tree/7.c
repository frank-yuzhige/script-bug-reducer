#include "7.h"
#include "10.h"
#include <stdio.h>

int f7(void) {
  printf("7 depends on 10\n");
  f10();
  return 7;
}