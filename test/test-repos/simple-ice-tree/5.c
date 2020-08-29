#include "5.h"
#include "9.h"
#include <stdio.h>

int f5(void) {
  printf("5 depends on 9\n");
  f9();
  return 5;
}