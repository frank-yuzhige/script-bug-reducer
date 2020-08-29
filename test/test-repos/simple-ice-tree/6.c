#include "6.h"

#include "9.h"
#include "10.h"
#include <stdio.h>

int f6(void) {
  printf("6 depends on 9, 10\n");
  f9();
  f10();
  return 6;
}