#include "4.h"
#include "6.h"
#include "7.h"
#include "8.h"
#include <stdio.h>

int f4(void) {
  printf("4 depends on 6, 7, 8\n");
  f6();
  f7();
  f8();
  return 4;
}