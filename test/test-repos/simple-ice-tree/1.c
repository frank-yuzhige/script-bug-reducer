#include "1.h"
#include "2.h"
#include "3.h"
#include "4.h"
#include <stdio.h>

int f1(void) {
  printf("1 depends on 2, 3, 4\n");
  f2();
  f3();
  f4();
  return 1;
}

int main(int argc, char **argv) {
  f1();
}