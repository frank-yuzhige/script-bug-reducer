#include <stdio.h>
#include "bug.h"
#include "good.h"

int main() {
  printf("GoodFun prints %d\n", goodFun());
  printf("BadFun  prints %d\n", buggyFun());
  return 0;
}