#include <stdio.h>
#include <stdlib.h>
extern int *g;
void foo() {
  g = malloc(sizeof(int));
  *g = 10;
  printf("in b: g = %d\n", *g);
}

