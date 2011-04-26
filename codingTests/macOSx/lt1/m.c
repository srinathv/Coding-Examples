#include <stdio.h>

void foo();
extern int *g;

int main() {
 foo();
 printf("in main: g = %d\n", *g);
 return 0;
}

