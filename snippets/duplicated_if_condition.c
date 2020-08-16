#include <stdio.h>

// See_Also: https://stackoverflow.com/questions/37283128/how-to-catch-a-duplicate-like-wduplicated-cond-from-gcc-6
int main(void) {
  int a = 5;
  if (a == 5) {
    printf("First condition is True, a: %d\n", a);
  } else if (a == 5) {
    printf("Second condition is True, a: %d\n", a);
  }
}
