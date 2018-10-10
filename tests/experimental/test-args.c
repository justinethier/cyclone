#include <stdio.h>
#include <stdlib.h>

int *buf;
int fglo() {
  return buf[0] + buf[1] + buf[2];
}
int fargs(int x, int y, int z) {
  return x + y + z;
}

void main() {
  buf = calloc(sizeof(int), 3);
  long long sum = 0;
  int MAX = 1 * 1000;

  for (int a = 0; a < MAX; a++) {
    for (int b = 0; b < MAX; b++) {
      for (int c = 0; c < MAX; c++) {
        buf[0] = a;
        buf[1] = b;
        buf[2] = c;
        sum += fglo();
        //sum += fargs(a, b, c);
      }
    }
  }
  printf("%ld\n", sum);
}
