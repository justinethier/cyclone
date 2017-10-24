// A temporary test file
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

void main(){
  char c[128];
  uint32_t val = 0x32363435;
  uint8_t *ptr = (uint8_t *)&val;
  int i, j = 0;
  //memset(c, 0x34, 128);
  for (i = 0; i < 127; i++) {
    c[i] = ptr[j++];    
    if (j == 4) j = 0;
  }
  c[127] = '\0';
  printf("%s\n", c);
  return;
}
