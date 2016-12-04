#include "include/cyclone/types.h"

void main() {
  printf("bytevector_type %d\n", sizeof(bytevector_type));
  printf("c_opaque_type %d\n", sizeof(c_opaque_type));
  printf("closure0_type %d\n", sizeof(closure0_type));
  printf("closure1_type %d\n", sizeof(closure1_type));
  printf("closureN_type %d\n", sizeof(closureN_type));
  printf("cond_var_type %d\n", sizeof(cond_var_type));
  printf("cvar_type     %d\n", sizeof(cvar_type    ));
  printf("double_type   %d\n", sizeof(double_type  ));
//  printf("eof_type      %d\n", sizeof(eof_type     ));
//  printf("forward_type  %d\n", sizeof(forward_type ));
  printf("integer_type  %d\n", sizeof(integer_type ));
  printf("macro_type    %d\n", sizeof(macro_type   ));
  printf("mutex_type    %d\n", sizeof(mutex_type   ));
  printf("pair_type     %d\n", sizeof(pair_type    ));
  printf("port_type     %d\n", sizeof(port_type    ));
  printf("primitive_type %d\n", sizeof(primitive_type));
  printf("string_type   %d\n", sizeof(string_type  ));
  printf("symbol_type   %d\n", sizeof(symbol_type  ));
  printf("vector_type   %d\n", sizeof(vector_type  ));
}
