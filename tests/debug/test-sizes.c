#include <stdint.h>
#include "../../include/cyclone/types.h"

/* HEAP definitions, based off heap from Chibi scheme */
#define gc_heap_first_block(h) ((object)(h->data + gc_heap_align(gc_free_chunk_size)))
#define gc_heap_last_block(h) ((object)((char*)h->data + h->size - gc_heap_align(gc_free_chunk_size)))
#define gc_heap_end(h) ((object)((char*)h->data + h->size))
#define gc_heap_pad_size(s) (sizeof(struct gc_heap_t) + (s) + gc_heap_align(1))
#define gc_free_chunk_size (sizeof(gc_free_list))

#define gc_align(n, bits) (((n)+(1<<(bits))-1)&(((uintptr_t)-1)-((1<<(bits))-1)))
// 64-bit is 3, 32-bit is 2
//#define gc_word_align(n) gc_align((n), 2)
#define gc_heap_align(n) gc_align(n, 5)

void print_stats(const char *type, size_t sz, size_t heap_sz)
{
  printf("%s \t\t %d \t\t %d \t\t %d\n",
         type, sz, heap_sz, heap_sz - sz);
}
void main() {
  print_stats("bytevector_type", sizeof(bytevector_type), gc_heap_align(sizeof(bytevector_type))  );
  print_stats("c_opaque_type  ", sizeof(c_opaque_type  ), gc_heap_align(sizeof(c_opaque_type  ))  );
  print_stats("closure0_type  ", sizeof(closure0_type  ), gc_heap_align(sizeof(closure0_type  ))  );
  print_stats("closure1_type  ", sizeof(closure1_type  ), gc_heap_align(sizeof(closure1_type  ))  );
  print_stats("closureN_type  ", sizeof(closureN_type  ), gc_heap_align(sizeof(closureN_type  ))  );
  print_stats("cond_var_type  ", sizeof(cond_var_type  ), gc_heap_align(sizeof(cond_var_type  ))  );
  print_stats("cvar_type      ", sizeof(cvar_type      ), gc_heap_align(sizeof(cvar_type      ))  );
  print_stats("double_type    ", sizeof(double_type    ), gc_heap_align(sizeof(double_type    ))  );
  print_stats("integer_type   ", sizeof(integer_type   ), gc_heap_align(sizeof(integer_type   ))  );
  print_stats("macro_type     ", sizeof(macro_type     ), gc_heap_align(sizeof(macro_type     ))  );
  print_stats("mutex_type     ", sizeof(mutex_type     ), gc_heap_align(sizeof(mutex_type     ))  );
  print_stats("pair_type      ", sizeof(pair_type      ), gc_heap_align(sizeof(pair_type      ))  );
  print_stats("port_type      ", sizeof(port_type      ), gc_heap_align(sizeof(port_type      ))  );
  print_stats("primitive_type ", sizeof(primitive_type ), gc_heap_align(sizeof(primitive_type ))  );
  print_stats("string_type    ", sizeof(string_type    ), gc_heap_align(sizeof(string_type    ))  );
  print_stats("symbol_type    ", sizeof(symbol_type    ), gc_heap_align(sizeof(symbol_type    ))  );
  print_stats("vector_type    ", sizeof(vector_type    ), gc_heap_align(sizeof(vector_type    ))  );
//  printf("bytevector_type %d\n", sizeof(bytevector_type));
//  printf("c_opaque_type   %d\n", sizeof(c_opaque_type));
//  printf("closure0_type   %d\n", sizeof(closure0_type));
//  printf("closure1_type   %d\n", sizeof(closure1_type));
//  printf("closureN_type   %d\n", sizeof(closureN_type));
//  printf("cond_var_type   %d\n", sizeof(cond_var_type));
//  printf("cvar_type       %d\n", sizeof(cvar_type    ));
//  printf("double_type     %d\n", sizeof(double_type  ));
////  printf("eof_type      %d\n", sizeof(eof_type     ));
////  printf("forward_type  %d\n", sizeof(forward_type ));
//  printf("integer_type    %d\n", sizeof(integer_type ));
//  printf("macro_type      %d\n", sizeof(macro_type   ));
//  printf("mutex_type      %d\n", sizeof(mutex_type   ));
//  printf("pair_type       %d\n", sizeof(pair_type    ));
//  printf("port_type       %d\n", sizeof(port_type    ));
//  printf("primitive_type  %d\n", sizeof(primitive_type));
//  printf("string_type     %d\n", sizeof(string_type  ));
//  printf("symbol_type     %d\n", sizeof(symbol_type  ));
//  printf("vector_type     %d\n", sizeof(vector_type  ));
}
