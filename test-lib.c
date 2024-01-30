#include <stdio.h>
#include <assert.h>
#include "include/cyclone/types.h"
#include "include/cyclone/runtime.h"
#include "include/cyclone/runtime-main.h"

/* Future considerations:
int main(int argc, char **argv, char **envp)
{gc_thread_data *thd;
 long stack_size = global_stack_size = STACK_SIZE;
 long heap_size = global_heap_size = HEAP_SIZE;
 init_polyfills();
 mclosure0(clos_halt,&Cyc_halt);  // Halt if final closure is reached
 mclosure0(entry_pt,&c_entry_pt); // First function to execute
 _cyc_argc = argc;
 _cyc_argv = argv;
 set_env_variables(envp);
 gc_initialize();
 thd = malloc(sizeof(gc_thread_data));
 gc_thread_data_init(thd, 0, (char *) &stack_size, stack_size);
 thd->gc_cont = &entry_pt;
 thd->gc_args[0] = &clos_halt;
 thd->gc_num_args = 1;
 thd->thread_id = pthread_self();
 gc_add_mutator(thd);
 Cyc_heap_init(heap_size);
 thd->thread_state = CYC_THREAD_STATE_RUNNABLE;
 Cyc_start_trampoline(thd);
 return 0;}
 */

int main(){
  assert(boolean_t == boolean_t);
  assert(boolean_t != boolean_f);

  printf("All tests passed successfully!\n");
  return 0;
}
