#include "cyclone/types.h"

void start_c_thread(gc_thread_data *thd);
object scm_call_no_gc(gc_thread_data *parent_thd, object fnc, object arg);
