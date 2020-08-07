#include "cyclone/types.h"

void start_c_thread(gc_thread_data *thd);
object c_trampoline(gc_thread_data *parent_thd, object fnc, object arg);
