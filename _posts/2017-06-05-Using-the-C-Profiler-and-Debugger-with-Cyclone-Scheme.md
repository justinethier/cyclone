---
layout: post
title: Using the C Profiler and Debugger with Cyclone Scheme
excerpt: While going through a new run of the R7RS benchmarks from Larceny, I noticed Cyclone performed significantly worse than other schemes on the tail benchmark.
---

While going through a new run of the [R7RS benchmarks from Larceny](http://www.larcenists.org/benchmarksGenuineR7Linux.html), I noticed Cyclone performed significantly worse than other schemes on the tail benchmark. Certainly when testing locally the results are disappointing:

    [justin@justin-pc r7rs-benchmarks]$ ./bench cyclone tail
    
    Testing tail under Cyclone
    Including postlude /home/justin/Documents/r7rs-benchmarks/src/Cyclone-postlude.scm
    Compiling...
    Running...
    Running tail:25
    Elapsed time: 32.261715 seconds (32) for tail:25
    +!CSVLINE!+cyclone-0.5.1,tail:25,32.261715
    
    real    0m32.379s
    user    0m31.783s
    sys     0m0.513s

One of the easiest things to do is run a profiler on the code to figure out what is going on. This lets us see if there is something in the runtime or compiled code that is dominating the execution time, and possibly slowing things down. This isn't a catch-all - for example, it can't show us if a compiler optimization is needed. But it helps paint a picture of what is going on.

A compiled Cyclone program is just a regular C program so we can use the standard GNU tools for profiling and debugging.

To get started we change `Makefile.config` in cyclone-bootstrap to enable profiling. The `-O2` option in the lines below are replaced with `-g -pg`:

    CFLAGS       ?= -g -pg -fPIC -rdynamic -Wall -Iinclude -L.
    COMP_CFLAGS  ?= -g -pg -fPIC -rdynamic -Wall -I$(PREFIX)/include -L$(PREFIX)/lib

Then Cyclone must be rebuilt:

    [justin@justin-pc cyclone-bootstrap]$ sudo make clean ; ./install.sh

Once this is done a `gmon.out` file will be generated each time Cyclone or a compiled Cyclone program is executed. This can be used to create a detailed analysis of what the program is doing at runtime.

Now we perform set up for running the `tail` benchmark directly:

    [justin@justin-pc r7rs-benchmarks]$ cd /tmp/larcenous/Cyclone/
    [justin@justin-pc Cyclone]$ cp -r ~/Documents/r7rs-benchmarks/inputs/ .
    [justin@justin-pc Cyclone]$ mkdir outputs

And recompile `tail.scm` to get a version with profiling. We then run it to generate a `gmon.out` file:

    [justin@justin-pc Cyclone]$ cyclone  tail.scm
    [justin@justin-pc Cyclone]$ ./tail < inputs/tail.input

Then we run `gprof` to create a report:

    [justin@justin-pc Cyclone]$ ls
    gmon.out  inputs  outputs  tail  tail.c  tail.o  tail.scm
    [justin@justin-pc Cyclone]$ gprof ./tail gmon.out > report.txt

Let's examine the start of `report.txt` to see the functions that are taking up the most of the program's runtime: 

    Flat profile:
    
    Each sample counts as 0.01 seconds.
      %   cumulative   self              self     total
     time   seconds   seconds    calls   s/call   s/call  name
     99.19     61.09    61.09  2331598     0.00     0.00  Cyc_length
      0.10     61.15     0.06    11221     0.00     0.00  gc_minor
      0.08     61.20     0.05   777200     0.00     0.00  __lambda_3
      0.08     61.25     0.05   777097     0.00     0.00  __lambda_300
      0.03     61.27     0.02  4664280     0.00     0.00  Cyc_st_add
      0.03     61.29     0.02  1562923     0.00     0.00  gc_thr_add_to_move_buffer
      0.03     61.31     0.02   777572     0.00     0.00  __lambda_278
      0.03     61.33     0.02   777178     0.00     0.00  dispatch
      0.03     61.35     0.02   777147     0.00     0.00  __lambda_297
      0.03     61.37     0.02   777135     0.00     0.00  __lambda_368
      0.03     61.39     0.02       59     0.00     0.00  gc_empty_collector_stack
      0.02     61.41     0.02  5441402     0.00     0.00  Cyc_is_pair
      0.02     61.42     0.02                             ck_pr_cas_char
      0.02     61.43     0.01  3109193     0.00     0.00  Cyc_is_null
      0.02     61.44     0.01  3109170     0.00     0.00  Cyc_car
    
Well that's interesting, `tail` is spending all of its time computing `Cyc_length`.

Again, Cyclone compiles programs to C, so we can use `gdb` to debug them and figure out how `Cyc_length` is being called. 

First we need to know what inputs to use:

    [justin@justin-pc Cyclone]$ cat inputs/tail.input
    25
    "inputs/bib"
    "outputs/tail.output"
    ignored

To run gdb:

    [justin@justin-pc Cyclone]$ gdb ./tail
    GNU gdb (GDB) 7.12.1
    Copyright (C) 2017 Free Software Foundation, Inc.
    License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>
    This is free software: you are free to change and redistribute it.
    There is NO WARRANTY, to the extent permitted by law.  Type "show copying"
    and "show warranty" for details.
    This GDB was configured as "x86_64-pc-linux-gnu".
    Type "show configuration" for configuration details.
    For bug reporting instructions, please see:
    <http://www.gnu.org/software/gdb/bugs/>.
    Find the GDB manual and other documentation resources online at:
    <http://www.gnu.org/software/gdb/documentation/>.
    For help, type "help".
    Type "apropos word" to search for commands related to "word"...
    Reading symbols from ./tail...done.
    (gdb) run
    Starting program: /tmp/larcenous/Cyclone/tail
    [Thread debugging using libthread_db enabled]
    Using host libthread_db library "/usr/lib/libthread_db.so.1".
    [New Thread 0x7ffff6031700 (LWP 1850)]
    25
    "inputs/bib"
    "outputs/tail.output"
    ^C
    Thread 1 "tail" received signal SIGINT, Interrupt.
    0x00007ffff6fd44ed in read () from /usr/lib/libc.so.6
    (gdb) break Cyc_length
    Breakpoint 1 at 0x53b145: file runtime.c, line 1713.
    (gdb) c
    Continuing.
    ignored

After continuing a few times, the code breaks here:

    #0  Cyc_length (data=0x7cd4e0, l=0x7ffffffbc660) at runtime.c:1713
    #1  0x00000000004af96b in __lambda_368 (data=0x7cd4e0, argc=3, _=0x7ffff6635340, k_734906=0x7ffff63331c0, f_731928=0x7ffffffb2dd0, lis1_731927=0x7ffffffbc660,
        lists_731926_raw=0x7ca420 <Cyc_void_symbol>) at scheme/base.c:22680
    #2  0x00000000004afde1 in __lambda_367 (data=0x7cd4e0, argc=1, self_736727=0x7ffffff9d4c0, r_734921=0x7ca420 <Cyc_void_symbol>) at scheme/base.c:22703
    #3  0x00000000004b7aa7 in __lambda_299 (data=0x7cd4e0, argc=3, _=0x7ffff66350e0, k_735061=0x7ffffff9d4c0, char_731994=0x2a, port_731993_raw=0x7ffff6333200) at scheme/base.c:23706
    #4  0x000000000055cf5e in do_dispatch (data=0x7cd4e0, argc=3, func=0x4b7523 <__lambda_299>, clo=0x7ffff66350e0, b=0x7ffffff9ccf0) at dispatch.c:6

Opening the source code for `scheme/base.c` you can see the code breaks in the `for-each` function:

    static void __lambda_368(void *data, int argc, closure _,object k_734906, object f_731928, object lis1_731927, object lists_731926_raw, ...) {
    load_varargs(lists_731926, lists_731926_raw, argc - 3);
    Cyc_st_add(data, "scheme/base.sld:for-each");
    if( (boolean_f != Cyc_is_null(lis1_731927)) ){

And in `scheme/base.sld` you can see where `length` is being called:

    (define (for-each f lis1 . lists)
      (if (not (null? lis1))
        (if (pair? lists)
          (let recur ((lists (cons lis1 lists)))
            (receive (cars cdrs) (%cars+cdrs lists)
              (if (pair? cars)
                  (begin
                    (apply f cars)
                    (recur cdrs)))))
          ;; Fast path.
          (if (eq? 1 (length lis1))
            (f (car lis1))
            (begin (f (car lis1))
                   (for-each f (cdr lis1)))))))

The code can be simplified to make it more obvious what is going on:

    (define (for-each f lis1 . lists)
      (if (not (null? lis1))
          (if (eq? 1 (length lis1))
            (f (car lis1))
            (begin (f (car lis1))
                   (for-each f (cdr lis1))))))

Basically on every iteration of `for-each` the code is calling `length` to see if `f` can be called directly. Well, that's not good - the main `for-each` loop itself has a [time complexity of `O(n)`](https://en.wikipedia.org/wiki/Big_O_notation). The runtime depends directly on the length of `lis1`. But each time `length` is called it must examine the entire contents of `lis1`, which is another `O(n)` operation. Combined with the outer loop this raises the overall time complexity to `O(n^2)` - which can really add up for large values of `n`.

This reminds me of [an old article from Joel Spolsky](http://global.joelonsoftware.com/English/Articles/Interviewing.html) that talks about the same issue with respect to strings:

> Is their function fast? Look at how many times they call strlen. I've seen O(n^2) algorithms for strrev when it should be O(n), because they are calling strlen again and again in a loop.

The solution is to check directly for null, instead of scanning the whole string:

    (if (null? (cdr lis1))
      (f (car lis1))
      (begin (f (car lis1))

After rebuilding with this fix we can re-run the tail benchmark:

    [justin@justin-pc r7rs-benchmarks]$ ./bench cyclone tail
    
    Testing tail under Cyclone
    Including postlude /home/justin/Documents/r7rs-benchmarks/src/Cyclone-postlude.scm
    Compiling...
    Running...
    Running tail:25
    Elapsed time: 0.72314 seconds (1) for tail:25
    +!CSVLINE!+cyclone-0.5.1,tail:25,0.72314
    
    real    0m0.729s
    user    0m0.540s
    sys     0m0.187s 

Whoa! Remember how the older code took over 32 seconds to finish? Now it finishes in less than a second. Not bad.
