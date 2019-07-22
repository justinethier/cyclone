This file is the attempted start of a best practices guide for the C interface... not much more than notes at this point, though.

# Use built-in macros to create objects

It is best for application code to avoid assigning to internal structures (such as hdr) if we can help it, just in case any of that were to change in the future. For example, hdr.immutable is a recent addition. Also, the C code is compiled with optimizations so most likely the generated assembly will avoid a double-assignment:

       alloca_pair(pl, ps, NULL);
       //       object pl = alloca(sizeof(pair_type));
       //       ((list) pl)->hdr.mark = gc_color_red;
       //       ((list) pl)->hdr.grayed = 0;
       //       ((list) pl)->hdr.immutable = 0;
       //       ((list) pl)->tag = pair_tag;
       //       ((list) pl)->pair_car = ps;

