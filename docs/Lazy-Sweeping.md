One of the basic improvements to the mark-sweep algorithm suggested by the Garbage Collection Handbook is lazy sweeping. 

With this approach instead of waiting until tracing is finished and having the collector thread sweep the entire heap at once, each thread will sweep its own heaps as part of the allcation. When no more free space is available to meet a request the allocator will check to see if there are unswept heap pages. If so, the next one will be selected, the mutator will sweep it to free up space, and the new page will be used for allocation. If insufficient space is available then a major collection is triggered.

The main goal of this process is to improve performance:

- better locality - heap slots tend to be used after they are swept
- thread-local data - there is no need to lock the heap for allocation or sweeping since both operations are now performed by each mutator thread
- accorinding to the GC handbook, it reduces the algorithmic complexity of mark-sweep to be proportional to the size of the live data in the heap, similar to a copying collector


Older notes:

Original notes:
Should consider lazy sweeping, riptide does this. Perhaps it would improve cache locality when sweeping fixed-size heaps
Pseudocode on page 25 of the GC Handbook, but we need to adapt it for cyclone
Need to pay attention to marking, probably would want to increment both clear/mark values and explicitly check for the mark value now
Look at heap_lock, think we only use that during alloc or sweep
One potential issue is gc_thread_data_free which uses the heap_lock to block the main mutator thread while merging in the dead thread’s heap. Will need to figure out another way of doing this locking/merge
Goal is to remove heap locks and cached heap size atomics (just compute directly)
Questions
How does alloc know when to call sweep, without either locking the mutator to set bits, or having alloc need to make a check each time?
Maybe we wait for heap page to be empty, then add the check as part of the slow case
How do we handle heap growth and GC initiation when we are doing partial, lazy sweeps?


# Object Coloring with the old collector

Before this change, an object could be marked using any of the following colors to indicate the status of its memory:

  - Blue - Unallocated memory.
  - Red - An object on the stack.
  - White - Heap memory that has not been scanned by the collector. 
  - Gray - Objects marked by the collector that may still have child objects that must be marked.
  - Black - Objects marked by the collector whose immediate child objects have also been marked.

Only objects marked as white, gray, or black participate in major collections:

- White objects are freed during the sweep state. White is sometimes also referred to as the clear color.
- Gray is never explicitly assigned to an object. Instead, objects are grayed by being added to lists of gray objects awaiting marking. This improves performance by avoiding repeated passes over the heap to search for gray objects.
- Black objects survive the collection cycle. Black is sometimes referred to as the mark color as live objects are ultimately marked black.

After a major GC is completed the collector thread swaps the values of the black and white color. This simple optimization avoids having to revisit any objects while allowing the next cycle to start with a fresh set of white objects.

# Object Coloring with Lazy Sweeping

The current set of colors is insufficient for lazy sweeping because parts of the heap may not be swept during a collection cycle.

(an example might help here)

Thus an object that is really garbage could accidentally be assigned the black color.

The solution is to add a new color (purple) to indicate garbage objects on the heap. Garbage can then be swept while the collector is busy doing other work such as mark/trace. In order to account for multiple generations of objects the object colors are incremented each cycle instead of being swapped. For example, the collector starts in the following state:

    static unsigned char gc_color_mark = 5;   // Black, is swapped during GC
    static unsigned char gc_color_clear = 3;  // White, is swapped during GC
    static unsigned char gc_color_purple = 1;  // There are many "shades" of purple, this is the most recent one

We can assign a new purple color after tracing is finished. At this point the clear color and the purple color are (essentially) the same, and any new objects are allocated using the mark color. When gc starts back up, the clear and mark colors are each incremented by 2:

    // We now increment both so that clear becomes the old mark color and a
    // new value is used for the mark color. The old clear color becomes
    // purple, indicating any of these objects are garbage
    ck_pr_add_8(&gc_color_purple, 2);
    ck_pr_add_8(&gc_color_clear, 2);
    ck_pr_add_8(&gc_color_mark, 2);

So we would then have purple (assigned the previous clear color), clear (assigned the previous mark color), and mark (assigned a new number). All of these numbers must be odd so they will never conflict with the red (stack) color or the blue color (though that one is presently unused).

(In this manner there is a purple color representing the current set of garbage.)
Effectively any odd numbered mark colors not part of this set represent other "shades" of purple.

(TODO: graphic here)

Notes:
If we now have two alloc colors:
One is the existing alloc color
The other is the previous clear color, when we cooperate. We can't free objects of this color because the collector is tracing over them
After tracing is finished, we would want to remove this color because at that point objects that still have it need to become garbage
Globals (collector? who sets these?):

Mutator data:
  // Data needed for heap GC
  unsigned char gc_alloc_color;
  unsigned char gc_trace_color;
  uint8_t gc_done_tracing;
  int gc_status;
  // Lazy-sweep related data
  int free_size; // Amount of heap data that is free
  unsigned char is_full; // Determine if the heap is full
  unsigned char cached_free_size_status;


# Allocation

TODO: discuss fast path (slot on current heap page) and slow path (page full, need to find another one)

(below about selecting next heap page)

Each heap will have to maintain a full bit. This is necessary to avoid wasted work of re-examining heaps that we already know to be full.
Bit is set by the allocate function when no more allocations are possible
Bit is cleared by the collector after tracing is complete
Would be better if the mutator could do it to avoid contention
We could cheat and set a flag on the thread that will be examined during cooperation. When set, the mutator goes through all of its heaps and removes the full bit. Cooperation happens frequently so the update would be timely. Does add one additional comparison per cooperation, but that is not significant. In any case, this is also a convenient time to recompute the amount of free space in each heap.
TODO: how will sweep compute (and store) amount of free space per heap?
Guarantees each heap block is only used once per collection cycle

Growing the heap - may need to iterate to the end since we are not necessarily there when we run out of heap space. IE, we do not move the mutator back to the first heap page anymore after a sweep. gc_grow already does this, I think we are OK.

To avoid having to force any sweeps, each thread will maintain two colors that are “safe” from sweeping:
The allocation color (already present)
The white color (if we are tracing, otherwise this is also the allocation color)
Initialize it to the same as the allocation color
We want to assign this during cooperation, in preparation for tracing. This can be done using the existing code (note there are 2 places, in case collector cooperates on behalf of a mutator). Actually, during cooperation this value can remain unchanged since it is already assigned properly (IE, it is the white color).
After tracing is finished, we want to assign white color to the same value as the new allocation color. gc_collector_sweep already loops over all mutators. We can still do this and just atomically update the second alloc color to allow it to be freed again (IE, just set it to the mark color)

# Sweeping

      // Use the object's mark to determine if we keep it. 
      // Need to check for both colors because:
      // - Objects that are either newly-allocated or recently traced are given 
      //   the alloc color, and we need to keep them.
      // - If the collector is currently tracing, objects not traced yet will 
      //   have the trace/clear color. We need to keep any of those to make sure
      //   the collector has a chance to trace the entire heap.
      if (//mark(p) != markColor &&
          mark(p) != thd->gc_alloc_color && 
          mark(p) != thd->gc_trace_color) { //gc_color_clear) 
This makes sweep slightly more expensive because now to determine if an object is garbage it needs to make sure it is not using the allocation color or the white color (remember, we only want to free purple objects, but that color value changes each GC cycle). I think this will be acceptable though because it allows us to only sweep when necessary (IE, a heap does not need to be swept at all during a GC cycle if we don’t need the space) and when we sweep we will only iterate over one heap object.


# Starting a Major Collection

The existing GC tracked free space and would start a major GC once the amount of available heap memory was below a threshold. We continue to use the same strategy with lazy sweeping, but during a slow allocation the mutators also check how many heap pages are still free. If that number is too low we trigger a new GC cycle.


# Results

TODO: compare performance of new GC to old one, perhaps with benchmarks (compare 0.8.1 release with current 0.9 branch)

# Conclusion

wrap this up...

# References

- Garbage Collection Handbook
- riptide (see blog post)


