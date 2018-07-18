
Goals
Performance

Ideally want to improve performance with this approach. Hopefully it improves cache locality since we will only sweep a little bit and then will use the newly-sweeped portion for allocations.

Can we minimize the size of gc_try_alloc to allow inlining of this heavily-used function?

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


Object Coloring

Plan B - Do not force any sweeps
The plan is to add a new color (purple) that will be used to indicate garbage objects on the heap. That way we can sweep while the collector is busy doing other work such as mark/trace.
We can assign a new purple color after tracing is finished. At this point the clear color and the purple color are (essentially) the same, and any new objects are allocated using the mark color. When gc starts back up, the clear and mark colors are each incremented by 2. So we would then have purple (assigned the previous clear color), clear (assigned the previous mark color), and mark (assigned a new number). All of these numbers must be odd so they will never conflict with the red (stack) color or the blue color (though that one is presently unused).

With this strategy we could use a global for the purple (garbage) color. But do we even need to track purple at all? I don’t think so, all of the shades of purple are implicit - they are just the odd-numbered colors that are not the mark or (sometimes) the clear color.

Each heap will have to maintain a “full” bit. This is necessary to avoid wasted work of re-examining heaps that we already know to be full.
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

This makes sweep slightly more expensive because now to determine if an object is garbage it needs to make sure it is not using the allocation color or the white color (remember, we only want to free purple objects, but that color value changes each GC cycle). I think this will be acceptable though because it allows us to only sweep when necessary (IE, a heap does not need to be swept at all during a GC cycle if we don’t need the space) and when we sweep we will only iterate over one heap object.


Free Space Tracking
TBD


Notes:
If we now have two alloc colors:
One is the existing alloc color
The other is the previous clear color, when we cooperate. We can’t free objects of this color because the collector is tracing over them
After tracing is finished, we would want to remove this color because at that point objects that still have it need to become garbage

