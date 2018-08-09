[<img src="images/cyclone-logo-04-header.png" alt="cyclone-scheme">](http://github.com/justinethier/cyclone)

# Garbage Collection Using Lazy Sweeping

- [Introduction](#introduction)
- [Terms](#terms)
- [Marking Objects](#marking-objects)
  - [Tri-color Marking](#tri-color-marking)
  - [Requirements for Lazy Sweeping](#requirements-for-lazy-sweeping)
- [Allocation](#allocation)
- [Sweeping](#sweeping)
- [Collector Thread](#collector-thread)
- [Starting a Major Collection](#starting-a-major-collection)
- [Results](#results)
- [Conclusion](#conclusion)
- [References](#references)

# Introduction

Cyclone uses a concurrent mark-sweep garbage collection algorithm [described in detail here](Garbage-Collector.md). But there are still some basic improvements to the mark-sweep algorithm that can be made. One such improvement suggested by the [Garbage Collection Handbook](#references) is lazy sweeping. 

The basic idea is that instead of having the collector thread sweep the entire heap at once when tracing is finished, each mutator thread will sweep its own heap incrementally as part of allocation. When no more free space is available to meet a request the allocator will check to see if there are unswept heap pages, and if so, the mutator will pick one and sweep it to free up space. This amortizes the cost of sweeping.

The main goal of this process is to improve performance through:

- Better Locality - Heap slots tend to be used soon after they are swept and sweep only needs to visit a small part of the heap. This allows programs to make better use of the processor cache.
- Thread-Local Data - There is no need to lock the heap for allocation or sweeping since both operations are performed by the same thread.
- Reduced Complexity - According to [[1]](#references) the algorithmic complexity of mark-sweep is reduced to be proportional to the size of the live data in the heap instead of the whole heap, similar to a copying collector. Lazy sweeping will perform best when most of the heap is empty.

In the latest version of Cyclone Scheme (0.9) we have modified major GC to use lazy sweeping. We discuss the changes required to the existing GC, present results, and consider next steps.

# Terms

- Collector - A thread running the garbage collection code. The collector is responsible for coordinating and performing most of the work for major garbage collections.
- GC - Garbage collector.
- Heap - A section of dynamic memory managed by the garbage collector. The heap is not stored as a single contiguous block but rather is broken up into a series of pages.
- Mutator - A thread running user (or "application") code; there may be more than one mutator running concurrently.
- Root - During tracing the collector uses these objects as the starting point to find all reachable data.
- Sweep - A phase of garbage collection where the heap - either the whole heap or a subset - is scanned and any unused slots are made available for new allocations.
- Tracing - A phase of garbage collection that visits and marks all live objects on the heap. This is done by starting from a set of "root" objects and iteratively following references to child objects.

# Marking Objects

## Tri-color Marking

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

## Requirements for Lazy Sweeping

The current set of colors is insufficient for lazy sweeping because parts of the heap may not be swept during a collection cycle. Thus an object that is really garbage could accidentally be assigned the black color.

For example, suppose a heap page consists entirely of white objects after a GC is finished. All of the objects are garbage and would be freed if the page is swept. However if this page is not swept before the next collection starts, the collector will swap the values of white/black and during the subsequent cycle all of the objects will appear as if they have the black color. Thus a sweep during this most recent GC cycle would not be able to free any of the objects!

The solution is to add a new color (purple) to indicate garbage objects on the heap. Garbage can then be swept while the collector is busy doing other work such as mark/trace. In order to account for multiple generations of objects the object colors are incremented each cycle instead of being swapped. For example, the collector starts in the following state:

    static unsigned char gc_color_mark = 5;   // Black, is swapped during GC
    static unsigned char gc_color_clear = 3;  // White, is swapped during GC
    static unsigned char gc_color_purple = 1;  // There are many "shades" of purple, this is the most recent one

We can assign a new purple color after tracing is finished. At this point the clear color and the purple color are (essentially) the same, and any new objects are allocated using the mark color. When GC starts back up, the clear and mark colors are each incremented by 2:

    // We now increment both so that clear becomes the old mark color and a
    // new value is used for the mark color. The old clear color becomes
    // purple, indicating any of these objects are garbage
    ck_pr_add_8(&gc_color_purple, 2);
    ck_pr_add_8(&gc_color_clear, 2);
    ck_pr_add_8(&gc_color_mark, 2);

So we now have purple (assigned the previous clear color), clear (assigned the previous mark color), and mark (assigned a new number). All of these numbers must be odd so they will never conflict with the red or blue colors. Effectively any odd numbered colors not part of this set represent other "shades" of purple.

# Allocation

The main allocation function takes a fast or slow path depending upon whether a free slot is found on the current heap page. 

The logic in simplified form is:

    result = try_alloc();
    if (result)
      return result;

    result = try_alloc_slow();
    if (result) 
      return result;

    grow_heap(); // malloc more heap space
    result = try_alloc_slow();
    if (result) 
      return result;

    out_of_memory_error();
  
A heap page uses a "free list" of available slots to quickly find the next available slot. The `try_alloc` function simply finds the first slot on the free list and returns it, or `NULL` if there is no free slot.

On the other hand, `try_alloc_slow` has to do more work to find the next available heap page, sweep it, and then call `try_alloc` to perform an allocation.

# Sweeping

Sweep walks an entire heap page, freeing all unused slots along the way. The algorithm itself is mostly unchanged except that to identify an unused object we need to check for two colors:

- Objects that are either newly-allocated or recently traced are given the allocation color; we need to keep them.
- If the collector is currently tracing, objects not traced yet will have the trace/clear color. We need to keep any of those to make sure the collector has a chance to trace the entire heap.

      if (mark(p) != thd->gc_alloc_color && 
          mark(p) != thd->gc_trace_color) {
        ... // Free slot p
      }

# Collector Thread

As well as coordinating major GC the main job of the collector thread is now just tracing. 

During this phase the collector visits all live objects and marks them as being in use. Since these objects are stored all across the heap the tracing algorithm cannot take advantage of object locality and tends to demonstrate unusual memory access patterns, leading to inefficient use of the processor cache and poor performance. This makes tracing an excellent task to be done in parallel with the mutator threads so it does not slow down application code.

Note that during tracing some synchronization is required between the collector and the mutator threads. When an object is changed (EG via: `set!`, `vector-set!`, etc) the mutator needs to add this object to the mark stack, which requires a mutex lock to safely update shared resources.

# Starting a Major Collection

The existing GC tracked free space and would start a major GC once the amount of available heap memory was below a threshold. We continue to use the same strategy with lazy sweeping but during a slow allocation the mutator also checks how many heap pages are still free. If that number is too low we trigger a new GC cycle.

# Results

A benchmark suite [[3]](#references) was used to compare performance between the previous version of Cyclone (0.8.1) and the new version with lazy sweeping.

The following table lists the differences in elapsed time (seconds) between versions:

Benchmark | Baseline | Lazy Sweeping | Improvement
--------- | -------- | ------------- | ------------
browse | 25.34 | 22.21 | 12.35%
deriv | 17.17 | 10.83 | 36.90%
destruc | 38.00 | 30.94 | 18.59%
diviter | 8.57 | 6.05 | 29.35%
divrec | 17.98 | 14.49 | 19.46%
puzzle | 46.97 | 44.97 | 4.25%
triangl | 26.20 | 25.35 | 3.23%
tak | 18.73 | 18.36 | 1.99%
takl | 14.42 | 11.30 | 21.64%
ntakl | 15.32 | 11.22 | 26.74%
cpstak | 21.09 | 20.92 | 0.80%
ctak | 2.78 | 2.77 | 0.28%
fib | 41.26 | 41.05 | 0.51%
fibc | 3.52 | 3.47 | 1.37%
fibfp | 9.56 | 9.57 | -0.12%
sum | 30.28 | 30.29 | -0.02%
sumfp | 11.55 | 11.53 | 0.23%
fft | 21.19 | 17.25 | 18.57%
mbrot | 16.84 | 15.27 | 9.34%
mbrotZ | 23.35 | 22.88 | 2.01%
nucleic | 8.29 | 7.91 | 4.56%
pi | 0.13 | 0.13 | 1.90%
pnpoly | 43.64 | 41.80 | 4.22%
ray | 9.13 | 9.12 | 0.05%
simplex | 53.26 | 42.60 | 20.02%
ack | 75.78 | 50.64 | 33.18%
array1 | 30.84 | 30.65 | 0.60%
string | 0.28 | 0.26 | 6.91%
sum1 | 1.01 | 1.00 | 1.23%
cat | 22.05 | 22.42 | -1.69%
tail | 1.04 | 0.99 | 4.56%
wc | 14.46 | 14.75 | -2.07%
read1 | 3.61 | 3.20 | 11.31%
conform | 40.67 | 34.00 | 16.40%
dynamic | 33.84 | 27.61 | 18.41%
earley | 31.49 | 26.84 | 14.78%
graphs | 64.84 | 55.22 | 14.84%
lattice | 84.57 | 68.93 | 18.50%
matrix | 61.07 | 48.46 | 20.64%
maze | 23.02 | 18.46 | 19.79%
mazefun | 23.73 | 20.74 | 12.61%
nqueens | 47.92 | 45.18 | 5.71%
paraffins | 15.21 | 10.76 | 29.28%
parsing | 39.50 | 38.55 | 2.41%
peval | 32.11 | 27.72 | 13.67%
primes | 18.79 | 12.83 | 31.74%
quicksort | 56.64 | 48.13 | 15.03%
scheme | 23.32 | 21.39 | 8.30%
slatex | 9.74 | 8.14 | 16.37%
chudnovsky | 0.09 | 0.09 | 1.79%
nboyer | 13.80 | 11.84 | 14.24%
sboyer | 11.90 | 12.09 | -1.60%
gcbench | 37.12 | 32.37 | 12.79%
mperm | 49.94 | 39.97 | 19.95%
equal | 0.74 | 0.70 | 4.43%
bv2string | 7.54 | 7.62 | -1.00%

This data is illustrated in the following chart:

<img src="images/benchmarks/lazy-sweep-benchmark-times.png" alt="Chart of Results">

Here is an overall summary:

Statistic | Benchmark | Result
--------- | --------- | ------
Overall Improvement | N/A |  13.36%
Average Speedup     | N/A |  10.74%
Maximum Speedup     | deriv |  36.90%
Minimum Speedup     | wc |  -2.07%

Overall we achieve an average speedup of 10.74% with lazy sweeping, though there are a wide range of performance impacts across the whole benchmark suite. 

Those benchmarks with the biggest speedups are likely those that are generating the most garbage. For example `ack` frequently invokes GC and most of the heap is freed during each GC cycle - this benchmark benefits greatly from lazy sweeping. Alternatively `wc` - which did not realize a speedup - spends most of its time running in a tight loop, invokes GC infrequently, and after a GC cycle there are many live objects left on the heap. 

# Conclusion

By all accounts lazy sweeping is a great win for Cyclone and has exceeded performance expectations. Though there is a slight performance overhead that affects some programs the overall performance improvement across a wide range of programs more than compensates. 

Lazy sweeping is a large-scale change that took a few months to fully-integrate into Cyclone. Although there is work involved to re-stabilize the GC code after such a significant change, this does open up the possibility to experiment with future larger-scale GC optimizations.

# References

1. [The Garbage Collection Handbook: The Art of Automatic Memory Management](http://gchandbook.org/), by Antony Hosking, Eliot Moss, and Richard Jones
2. [Introducing Riptide: WebKit's Retreating Wavefront Concurrent Garbage Collector](https://webkit.org/blog/7122/introducing-riptide-webkits-retreating-wavefront-concurrent-garbage-collector/), by Filip Pizlo
3. [Scheme Benchmarks](https://ecraven.github.io/r7rs-benchmarks/), by [ecraven](https://github.com/ecraven)
4. [The Ramsey sweep](http://people.csail.mit.edu/gregs/ll1-discuss-archive-html/msg00761.html), by Olin Shivers
5. [The Cyclone Scheme Garbage Collector](Garbage-Collector.md), by Justin Ethier
