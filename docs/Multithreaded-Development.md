
TODO: documentation of how to do MT programming today using Cyclone.
- issues with stack objects
- concurrency primitives (mutex, cond) always safe to share

TODO: ideas for future improvement

- Declare shared objects?

 could have (declare-shared obj) or (with-shared-objects thunk)

 negative is this is an extra step that only cyclone users would have to do.
 a better solution only requires SRFI-18 and is portable

- Portable solution
 ???
 
 needs to ensure minimal performance impacts

Older Notes:

(A)

That's an excellent question and unfortunately there is no explicit answer in the GC write-up. This is an important consideration and a section should be added to discuss it. 

There is a note buried in the SRFI 18 documentation:

http://justinethier.github.io/cyclone/docs/api/srfi/18#cyc-minor-gc

(Cyc-minor-gc)
Trigger a minor garbage collection. This is potentially useful to evacuate all objects from a thread’s stack to the heap. An object must be moved to the heap before it can be safely used by more than one thread.

Basically, objects allocated on a thread's stack are private to that thread. To answer your question minor GC does not handle synchronization of other threads at all. It is up to the application code to ensure any objects are moved to the heap prior to their use by multiple threads. This is not a great answer but unfortunately that is where we are today.

One possible solution is to provide libraries that can ensure these guarantees - EG: a thread-safe queue, hash table, etc. That way the application programmer is freed of this burden (which is not trivial) though at the cost of some runtime performance. I am interested to hear your thoughts on the matter.



(B)

Agreed the problem is not a read/write data race but rather that minor GC needs to be synchronized. Your proposed change about getting a new stack is interesting but the problem is that we cannot change stacks because we are using the one provided by the C compiler. The only way to get a new one would be to malloc our own nursery area(s) and bump-allocate objects from there.

Ultimately if we ever move objects we run into the same thread synchronization problem, though. The real issue is relocating objects (moving them from stack to heap, IE nursery to second generation), and how to synchronize those relocations across threads. What does the literature have to say about that? I know one solution is read barriers but those are very expensive in terms of runtime performance costs. We could do what you suggest and coordinate minor GC across all threads at once but that seems expensive, especially since minor GC happens all the time (multiple times per second). My concern is that we would end up having to stop the world for an extended period of time.

Another interesting approach is if the compiler could perform flow-control analysis to find shared objects, in which case it could generate "safe" code. Unfortunately Cyclone only compiles one module at a time (as opposed to whole-program compilation) so the compiler would not be able to consider interactions across modules in this analysis.
