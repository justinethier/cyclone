car2-dev status on x86_64

TBD. will either fail or take a long time to finish:
- compiler.scm - Never finishes compiling (during previous testing)
- earley.scm - should run, just may take awhile
  I think what is happening is that there are a lot of requests for objects on the REST heap, most of
  size 96 but some of 128 (or maybe larger?). this causes heap fragmentation and over time makes it
  take a long time to find larger free chunks (> 96) on this heap.
  need to address the fragmentation issue somehow. can either kick the can down the road and add a 96 byte
  heap, or figure out a more general solution (have N fixed sized heaps, allocate arrays in fixed sized chunks, ??)

- mbrotZ.scm - No complex library, this should still fail
- pi.scm - Needs bignum support
- slatex.scm - Cyclone hangs during the CPS optimization phase

Fail:
Pass:
- graphs.scm
Not Tested yet:
- ack.scm
- array1.scm
- browse.scm
- bv2string.scm
- cat.scm
- conform.scm
- cpstak.scm
- ctak.scm
- deriv.scm
- destruc.scm
- diviter.scm
- dynamic.scm
- divrec.scm
- equal.scm
- fft.scm
- fibc.scm
- fibfp.scm
- fib.scm
- gcbench.scm
- lattice.scm
- matrix.scm
- mazefun.scm
- maze.scm
- mbrot.scm
- mperm.scm
- nboyer.scm
- nqueens.scm
- ntakl.scm
- nucleic.scm
- paraffins.scm
- parsing.scm
- peval.scm
- pnpoly.scm
- primes.scm
- puzzle.scm
- quicksort.scm
- ray.scm
- sboyer.scm
- string.scm
- read1.scm
- scheme.scm
- simplex.scm
- sum1.scm
- sumfp.scm
- sum.scm
- tail.scm
- takl.scm
- tak.scm
- triangl.scm
- wc.scm
