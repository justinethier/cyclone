car2-dev status on x86_64

TBD. will either fail or take a long time to finish:
- compiler.scm - Never finishes compiling (during previous testing)
- earley.scm - should run, just may take awhile
- graphs.scm - seems like not working on 64 bit, GC memory usage is constant, with too many allocations
- mbrotZ.scm - No complex library, this should still fail
- pi.scm - Needs bignum support
- slatex.scm - Cyclone hangs during the CPS optimization phase

Fail:
Pass:
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
Not Tested yet:
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
