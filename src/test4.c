
/* test4.c, Colin Ramsay, 18 Apr 14 ********************************************

Copyright (c)  2014  Centre for Discrete Mathematics and Computing, 
                     The University of Queensland, Australia

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.


How does the clock_t type from clock() interact with CLOCKS_PER_SEC (cps) to
determine when the reported CPU time (processor time, not real time) rolls over.
On 'traditional' 32 bit machines, this was at ~72 min (32 bits and cps = 10^6 
=> 2^32/10^6 = 4294.967296 sec ~= 71m35s).  Depending as clock_t is 32/64 bits 
(and treated as signed/unsigned), and the value of cps (10^6 on unix boxes and 
10^3 on windows boxes?), there are many possibilities.  This programme dumps out
the accumulated CPU time every few seconds (depending on the limit on the 
spin-a-bit loop), to allow the value to be tracked (is it monotonic, does it 
roll through ~35m47s & ~71m35s OK).

The accumulated times are processed into floating-point values and dumped with a
precision of 1 uSec to enable the 'resolution' of the clock() value to be 
determined.  This is traditionally 10 mSec for unix/linux, but OSX seems to use
1 uSec and Windows 1 mSec.  The resolution does not affect the roll-over 
interval, and need not equal 1/cps.

notes:
- you need to kill or ctrl-C this programme to stop it
- the cast of ct to int64_t will sign extend if it's signed
- if you compile this with optimisation, the "for (i ..." loop may disappear
- the C standard allows the initial value of clock() to be arbitrary, but it's
  typically (close to) zero.

*******************************************************************************/

#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main(void)
  {
  int i,j;
  clock_t ct;

  printf("\n");
  printf("       clock_t = x%d\n", (int)sizeof(clock_t));
  printf("CLOCKS_PER_SEC = %d\n", (int)CLOCKS_PER_SEC);
  printf("\n");
  fflush(stdout);

  while (1)                                                   /* spin forever */
    {
    ct = clock();
    printf(" %11.6f  (0x%"PRIx64")\n", 
                              (double)ct/(double)CLOCKS_PER_SEC, (int64_t)(ct));
    fflush(stdout);
    for (i = 0; i < 1000000000; i++) { j += i*i;  j -= i; }     /* spin a bit */
    }

  printf("\n");                                             /* never get here */
  exit(EXIT_SUCCESS);
  }


