
/* test0a.c, Colin Ramsay, 19 Apr 14 *******************************************

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

This is similar to test4.c, except that it uses the ACE level 0 (hence the 
"testZEROa.c" name) al0_clock() & al0_diff() routines, and drives them in the
same way as ACE's messaging feature does.  Thus, it tests how these routines
actually perform, and helps us decide if we made the 'correct' choice of CLK32 
or CLK64 based on the output of test4.

notes:
- We've changed the details of the loop and added the "if (i==j) ...", since
  optimisation (which is usually active for ACE & ACE dependents) tends to make
  the loop much faster and/or blow it away.  You may need to tinker with this to
  get it working as desired on your machine.
- The first delta value may be longer than the subsequent ones.

*******************************************************************************/

#include "al0.h"

#include <time.h>

int main(void)
  {
  int64_t i = 0, j = 0;
  double init, t1, t2, delta, curr = 0.0;

  printf("\n");
  printf("       clock_t = x%d\n", (int)sizeof(clock_t));
  printf("CLOCKS_PER_SEC = %d\n", (int)CLOCKS_PER_SEC);
#if defined (CLK32)
  printf("           CLK = 32\n");
#elif defined (CLK64)
  printf("           CLK = 64\n");
#else
  printf("           CLK = unknown\n");
#endif

  printf("\n");
  init = al0_clock();
  printf("start = %.6f\n", init);
  fflush(stdout);

  printf("\n");
  t1 = init;

  while (1)                                                   /* spin forever */
    {
    for (i = 0; i < 5000000000; i++) { j += i*i;  j -= i; }     /* spin a bit */
    if (i == j) { t2 = al0_clock(); }                /* 'fools' the optimiser */

    t2 = al0_clock();
    delta = al0_diff(t1, t2);
    curr += delta;

    printf("delta = %.6f", delta);
    printf("    total = %.6f", curr);
    printf("    overall = %.6f\n", al0_diff(init, t2));

    t1 = t2;
    fflush(stdout);
    }

  printf("\n");                                             /* never get here */
  exit(EXIT_SUCCESS);
  }

