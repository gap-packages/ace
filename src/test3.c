
/* test3.c, Colin Ramsay, 18 Apr 14 ********************************************

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


Various tests on the sizes of various types (along with some other interesting
stuff).  These can vary depending on the compiler, the compiler flags used, the
underlying CPU and the OS.   It is important to make sure that the system being 
used can support the required coset table, and that memory is used efficiently.

notes:
- The sizes are in terms of the 'char' type (usually 8 bits).
- Inttypes.h is used instead of stdint.h (for the PRIdX macros).
- This programme does not check how much actual memory the system has or how 
  much can be allocated to a process.  On unix sytems, the shell's "ulimit -a" 
  command can be used query "the resources available to the shell".  See the
  manual on how to set limits (incl. memory).

*******************************************************************************/

#include <inttypes.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main(void)
  {
  printf("\n");
  printf("The standard library types & sizes ...\n");
  printf("\n");

  /* The standard integer types ... */

  printf("         char = x%d\n", (int)sizeof(char));
  printf("        short = x%d\n", (int)sizeof(short));
  printf("          int = x%d\n", (int)sizeof(int));
  printf("         long = x%d\n", (int)sizeof(long));
  printf("    long long = x%d\n", (int)sizeof(long long));
  printf("\n");

  /* ptrdiff_t's an 'int' which can contain the max poss address (stddef.h) ...
     Memory allocation uses the size_t type (stdlib.h) ...
     The clock() routine uses the clock_t type (time.h) ...
     The time() routine uses the time_t type (time.h) ... */

  printf("    ptrdiff_t = x%d\n", (int)sizeof(ptrdiff_t));
  printf("       size_t = x%d\n", (int)sizeof(size_t));
  printf("      clock_t = x%d\n", (int)sizeof(clock_t));
  printf("       time_t = x%d\n", (int)sizeof(time_t));
  printf("\n");

  /* How many bits from a rand() call ... */

  printf("     RAND_MAX = %d\n", RAND_MAX);
  printf("\n");

  /* The standard floating point types ... */

  printf("        float = x%d\n", (int)sizeof(float));
  printf("       double = x%d\n", (int)sizeof(double));
  printf("  long double = x%d\n", (int)sizeof(long double));
  printf("\n");

  printf("The inttypes.h types & sizes ...\n");
  printf("\n");

  /* The fixed size integer types ... */

  printf("    int8_t = x%d\n", (int)sizeof(int8_t));
  printf("   int16_t = x%d\n", (int)sizeof(int16_t));
  printf("   int32_t = x%d\n", (int)sizeof(int32_t));
  printf("   int64_t = x%d\n", (int)sizeof(int64_t));
  printf("\n");

  printf("  intptr_t = x%d\n", (int)sizeof(intptr_t));
  printf("  intmax_t = x%d\n", (int)sizeof(intmax_t));
  printf("\n");

  /* The printf format string macros ... */

  printf("     PRId8 = %s\n", ""PRId8);
  printf("    PRId16 = %s\n", ""PRId16);
  printf("    PRId32 = %s\n", ""PRId32);
  printf("    PRId64 = %s\n", ""PRId64);
  printf("\n");

  exit(EXIT_SUCCESS);
  }

