
/**************************************************************************

        timtst.c - Colin Ramsay - 19 Jun 98

Tests the various clock/timing features available.  In all cases, although
the type clock_t is defined as a (signed) long, it seems to behave as if
it's a 32-bit unsigned integer.

          type/cpu/comp    CPS      Res   Range
  -----------------------------------------------------
  flute   sun4m/SPARC/gcc  1000000  10mS  0..71m35s
  ozone   IP27/R10000/cc   1000000  10mS  0..71m35s      ? 64-bit mode
  darter  PC/i586/gcc      100      10mS  0..248.5/497d

**************************************************************************/

#include <stdio.h>
#include <stdlib.h>

#include <sys/types.h>
#include <time.h>

void main(void)
  {
  int i,j,k;
  time_t t;
  clock_t c;

  system("date");
  fflush(stdout);

  while (1)
    {
    t = time(NULL);
    printf("%10lu    %s", (unsigned long)t, ctime(&t));

    c = clock();
    printf("%12lu    %lu:%lu    %4.2f\n", (unsigned long)c, 
           (unsigned long)c / (unsigned long)CLOCKS_PER_SEC, 
           (unsigned long)c % (unsigned long)CLOCKS_PER_SEC,
           (double)((unsigned long)c) / (double)CLOCKS_PER_SEC);

    fflush(stdout);

    i = j = k = 0;
    for (i = 0; i < 10000000; i++)
      {
      j = (i%15 - k) * (i%511 + k);
      k = 3 * j%(1024*1023);
      }
    }
  }

