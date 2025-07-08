
/* test7.c, Colin Ramsay, 18 Apr 14 ********************************************

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


This is a version of test6.c for those systems which don't support getrusage().
It uses clock() instead (ie, as ACE does).  Note that clock() is in the C99 
standard, whereas getrusage() is not.  Both are in the POSIX standard.

notes:
- we assume that the running times are short enough so that there's no clock 
  rollover, and that clock() always returns a positive value
- test6 typically has a higher resolution than test7
- test6 returns "user" & "system" times (similar to the unix "time" command line
  utility), whereas test7 returns simply a "processor" time
- the relationship between the test6 & test7 times varies, and the usual caveats
  regarding measuring running times of executables apply

*******************************************************************************/

#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#include <inttypes.h>

/* #define SIZE 10000000 */
/* #define SIZE 25000000 */
   #define SIZE 100000000 

ptrdiff_t p0, p1;                 /* 1st & last elements of the array */
int64_t j;                        /* result of the 'computation' */

/* common code 1/2/4/8 ... */

#define CCODE1  \
  a = malloc(SIZE*s);  \
  p0 = (ptrdiff_t)(&a[0]);  \
  p1 = (ptrdiff_t)(&a[SIZE-1]);  \
  for (i = 0; i < SIZE; i++)    { a[i] = 0; }  \
  for (i = 0; i < SIZE; i++)    { a[i] = s; }  \
  j = 0; \
  for (i = SIZE-1; i >= 0; i--) { j += a[i]; }  \
  for (i = SIZE-1; i >= 0; i--) { a[i] = 0; }  \
  free(a);

/* common code 3/5/6/7 ... */

#define CCODE2  \
  a = malloc(SIZE*s);  \
  p0 = (ptrdiff_t)(&a[0]);  \
  p1 = (ptrdiff_t)(&a[SIZE-1]);  \
  for (i = 0; i < SIZE; i++)    { a[i].b = 0; }  \
  for (i = 0; i < SIZE; i++)    { a[i].b = s; }  \
  j = 0; \
  for (i = SIZE-1; i >= 0; i--) { j += a[i].b; }  \
  for (i = SIZE-1; i >= 0; i--) { a[i].b = 0; }  \
  free(a);

void doOne(void)
  {
  int i;
  int8_t *a;
  int s = sizeof(int8_t);
  CCODE1
  }

void doTwo(void)
  {
  int i;
  int16_t *a;
  int s = sizeof(int16_t);
  CCODE1
  }

void doThree(void)
  {
  struct sp
    { int32_t b:24; }
  __attribute__((gcc_struct,packed));

  int i;
  struct sp *a;
  int s = sizeof(struct sp);
  CCODE2
  }

void doFour(void)
  {
  int i;
  int32_t *a;
  int s = sizeof(int32_t);
  CCODE1
  }

void doFive(void)
  {
  struct sp
    { int64_t b:40; }
  __attribute__((gcc_struct,packed));

  int i;
  struct sp *a;
  int s = sizeof(struct sp);
  CCODE2
  }

void doSix(void)
  {
  struct sp
    { int64_t b:48; }
  __attribute__((gcc_struct,packed));

  int i;
  struct sp *a;
  int s = sizeof(struct sp);
  CCODE2
  }

void doSeven(void)
  {
  struct sp
    { int64_t b:56; }
  __attribute__((gcc_struct,packed));

  int i;
  struct sp *a;
  int s = sizeof(struct sp);
  CCODE2
  }

void doEight(void)
  {
  int i;
  int64_t *a;
  int s = sizeof(int64_t);
  CCODE1
  }

int main(int argc, char *argv[])
  {
  int b;
  clock_t c1, c2;

  if      (argc == 1) { b = 4; }
  else if (argc == 2) { b = atoi(argv[1]); }
  else
    {
    fprintf(stderr, "** bad number of arguments\n");
    exit(EXIT_FAILURE);
    }

  if ((c1 = clock()) < 0)
    {
    fprintf(stderr, "** invalid clock() call result\n");
    exit(EXIT_FAILURE);
    }

  switch (b)
    {
    case 1:  doOne();     break;
    case 2:  doTwo();     break;
    case 3:  doThree();   break;
    case 4:  doFour();    break;
    case 5:  doFive();    break;
    case 6:  doSix();     break;
    case 7:  doSeven();   break;
    case 8:  doEight();   break;
    default:
      fprintf(stderr, "** bad byte argument\n");
      exit(EXIT_FAILURE);
    }

  if ((c2 = clock()) < 0)
    {
    fprintf(stderr, "** invalid clock() call result\n");
    exit(EXIT_FAILURE);
    }

  printf("\n");
  printf("        array size = %d\n", SIZE);
  printf("         data size = x%d\n", b);
  printf("&a[last]-&a[first] = %"PRId64"\n", (int64_t)(p1-p0));
  printf("            result = %"PRId64"\n", j);
  printf("\n");
  printf("processor time = %0.6f\n", (double)(c2-c1)/(double)CLOCKS_PER_SEC);
  printf("\n");

  exit(EXIT_SUCCESS);
  }

