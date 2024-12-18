
/* test5.c, Colin Ramsay, 28 Mar 14 ********************************************

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


Are packed structs packed as tightly as possible (MS Windows may not do this;
note the gcc_struct to force this, there are other ways) and can we use pointers
(correctly) on arrays of the little buggers?  Some snippets from the www ...

  When referring to the member x of a struct foo by name, the compiler knows 
  that x is potentially misaligned, and will generate additional code to access 
  it correctly.  Once the address of arr[0].x or arr[1].x has been stored in a 
  pointer object, neither the compiler nor the running program knows that it 
  points to a misaligned int object.  It just assumes that it's properly 
  aligned, resulting (on some systems) in a bus error or similar other failure.

  It's perfectly safe as long as you always access the values through the struct
  via the . (dot) or -> notation.  What's not safe is taking the pointer of 
  unaligned data and then accessing it without taking that into account.  Also, 
  even though each item in the struct is known to be unaligned, it's known to be
  unaligned in a particular way, so the struct as a whole must be aligned as the
  compiler expects or there'll be trouble (on some platforms, or in future if a 
  new way is invented to optimise unaligned accesses).

Given the sizeof() values for char & int, this programme tests the size of the
struct, the offsets of the fields, and the '.' & '->' access.  This should all
work OK.  Taking the (byte) addresses of the fields in the struct array should
also be fine, and checks that the array is packed.  However, *dereferencing* 
these addresses may yield gibberish or a crash.

*******************************************************************************/

#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>

int main(void)
  {
  struct foo 
    {
    char c;
    int x;
    } __attribute__((gcc_struct,packed));

  struct foo arr[2] = { { 'a', 12345 }, {'Z', 67890 } };
  struct foo *sp0 = &(arr[0]);
  struct foo *sp1 = &(arr[1]);

  printf ("\n");
  printf("      sizeof(char) = x%d\n", (int)sizeof(char));
  printf("       sizeof(int) = x%d\n", (int)sizeof(int));
  printf("sizeof(struct foo) = x%d\n", (int)sizeof(struct foo));

  printf ("\n");
  printf("offsetof(struct foo, c) = %d\n", (int)offsetof(struct foo, c));
  printf("offsetof(struct foo, x) = %d\n", (int)offsetof(struct foo, x));

  printf ("\n");
  printf("arr[0].c = %c\n", arr[0].c);
  printf("arr[0].x = %d\n", arr[0].x);
  printf("arr[1].c = %c\n", arr[1].c);
  printf("arr[1].x = %d\n", arr[1].x);

  printf ("\n");
  printf("sp0->c = %c\n", sp0->c);
  printf("sp0->x = %d\n", sp0->x);
  printf("sp1->c = %c\n", sp1->c);
  printf("sp1->x = %d\n", sp1->x);

  char *p0 = &(arr[0].c);
  int  *p1 = &(arr[0].x);
  char *p2 = &(arr[1].c);
  int  *p3 = &(arr[1].x);

  printf ("\n");
  printf("p0 = %p\n", (void*)p0);
  printf("p1 = %p\n", (void*)p1);
  printf("p2 = %p\n", (void*)p2);
  printf("p3 = %p\n", (void*)p3);

  fflush(stdout);

  /* Should be OK up to here, but may fail when the 'raw' pointers are actually
  dereferenced ... */

  printf ("\n");
  printf("*p0 = %c\n", *p0);
  printf("*p1 = %d\n", *p1);
  printf("*p2 = %c\n", *p2);
  printf("*p3 = %d\n", *p3);

  printf ("\n");
  exit(EXIT_SUCCESS);
  }

