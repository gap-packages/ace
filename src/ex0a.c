
/*******************************************************************************

  ex0a.c
  22 Dec 98, 26 Apr 14
  Colin Ramsay, uqcramsa@uq.edu.au

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


This is a simple example of how to drive ACE Level 0 (ie, the core enumerator) 
directly.  No attempt is made to be terribly clever, or general purpose, or to 
do much error checking.

notes:
- The enumeration is done in (pure) R-style (aka HLT).  So, we do not need to 
  setup (ie, malloc() and set size) the deduction stack or the preferred 
  definition list arrays, since these are never used.
- We need to use BInt/SInt/Coset/Entry for our integers & pointers.  This is a
  pain, but is needed to ensure correct compilation for all memory models.
- The result of an access to the table (ie, CT(...)) needs to be cast from type
  Entry to type Coset to allow correct formatting in printf(...) for all memory
  models.

*******************************************************************************/

#include "al0.h"

#include <time.h>

/*******************************************************************************
From level 2 ...
*******************************************************************************/

static char *datetime(void)
  {
  time_t t = time(NULL);
  return ctime(&t);
  }

/*******************************************************************************
Noddy macro to dump the table.  We include any coincident cosets and the 'next'
coset (which is not part of the table), and indicate the values of knr, knh and
nextdf.
*******************************************************************************/

#define DUMP2 \
  printf("     |   b   B a/A\n"); \
  printf(" ----+------------\n"); \
  for (i = 1; i <= nextdf; i++)   \
    {                             \
    printf("%4"PC" |", i);        \
    for (j = 1; j <= ncol; j++)              \
      { printf("%4"PC, (Coset)CT(i,j)); }    \
    if (i == knr)    { printf("  knr"); }    \
    if (i == knh)    { printf("  knh"); }    \
    if (i == nextdf) { printf("  nextdf"); } \
    printf("\n");                            \
    }

/*******************************************************************************
Group name: A_5 ;
Group generators: a, b ;
Group relators: a^2, b^3, (ab)^5 ;
Subgroup name: C_3 ;
Subgroup generators: ab ;

This gives index (5!/2)/5 = 12.  We *choose* to treat "a" as an involution.  The
columns of the table (generators) are numbered 1..ncol and the rows (cosets) 
1..maxrow.  Note the extra space for the unused 0th positions here; in the other
arrays we may, or may not, use position 0 - take care.  trellen & tgenlen are
from globals in Level 1.
*******************************************************************************/

void bld_pres(void)
  {
  Entry *p;
  SInt trellen, tgenlen;

  ncol = 3;

  /* p[] is the memory for the table, and colptr[] is that for the table's 
  column pointers.  NB: colptr[0] is *not* used, so no space is allocated for it
  in p[]. */

  p = (Entry *)malloc(ncol*(maxrow+1)*sizeof(Entry));
  colptr = (Entry **)malloc((ncol + 1)*sizeof(Entry *));

  colptr[0] = NULL;                     /* Not used */
  colptr[1] = p;                        /* Column for b */
  colptr[2] = p + (maxrow+1);           /* Column for B */
  colptr[3] = p + 2*(maxrow+1);         /* Column for a/A */
  col1ptr = colptr[1];
  col2ptr = colptr[2];

  invcol = (SInt *)malloc((ncol+1)*sizeof(SInt));
  invcol[0] = 0;                        /* Not used */
  invcol[1] = 2;
  invcol[2] = 1;
  invcol[3] = 3;                        /* Involution */

  ndrel = 3;
  trellen = 1*2 + 1*3 + 2*5;

  relexp = (SInt *)malloc((ndrel+1)*sizeof(SInt));
  relexp[0] = 0;                        /* Not used */
  relexp[1] = 2;                        /* a^2 */
  relexp[2] = 3;                        /* b^3 */
  relexp[3] = 5;                        /* (ab)^5 */

  rellen = (SInt *)malloc((ndrel+1)*sizeof(SInt));
  rellen[0] = 0;                        /* Not used */
  rellen[1] = 2;
  rellen[2] = 3;
  rellen[3] = 10;

  /* Column 3 is a & column 1 is b.  Note that relators are doubled up. */

  relators = (SInt *)malloc(2*trellen*sizeof(SInt));
  relators[0] = relators[1] = 
                relators[2] = relators[3] = 3;
  relators[4] = relators[5] = relators[6] = 
                relators[7] = relators[8] = relators[9] = 1;
  relators[10]  = 3;  relators[11]  = 1;
  relators[12]  = 3;  relators[13]  = 1;
  relators[14]  = 3;  relators[15]  = 1;
  relators[16]  = 3;  relators[17]  = 1;
  relators[18]  = 3;  relators[19]  = 1;
  relators[20]  = 3;  relators[21]  = 1;
  relators[22]  = 3;  relators[23]  = 1;
  relators[24]  = 3;  relators[25]  = 1;
  relators[26]  = 3;  relators[27]  = 1;
  relators[28]  = 3;  relators[29]  = 1;

  relind = (SInt *)malloc((ndrel+1)*sizeof(SInt));
  relind[0] = -1;                       /* Not used */
  relind[1] = 0;
  relind[2] = 4;
  relind[3] = 10;

  /* Note that edpbeg & edpend step up by 2's.  a is an invol & b is not. */

  edp = (SInt *)malloc(2*trellen*sizeof(SInt));
  edp[0] = 4;   edp[1] = 3;             /* 1st sig. b @ pos 4 & of len 3 */
  edp[2] = 11;  edp[3] = 10;            /* 2nd sig. b */
  edp[4] = 10;  edp[5] = 10;            /* Only sig. a (invol. ignored) */

  edpbeg = (SInt *)malloc((ncol + 1)*sizeof(SInt));
  edpbeg[0] = -1;                       /* Not used */
  edpbeg[1] = 0;                        /* 1st index for b/B (cols 1/2) */
  edpbeg[2] = -1;                       /* There are no B's */
  edpbeg[3] = 4;                        /* 1st index for a/A (col 3) */

  edpend = (SInt *)malloc((ncol + 1)*sizeof(SInt));
  edpend[0] = -1;                       /* Not used */
  edpend[1] = 2;                        /* Last index for b/B (cols 1/2) */
  edpend[2] = -1;                       /* There are no B's */
  edpend[3] = 4;                        /* Last index for a/A (col 3) */

  nsgpg = 1;
  tgenlen = 2*1;

  subggen = (SInt *)malloc(tgenlen*sizeof(SInt));
  subggen[0] = 3;                       /* Column 3 is a */
  subggen[1] = 1;                       /* Column 1 is b */

  subgindex = (SInt *)malloc((nsgpg + 1)*sizeof(SInt));
  subgindex[0] = -1;                    /* Not used */
  subgindex[1] = 0;

  subglength = (SInt *)malloc((nsgpg + 1)*sizeof(SInt));
  subglength[0] = 0;                    /* Not used */
  subglength[1] = 2;
  }

/*******************************************************************************
Subgroup name: identity ;
Subgroup generators: ;

Blow away the subgroup generator(s), so  we're enumerating over the trivial
subgroup.  This gives index (5!/2) = 60.  We cheat, leaving the data-structure 
alone and simply setting nsgpg to zero.
*******************************************************************************/

void edit_pres(void)
  { nsgpg = 0; }

int main(void)
  {
  /* for DUMP2 macro */
  Coset i;
  SInt j;

  printf ("\n");
  printf("%s (Level 0): example programme \"a\"\n", ACEVER);
  printf("Start time: %s", datetime());

  al0_init();                  /* initialise Level 0 & set 'defaults' */

  msgctrl = TRUE;              /* messaging on */
  msgincr = 1;                 /* print all actions */

  maxrow = 100;                /* 100 physical/allowed cosets */

  /* Set up to do a 'pure' R-style enumerations. */

  rfactor = 5000;
  cfactor = 0;
  rfill   = FALSE;
  lahead  = 0;
  comppc  = 100;

  /* must be called *after* maxrow is set */

  bld_pres();

  printf("\n");
  printf("Start mode (#0) & R-style (#7): the index 12 subgroup ...\n");
  printf("\n");

  al0_rslt(al0_enum(0,7));
  printf("\n");
  DUMP2;

  printf("\n");
  printf("Compacting the table ...\n");
  printf("\n");

  al0_compact();
  DUMP2;

  printf("\n");
  printf("Standardising the table ...\n");
  printf("\n");

  al0_stdct();
  DUMP2;

  edit_pres();

  printf("\n");
  printf("Start mode (#0) & R-style (#7): the full group ...\n");
  printf("\n");

  al0_rslt(al0_enum(0,7));
  printf("\n");
  DUMP2;

  printf("\n");
  printf("Compacting & standardising the table ...\n");
  printf("\n");

  al0_compact();
  al0_stdct();
  DUMP2;

  printf ("\n");
  printf("Stop time: %s", datetime());
  printf ("\n");

  exit(EXIT_SUCCESS);
  }

