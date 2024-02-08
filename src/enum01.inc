
/*****************************************************************************

  enum01.inc
  20 Oct 03, 19 Apr 14
  Colin Ramsay, uqcramsa@uq.edu.au

  ACE 4.100: Advanced Coset Enumerator, Version 4.1, Release 00

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


This is the code for the al0_rpefn() routine (ie, R*-style), used by enum.c.  In
concept, it's closest to C-style, except that the defns are done using relator 
application instead of via fill-factor/next hole.  It could also be viewed as 
R-style with all dedns stacked & processed.  We `borrow' heavily from the 
_cdefn()/_rdefn() routines; see the comments therein for a less terse code 
run-through.

Originally, termination was judged only on knr.  However, for a (big) collapse
this required what was effectively an RA phase to count knr up from the collapse
point to nextdf.  This is expensive if mendel is on (it's switched off in a 
genuine RA phase).  Since there is no way in advance of detecting this state, we
elect to keep track of knh (ie, holes) also.  We can terminate on either; the 
final check phase may invoke either an RA or an UH phase (or neither), depending
on circumstances.  Since we have to do some work anyway to terminate/check the 
result, this seems to be the fastest way; the only `unnecessary' work is 
counting up knh.

The original version of this routine processed deductions on a row by row basis.
The current version processes deductions on a scan by scan basis; ie, much more 
frequently.  It is closer in spirit to Felsch mode, and tends to have smaller 
max/tot statistics (esp. if there are any very long relators).

*****************************************************************************/

static Coset al0_rpefn(SInt cnt, Logic fill)
  {
  SInt icol, rcol, col, ii, j, k, m, mi, first, last, i;
  SInt *beg, *end, *fwd, *bwd;
  Coset irow, ires, l = -1;		/* prevents compiler whinging */
  Coset ifront, iback, n;

#include "enum02.inc"			/* `empty' deduction stack */

  /* Count up knh to its `correct' value; its current value may be redundant 
  and/or we may already have a complete (hole-free) table.  Ditto knr; its 
  current value may be redundant and/or we may already have scanned all non-
  redundant cosets. */

  for ( ; knh < nextdf; knh++) 
    {
    if (COL1(knh) >= 0) 
      {
      for (icol = 1; icol <= ncol; icol++)
        { if (CT(knh, icol) == 0)  { goto hfill1; } }
      }
    }
  return(nalive);

  hfill1:

  while (knr < nextdf && COL1(knr) < 0)  { knr++; } 
  if (knr == nextdf)                     { return(nalive); }

  /* The main loop.  Provided cnt is non-zero, each pass through this scans
  and closes one row. */

  while (cnt != 0)
    {
    /* Scan through all relators for this coset.  The code here is essentially
    the same as that in al0_apply.  We inline for speed (and flexibility; the 
    code's not *exactly* the same). */

    for (ii = 1; ii <= ndrel; ii++)
      {
      j = (mendel ? rellen[ii]/relexp[ii] : 1);
      for (k = 0; k < j; k++)
        {

  /* <-- cancel indent */

  /* Setup start & stop positions for scan, and the coset at the current scan 
  positions. */

  beg = &(relators[relind[ii]+k]);
  end = beg-1 + rellen[ii];
  ifront = iback = knr;

  /* Forward scan, leaving ifront set to coset at left of leftmost hole in
  relator or to the last coset in the relator if no hole. */

  for (fwd = beg; fwd <= end; fwd++) 
    { 
    l = CT(ifront, *fwd);
    if (l > 0)  { ifront = l; }
    else        { break; }
    }

  /* If the scan completed, then l = ifront & iback = cos, and we'll fall 
  right through and check for a coincidence (ie, has ifront cycled back to cos
  or not?).  Else, there's a hole & a backward scan is required. */

  if (l == 0)
    {
    for (bwd = end; bwd >= fwd; bwd--) 
      {
      m = *bwd;  mi = invcol[m]; 

      l = CT(iback, mi);
      if (l > 0)  { iback = l; }
      else					/* scan stalled */
        {
        if (bwd == fwd)
          {
          /* The backward scan has only one gap, so note the deduction to
          complete the cycle & prime for coincidence check. */

          CT(iback, mi) = ifront; 
          SAVED(iback, mi);

          if (CT(ifront, m) > 0)  { ifront = CT(ifront, m); }
          else                    { CT(ifront, m) = iback;  ifront = iback; }
          }
        else                            	/* need to define new coset */
          {
          /* Note that if m is an involution and occurs next to itself, then
          after the first defn the remainder of the string of m's will close.
          If m^2 = 1 & m is *not* being treated as an involution, then
          `removing' it is a Tietze transformation, not a free reduction. */

          if (nextdf > maxrow)  { return(0); }	/* overflow */

          NEXTC(n);                     	/* making a definition ... */

          CT(iback,mi) = n;  CT(n,m) = iback;
          SAVED(iback,mi);

          iback = n;                    	/* advance to next spot */

          if (msgctrl && --msgnext == 0)
            {
            msgnext = msgincr;
            ETINT;
            printf("RD: a=%"PC" r=%"PC" h=%"PC" n=%"PC";", 
                                                        nalive,knr,knh,nextdf);
            printf(" l=%"PS" c=+%4.2f;", lcount, deltatime);
            printf(" m=%"PC" t=%"PB"\n", maxcos, totcos);
            begintime = endtime;
    } } } } }

  /* If we get here, the scan has been completed.  Check to see if we've found
  a pair of coincident cosets.  Recall that _coinc() (if it does not return 
  >0) is guaranteed *not* to change knc/knh, although it may render them
  redundant. */
 
  if (ifront != iback)
    {
    l = al0_coinc(ifront,iback,TRUE);
    if (l > 0)          { return(l); } 
    if (COL1(knr) < 0)  { goto do_next; }	/* knr now redundant */
    }

  /* --> restore indent */

#include "enum02.inc"				/* `empty' deduction stack */

        if (COL1(knr) < 0)  { goto do_next; }	/* knr now redundant */
        }
      }

    /* All relators close at this coset, any row-filling to do?  May be
    necessary if some g/G does *not* appear in any relator or if mendel is F &
    the gen'r is `hidden'; it's usually a good thing to do anyway.  Don't 
    bother if the row is guaranteed hole-free. */

    if (fill && knr >= knh)
      {
      for (i = 1; i <= ncol; i++)
        {
        if (CT(knr,i) == 0)
          {
          if (nextdf > maxrow)  { return(0); }	/* overflow */

          NEXTC(n);                     	/* make definition */
          CT(knr,i) = n;  CT(n,invcol[i]) = knr;
          SAVED(knr,i);

          if (msgctrl && --msgnext == 0)
            {
            msgnext = msgincr;
            ETINT;
            printf("RF: a=%"PC" r=%"PC" h=%"PC" n=%"PC";", 
                                                        nalive,knr,knh,nextdf);
            printf(" l=%"PS" c=+%4.2f;", lcount, deltatime);
            printf(" m=%"PC" t=%"PB"\n", maxcos, totcos);
            begintime = endtime;
            }
          }
        }
#include "enum02.inc"				/* `empty' deduction stack */
      }

    /* Row knr is fully scanned (or redundant), so we adjust knr up, jumping 
    over any redundancies & checking to see if we've finished.  We have also 
    used up one of our allowed rows, if there's a limit.  We also check to see
    if the table if complete. */

    do_next:		/* via coinc or dedn processing: knr redundant */

    do
      { knr++; }
    while (knr < nextdf && COL1(knr) < 0);

    if (knr == nextdf)  { return(nalive); }

    if (cnt > 0)  { cnt--; }

    for ( ; knh < nextdf; knh++) 
      {
      if (COL1(knh) >= 0) 
        {
        for (icol = 1; icol <= ncol; icol++)
          { if (CT(knh, icol) == 0)  { goto hfill2; } }
        }
      }
    return(nalive);

    hfill2: ;				/* ";" prevents compiler whinging */
    }           			/* end of "while(cnt!=0)" */

  return(-1);				/* `normal' return */
  }

