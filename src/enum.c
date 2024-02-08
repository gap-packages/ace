
/*******************************************************************************

  enum.c
  1 Nov 03, 23 May 14
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


This is the main enumeration stuff for the core coset enumerator.

Note: many of the functions `borrow' the scanning code (R-style or C-style) & 
the deduction processing code from one another.  We do this for speed (wrapping 
things up in a function can carry a significant penalty, and generality `costs')
or because the code in not *exactly* the same (be warned).  I sometimes don't 
bother commenting such copies as fully as I might; check all copies for the full
details.  (This is a form of distributed documentation.)

*******************************************************************************/

#include "al0.h"

/*****************************************************************************
This macro readies a new coset for use, and gathers some statistics.
*****************************************************************************/

#define NEXTC(kk)                   \
  kk = nextdf;                      \
  for (col = 1; col <= ncol; col++) \
    { CT(kk, col) = 0; }            \
  nextdf++;  totcos++;              \
  if (++nalive > maxcos)            \
    { maxcos = nalive; }            \

/*****************************************************************************
This is all the stuff declared in al0.h
*****************************************************************************/

double begintime, endtime, deltatime, totaltime;
Logic  msgctrl;
SInt   msgincr, msgnext;
Logic  mendel, rfill, pcomp;
Coset  maxrow; 
SInt   rfactor, cfactor, comppc, nrinsgp, lahead;
SInt   llimit, lcount;
Coset  nalive, maxcos;
BInt   totcos;
Coset  chead, ctail;
Logic  pdefn;
float  ffactor; 
SInt   pdsiz, *pdqcol;
Coset *pdqrow;
SInt   toppd, botpd;
SInt   dedsiz;
Coset *dedrow;
SInt  *dedcol, topded, dedmode;
Logic  disded;
SInt  *edp, *edpbeg, *edpend;
SInt   ncol;
Entry **colptr, *col1ptr, *col2ptr;
SInt  *invcol;
SInt   ndrel, *relind, *relexp, *rellen, *relators;
SInt   nsgpg, *subggen, *subgindex, *subglength;
Logic  sgdone;
Coset  knr, knh, nextdf;

/*****************************************************************************
  Coset al0_apply(Coset cos, SInt *beg, SInt *end, Logic defn, Logic save)

Apply coset cos to the word stored in beg...end.  If defn is true then 
definitions are made to complete the trace, and if save is true any 
definitions are saved on the deduction stack.  This routine is intended for 
`general' use during R-style scans, so it does *not* worry about fill-factors 
or short gaps, nor is there any limit on the number of definitions made.  It's
main use is in the subgroup generator & relators as generators phases (when 
defn will be true, and save may be true or false). 

If a finite `index' is obtained, this is the return value.  If an overflow 
occurs, 0 is returned.  Otherwise -1 is returned.

Warning: cos *must* be a valid (1...nextdf-1) & non-coincident coset.  This 
routine must *never* be called if the coincidence queue is non-empty (ie, all 
coincidences must have been fully processed).
*****************************************************************************/

Coset al0_apply(Coset cos, SInt *beg, SInt *end, Logic defn, Logic save)
  {
  SInt *fwd, *bwd, j, ji, col;  Coset ifront, iback, k, i = -1;

  ifront = iback = cos;

  /* Forward scan, leaving ifront set to coset at left of leftmost hole in
  relator or to the last coset in the relator if no hole. */

  for (fwd = beg; fwd <= end; fwd++) 
    { 
    if (( i = CT(ifront, *fwd) ) > 0)  { ifront = i; }
    else                               { break; }
    }

  /* If the scan completed, then i = ifront & iback = cos, and we'll fall
  right through and check for a coincidence (ie, has ifront cycled back to cos
  or not?).  Else, there's a hole & a backward scan is required. */

  if (i == 0)
    {
    for (bwd = end; bwd >= fwd; bwd--) 
      {
      j = *bwd;  ji = invcol[j]; 

      if (( i = CT(iback, ji) ) > 0)  { iback = i; }
      else						/* scan stalled */
        {
        if (bwd == fwd)
          {
          /* The backward scan has only one gap, so note the deduction to
          complete the cycle. */

          CT(iback, ji) = ifront; 
          if (save)  { SAVED(iback, ji); }

          /* Since bwd == fwd and there was a hole, then either CT(ifront,j) 
          is still 0, or it has been set by a `backward' definition 
          (particularly if j's an involution).  If it has been set (on-the-
          fly, so to speak), we need to setup correctly for a possible 
          coincidence. */
 
          if (CT(ifront,j) > 0)
            { ifront = CT(ifront,j); }		/* May be coinc here */
          else
            {
            CT(ifront,j) = iback;
            ifront = iback;			/* Prevent false coinc */
            }
          }
        else if (defn)				/* Define a new coset */
          {
          /* Note that, if j is an involution, and occurs next to itself, then
          after the first defn, the remainder of the string of j's will close.
          Note that if j^2 = 1 & j is *not* being treated as an involution, 
          then `removing' it is a Tietze transformation, not a free reduction,
          so it survives the setup in control.c. */

          if (nextdf > maxrow)  { return(0); }		/* overflow */  
          NEXTC(k); 
          CT(iback,ji) = k;  CT(k,j) = iback; 
          if (save)  { SAVED(iback,ji); }

          iback = k;

          if (msgctrl && --msgnext == 0)
            {
            msgnext = msgincr;
            ETINT;
            printf("AD: a=%"PC" r=%"PC" h=%"PC" n=%"PC";", 
                                                        nalive,knr,knh,nextdf);
            printf(" l=%"PS" c=+%4.2f;", lcount, deltatime);
            printf(" m=%"PC" t=%"PB"\n", maxcos, totcos);
            begintime = endtime;
            }
          } 
        else  { return(-1); }		/* New coset definition disabled */
        }
      }
    }

  /* If we get here, the scan has been completed.  Check to see if we've found
  a pair of coincident cosets. */
 
  if (ifront != iback)
    { if ((i = al0_coinc(ifront,iback,save)) > 0)  { return(i); }  }

  return(-1);
  }

/*****************************************************************************
  static Coset al0_rl(Coset first, Coset last, Logic saved)

Do an R-style lookahead from coset #first up to	#last.  We return -1 if 
nothing exciting happens and >=1 if we get a finite `index' (eg, collapse to 
1).  Note that this (incl. its call to coinc()) does *not* alter knr/knh, 
although in may `invalidate' them.   Lookahead *never* makes *new* definitions
(so, it never overflows), but it may stack deductions, if requested.
*****************************************************************************/

static Coset al0_rl(Coset first, Coset last, Logic saved)
  {
  Coset row, ifront, iback, i, ii, l = -1;  
  SInt rel, j, k, *pj, *pk, *fwd, *bwd;

  for (row = first; row <= last; row++)
    {
    if (COL1(row) >= 0)
      {
      for (rel = 1; rel <= ndrel; rel++)
        {
        j = (mendel ? rellen[rel]/relexp[rel] : 1);
        for (k = 0; k < j; k++)
          {
          pj = &(relators[relind[rel]+k]);
          pk = pj + rellen[rel]-1;

  /* <-- cancel indent; the code here is essentially al0_apply(). */

  ifront = iback = row;

  for (fwd = pj; fwd <= pk; fwd++) 
    { 
    if ((l = CT(ifront, *fwd)) > 0)  { ifront = l; }
    else                             { break; }
    }

  if (l == 0)
    {
    for (bwd = pk; bwd >= fwd; bwd--) 
      {
      i = *bwd;  ii = invcol[i]; 

      if (( l = CT(iback, ii) ) > 0)  { iback = l; }
      else if (bwd == fwd)
        {
        CT(iback, ii) = ifront; 
        if (saved)  { SAVED(iback, ii); }

        /* Since we're *not* making definitions, there is no need to check if
        CT(ifront,i) is still undefined.  The *only* case where it's not is if
        ifront=iback & i=ii; ie, i's an involution & we've just deduced that 
        ifront.i=ifront.  So, we may set CT(ifront,i) twice, but that's rare &
        does no damage, and is cheaper than checking. */

        CT(ifront, i) = iback;
        goto next_k;
        }
      else  { goto next_k; }
      }
    }

  if (ifront != iback)
    { if (( l = al0_coinc(ifront,iback,saved) ) > 0)  { return(l); } }

  /* --> restore indent */

          if (COL1(row) < 0)  { goto next_row; }	/* now redundant */

          next_k: ;			/* prevent non_ISO warning */
          }
        }
      }
    next_row: ;				/* ditto */
    }

  return(-1);
  }

/*****************************************************************************
  static Coset al0_cl(Coset first, Coset last, Logic saved)

Do a C-style `lookahead' over all the entries in the table from row #first to 
row #last inclusive; ie, treat it as a deduction stack.  We may, or may not, 
save deductions, depending as we're in R-style or C-style.  Returned value & 
comments as for al0_rl(). 
*****************************************************************************/

static Coset al0_cl(Coset first, Coset last, Logic saved)
  {
  Coset row, ifront, iback, k = -1;
  SInt col, beg, end, i, *pj, *pk, *fwd, *bwd, j, ji;

  for (row = first; row <= last; row++)
    {
    if (COL1(row) >= 0)
      {
      for (col = 1; col <= ncol; col++)
        {
        if (CT(row,col) > 0)
          {
          if (( beg = edpbeg[col] ) >= 0)
            {
            end = edpend[col];
            for (i = beg; i <= end; i += 2)
              {
              pj = &(relators[edp[i]]);  pk = pj + edp[i+1]-1;

  /* <-- cancel indent; the code here is essentially al0_apply(). */

  ifront = iback = row;

  for (fwd = pj; fwd <= pk; fwd++) 
    { 
    if (( k = CT(ifront, *fwd) ) > 0)  { ifront = k; }
    else                               { break; }
    }

  if (k == 0)
    {
    for (bwd = pk; bwd >= fwd; bwd--) 
      {
      j = *bwd;  ji = invcol[j]; 

      if (( k = CT(iback, ji) ) > 0)  { iback = k; }
      else if (bwd == fwd)
        {
        CT(iback, ji) = ifront; 
        if (saved)  { SAVED(iback, ji); }
        CT(ifront, j) = iback;
        goto next_i;
        }
      else  { goto next_i; }
      }
    }

  if (ifront != iback)
    { if (( k = al0_coinc(ifront,iback,saved) ) > 0)  { return(k); } }

  /* --> restore indent */

              if (COL1(row) < 0)  { goto next_row; }
              next_i: ;
      } } } } }
    next_row: ;
    }

  return(-1);
  }

/*****************************************************************************
  static Coset al0_rdefn(SInt cnt, Logic fillr, Logic saved)

Start scanning through the relators at coset knr, making definitions as 
necessary to close the scans.  If coset knr closes against all relators, we 
fill any empty slots in its row (if fillr is set), bump knr up, and loop round
to process the next row.  On overflow we return 0 (leaving knr unchanged & 
the row only partially scanned) and on a finite index we return nalive.  Up to
cnt rows will be scanned (either completely or partially).  If nothing 
`exciting' happens, we return -1.  Deductions are stacked if saved is true.  
If cnt <0 then an infinite number of rows will be scanned (so we'll get an 
index or overflow).  We try as far as possible to exit with complete rows 
scanned, so we do not continue scanning after we've processed cnt rows 
(although the next active row could close without any definitions required, 
or, in fact, we could have finished without knowing it).

Note that a finite index is only correct if the table has no holes.  If we get
knr = nextdf & there are holes, then one option open to the control logic is 
to set knr to 1, and then rerun _rdefn() with fillr set to fill the holes.  
(Of course, this *isn't* what we actually do in this situation, since a holy-
table is precisely what we'd expect if some of the generators don't appear in 
any of the relators.)
*****************************************************************************/

static Coset al0_rdefn(SInt cnt, Logic fillr, Logic saved)
  {
  Coset ifront, iback, n, l = -1;
  SInt i, j, k, *beg, *end, *fwd, *bwd, col, m, mi;

  /* Count current knr up if it's redundant and/or get an index.  Note, we
  check nextdf *first* so that COL1(knr) (ie, CT(knr,1)) is defined. */

  while (knr < nextdf && COL1(knr) < 0)  { knr++; } 
  if (knr == nextdf)  { return(nalive); }

  while (cnt != 0)
    {
    /* Scan through all relators for this coset.  The code here is essentially
    the same as that in al0_apply.  We inline for speed (and flexibility; the
    code's not *exactly* the same). */

    for (i = 1; i <= ndrel; i++)
      {
      j = (mendel ? rellen[i]/relexp[i] : 1);
      for (k = 0; k < j; k++)
        {

  /* <-- cancel indent */

  /* Setup start & stop positions for scan, and the coset at the current scan
  positions. */

  beg = &(relators[relind[i]+k]);  end = beg-1 + rellen[i];
  ifront = iback = knr;

  /* Forward scan, leaving ifront set to coset at left of leftmost hole in
  relator or to the last coset in the relator if no hole. */

  for (fwd = beg; fwd <= end; fwd++) 
    { 
    if (( l = CT(ifront, *fwd )) > 0)  { ifront = l; }
    else                               { break; }
    }

  /* If the scan completed, then l = ifront & iback = cos, and we'll fall
  right through and check for a coincidence (ie, has ifront cycled back to cos
  or not?).  Else, there's a hole & a backward scan is required. */

  if (l == 0)
    {
    for (bwd = end; bwd >= fwd; bwd--) 
      {
      m = *bwd;  mi = invcol[m]; 

      if ((l = CT(iback, mi)) > 0)  { iback = l; }
      else                              	/* scan stalled */
        {
        if (bwd == fwd)
          {
          /* The backward scan has only one gap, so note the deduction to
          complete the cycle & prime for coincidence check. */

          CT(iback, mi) = ifront; 
          if (saved)  { SAVED(iback, mi); }

          if (CT(ifront, m) > 0)  { ifront = CT(ifront, m); }
          else                    { CT(ifront, m) = iback;  ifront = iback; }
          }
        else                  		/* Need to define a new coset  */
          {
          /* Note that, if m is an involution, and occurs next to itself, then
          after the first defn, the remainder of the string of m's will close.
          Note that if m^2 = 1 & m is *not* being treated as an involution, 
          then it may occur. */

          if (nextdf > maxrow)  { return(0); }		/* overflow */
          
          NEXTC(n);         			/* Making a definition ... */

          CT(iback,mi) = n;  CT(n,m) = iback;
          if (saved)  { SAVED(iback,mi); }

          iback = n;				/* Advance to next spot */

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
  a pair of coincident cosets.  Recall that _coinc (if it does not return >0)
  is guaranteed *not* to change knc/knh, although it may render them 
  redundant. */
 
  if (ifront != iback)
    {
    if (( l = al0_coinc(ifront,iback,saved) ) > 0)  { return(l); } 
    if (COL1(knr) < 0)  { goto do_next; }		/* knr redundant */
    }

  /* --> restore indent */

        }
      }

    /* All relators close at this coset, any row-filling to do?  Only 
    (formally) necessary if some g/G does *not* appear in any relator, but 
    it's usually a good thing to do. */

    if (fillr)
      {
      for (i = 1; i <= ncol; i++)
        {
        if (CT(knr,i) == 0)
          {
          if (nextdf > maxrow)  { return(0); }          /* overflow */
          
          NEXTC(k);					/* definition ... */
          CT(knr,i) = k;  CT(k,invcol[i]) = knr;
          if (saved)  { SAVED(knr,i); }

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
      }

    /* Row knr is fully scanned (or redundant), so we adjust knr up, jumping 
    over any redundancies & checking to see if we've finished.  We have also 
    used up one of our allowed rows, if there's a limit. */

    do_next:			/* from al0_coinc(): knr redundant */

    do  { knr++; }  while (knr < nextdf && COL1(knr) < 0);

    if (knr == nextdf)  { return(nalive); }

    if (cnt > 0)  { cnt--; }
    }

  return(-1);			/* `normal' termination */
  }

/*****************************************************************************
  static Coset al0_cdefn(SInt cnt)

Repeatedly process any outstanding deductions and make definitions until: we 
get a finite result (return > 0), we get an overflow (return 0), or we've 
defined cnt new cosets (return -1).  If cnt is zero then we make no 
definitions, simply clearing the deduction stack.  If cnt < 0 there's no 
limit on the number of definitions.
*****************************************************************************/

static Coset al0_cdefn(SInt cnt)
  {
  Coset irow, ires, ifront, iback, l, swap, pdqr, k;
  SInt icol, rcol, first, last, *beg, *end, *fwd, *bwd, i, m, mi, pdqc, col;
  Logic fi;

  while(TRUE)
    {
    /* Process all outstanding deductions on the stack */

    while (topded >= 0)
      {
      irow = dedrow[topded];  icol = dedcol[topded--];
      if (COL1(irow) < 0)  { continue; }	/* coset now redundant */
      else                 { ires = CT(irow,icol);  rcol = invcol[icol]; }

      fi = TRUE;				/* first pass through */

      proc_ded:	;				/* entry point, second pass */

      if ((first = edpbeg[icol]) >= 0)
        {
        last = edpend[icol];
        for (i = first; i <= last; i += 2)
          {
          beg = &(relators[edp[i]]);  end = beg + edp[i+1]-1;

  /* <-- cancel indent */

  /* We scan this e.d.p. against irow.  We don't need to scan the first
  position, since we *know* it must be ok.  We have to set l, in case the
  relator has length precisely one. */

  ifront = l = ires;  iback = irow;

  /* Forward scan, leaving ifront set to coset at left of leftmost hole in
  relator or to the last coset in the relator if no hole. */

  for (fwd = beg+1; fwd <= end; fwd++) 
    { 
    if (( l = CT(ifront, *fwd) ) > 0)  { ifront = l; }
    else                               { break; }
    }

  /* If the scan completed, ifront = l > 0 & iback = irow, and we'll fall
  right through and check for a coincidence (ie, has ifront cycled back to
  irow or not?).  Else, there's a hole & a backward scan is required. */

  if (l == 0)
    {
    for (bwd = end; bwd >= fwd; bwd--)
      {
      m = *bwd;  mi = invcol[m];

      if (( l = CT(iback, mi) ) > 0)   { iback = l; }
      else                              		/* scan stalled */
        {
        if (bwd == fwd)
          {
          /* The backward scan has an `empty' gap, so note the deduction to
          complete the cycle. */

          CT(iback, mi) = ifront;  CT(ifront, m) = iback;
          SAVED(iback, mi);
          }
        else if (bwd == fwd + 1)
          {
          /* Gap of length = 1.  If pdefn = T, store this on the pd queue. */

          if (pdefn)
            {
            pdqrow[botpd] = iback;  pdqcol[botpd] = mi;
            NEXTPD(botpd);
            if (botpd == toppd)  { NEXTPD(toppd); }
            }
          }

        iback = ifront;			/* prevents a false coincidence */
        break;
        }
      }
    }

  /* At this stage, if ifront != iback, then either the initial forward scan 
  did not cycle back to irow, or a backward scan produced a mismatch; in 
  either case, we have found a coincidence. In all other cases ifront = iback
  has been enforced, to prevent problems. */

  if (iback != ifront)
    {
    /* We do *not* return an index at this stage if _coinc() returns a +ve
    value, since there may still be deductions to process (which might 
    *decrease* nalive).  We do, however, detect if the current rows have
    become redundant.  Currently, the only finite value returned by coinc() is
    1, on a total collapse.  This clears the dedn stack, so we'll drop out of
    the while(topded>=1) loop immediately & then drop out of this function
    with an index of 1.  */

    al0_coinc(ifront,iback,TRUE);
    if (COL1(irow) < 0 || COL1(ires) < 0)  { goto next_ded; }
    }

  /* --> restore indent */

          }
        }

      /* Do we have to do a second pass? */

      if (fi && (irow != ires || icol != rcol))
        {
        swap = irow;  irow = ires;  ires = swap;
        swap = icol;  icol = rcol;  rcol = swap;
        fi = FALSE;
        goto proc_ded;
        }

      /* End of processing this deduction, loop back to next deduction.  We
      also jump here if current deduction becomes redundant. */

      next_ded: ;

      if (msgctrl && --msgnext == 0)
        {
        msgnext = msgincr;
        ETINT;
        printf("DD: a=%"PC" r=%"PC" h=%"PC" n=%"PC";", 
                                                     nalive, knr, knh, nextdf);
        printf(" l=%"PS" c=+%4.2f;", lcount, deltatime);
        printf(" d=%"PS"\n", topded+1);
        begintime = endtime;
        }
      }

    /* Find next empty position (& maybe finish). */

    for ( ; knh < nextdf; knh++) 
      {
      if (COL1(knh) >= 0) 
        {
        for (icol = 1; icol <= ncol; icol++)
          { if (CT(knh, icol) == 0)  { goto hfill; } }
        }
      }
    return(nalive);			/* coset table is complete */

    /* Try to fill the next hole in the table, if cnt allows */

    hfill: ;

    if (cnt == 0)  { return(-1); }
    else
      {				
      /* Do we have space to make a definition?  If not, return overflow.  If
      yes, prime the next sequential position & get its row number. */

      if (nextdf > maxrow)  { return(0); }
      NEXTC(k);

      /* If preferred defns are active, the pdq is non-empty, and the fill
      factor allows, scan thro' pdq looking for the oldest entry which has not
      become redundant or been filled.  Set fi T if such an entry is found and
      we make a pref defn. */

      fi = FALSE;		
      if ( pdefn && toppd != botpd &&
           (float)(knh-1)*ffactor >= (float)(nextdf-1) )
        {
        pdqr = pdqrow[toppd];  pdqc = pdqcol[toppd];
        while (COL1(pdqr) < 0 || CT(pdqr,pdqc) != 0)
          {
          NEXTPD(toppd);
          if (toppd == botpd)  { break; }
          pdqr = pdqrow[toppd];  pdqc = pdqcol[toppd];
          }

        if (toppd != botpd)
          {
          NEXTPD(toppd);
          CT(pdqr,pdqc) = k;  CT(k,invcol[pdqc]) = pdqr;
          SAVED(pdqr,pdqc);

          fi = TRUE;

          if (msgctrl && --msgnext == 0)
            {
            msgnext = msgincr;
            ETINT;
            printf("CP: a=%"PC" r=%"PC" h=%"PC" n=%"PC";", 
                                                        nalive,knr,knh,nextdf);
            printf(" l=%"PS" c=+%4.2f;", lcount, deltatime);
            printf(" m=%"PC" t=%"PB"\n", maxcos, totcos);
            begintime = endtime;
            }
          }
        }

      /* If no preferred definition made, fill next hole. */

      if (!fi)
        {
        CT(knh,icol) = k;  CT(k,invcol[icol]) = knh;
        SAVED(knh,icol);

        if (msgctrl && --msgnext == 0)
          {
          msgnext = msgincr;
          ETINT;
          printf("CD: a=%"PC" r=%"PC" h=%"PC" n=%"PC";", 
                                                        nalive,knr,knh,nextdf);
          printf(" l=%"PS" c=+%4.2f;", lcount, deltatime);
          printf(" m=%"PC" t=%"PB"\n", maxcos, totcos);
          begintime = endtime;
          }
        }

      if (cnt > 0)  { cnt--; }			/* keep track, if required */
      }
    }
  }

/*****************************************************************************
enum01.inc pulls in the "static Coset al0_rpefn(SInt cnt, Logic fill)"
function, and enum00.inc pulls in the next action & state arrays (ie, the
state machine.
*****************************************************************************/

#include "enum01.inc"
#include "enum00.inc"

/*****************************************************************************
  Coset al0_enum(SInt mode, SInt style)

mode 0 : start enumeration (from row 1 of zeroed table)
     1 : continue enumeration (from current row of current table)
     2 : redo enumeration (from row 1 of current table)

style 0 : R/C       HLT until overflow, then CR-style
      1 : R*        R-style + dedn stacking/processing
      2 : Cr        1 pass Felsch, 1 pass HLT, then C-style
      3 : reserved  Level 1: C* (?)
      4 : reserved  Level 1: defaulted R/C
      5 : C         Felsch strategy
      6 : Rc        1 pass HLT, 1 pass Felsch, then R-style
      7 : R         HLT strategy
      8 : CR        alternate Felsch & HLT passes

We can `start' at any time.  We can `redo' at any time provided that any 
changes to the presentation are confined to *adding* group relators or 
subgroup generators.  We can `continue' only if we have made *no* changes to 
the presentation (& even if we already have a finite index).  The rfactor /
cfactor parameters must be set correctly (ie, >0) for the `style' of this 
call; they can be 0 (or even <0), but this may cause weirdness (although some 
intriguing things become possible). 

return >1 : non-trivial finite index
        1 : index of 1 (? collapse in coincidence processing)
        0 : overflow
     -256 : incomplete table (ie, unfilled positions)
     -257 : unused
     -258 : unused
     -259 : iteration (loops) limit exceeded
     -260 : overflow during SG phase
     -512 : disallowed mode
     -513 : disallowed style
     -514 : disallowed mode/style combination (not used yet))
    -4096 : invalid machine state (aka reality failure)
    -4097 : invalid finite result (aka reality failure)
*****************************************************************************/

Coset al0_enum(SInt mode, SInt style)
  {
  SInt state, action;		/* The current state & action */
  Coset result;			/* The result */
  Logic isave = FALSE;		/* Save definitions/deductions on stack? */
  static Logic rhfree;		/* Table guaranteed hole-free (R-style)? */
  static Logic cdapp;		/* All definitions applied (C-style)? */
  SInt i,j,k;			/* temp ints / indices */
  SInt *pj, *pk;		/* temp pointers */
  Logic li;			/* temp booleans */

  /* Start up the timing for this call.  Prime the message counter (if
  required) and zero the loop count. */

  totaltime = 0.0;  begintime = al0_clock();
  if (msgctrl)  { msgnext = msgincr; }
  lcount = 0;

  /* Check mode, style, and their combination. */

  if (mode < 0 || mode > 2)
    { result = -512;  goto tail_tail; }
  if (style < 0 || style > 8 || style == 3 || style == 4)
    { result = -513;  goto tail_tail; }

  /* Do the appropriate setup for the requested mode.  Note that we *never*
  preserve pd's between calls, and there are *never* outstanding coincs at a
  call's exit (so we don't actually need to zero chead/ctail, but we do 
  anyway).  We may, or may not, preserve deduction stack, entries in the
  table and the various `progress' counters. */

  toppd = botpd = 0;  chead = ctail = 0;

  switch(mode)
    {
    case 0:				/* start; ie, a new run */

      topded = -1;  disded = FALSE;

      for (i = 1; i <= ncol; i++)  { CT(1, i) = 0; }

      nalive = maxcos = totcos = 1;  knr = knh = 1;  nextdf = 2;

      sgdone = FALSE;			/* SG not (successfully) run yet */
      rhfree = cdapp = TRUE;		/* prime for this new run */
    
    break;

    case 1:				/* continue */

    break;
    
    case 2:				/* redo */

      topded = -1;  disded = FALSE;
      knr = knh = 1;
      sgdone = FALSE;

    break;
    }

  /* The static variable rhfree is primed to true at the start of every run
  (ie, start mode), and retains its value across a sequence of continue & redo
  commands until the next start.  Any time we could *potentially* do any R-
  style applying without filling rows, we toggle it to false.  This indicates
  that the table *may* contain holes, and that the al0_upknh() routine may 
  need to be called before a finite index (due to knr = nextdf) is returned.  
  Any code anywhere which could cause empty table slots should take care to 
  ensure that rhfree is false.  Since rhfree is only invoked when checking a 
  finite result due to knr = nextdf, its value if we get a result due to knh=
  nextdf is of no concern (here, the table is hole-free, since that's what knh
  means).

  Instead of trying to be clever, and keeping a close watch on what the value
  of this should be at each point, we simply reset it at the start of any 
  call(s) where the rfill control parameter is false.  The cost of running 
  _upknh() is no more than that of row-filling (although it's concentrated in
  one `lump'), since all table positions are checked in both cases.  In fact,
  it will usually be less, since we can start the check at knh, we need not
  check rows that have become redundant, and we make no definitions.  Note
  that, even if row-filling is not needed (to obtain a finite index), it may
  still be beneficial to turn it on, since it may alter the definition
  sequence. */

  if (!rfill)  { rhfree = FALSE; }

  /* Do the appropriate setup for the requested style.  Our main concern is
  whether or not to stack defns/dedns (isave flag) so that these can be tested
  against all edp, for C-style enumerations.  We also have to track whether or
  not we have done this over the course of an entire run; we use the cdapp 
  flag for this.  This flag is similar in concept to the rhfree one, but is 
  more difficult to manage, since it is `more expensive' to get it `wrong'.  
  Furthermore, saving & applying dedns is a 3-stage process, and cdapp only
  tracks the first step.

  cdapp is static, and is primed to true.  Any time we *may* make any table
  entries *without* stacking them, we toggle it to false.  Whether or not the
  stacking (call to SAVED()) was successful is monitored by the global disded
  flag.  Whether or not all the stacked dedns have been processed is
  determined by the stack size (the global topded).  If knh=nextdf and all
  three stages were successful (ie, cdapp=TRUE & disded=FALSE & topded==-1)
  then the result is valid.  If not, then the actual index may be smaller than
  the `current' one (ie, nalive).  Since our table is guaranteed hole-free at
  this stage, the quickest check is to run an R-style scan (with definitions &
  deduction stacking both off) to move knr up to nextdf, perhaps finding some
  coincs along the way; this RA phase is the default action.

  The settings below prime isave & cdapp for the styles.  However, finer
  control is needed since, for example, a (successful) CL phase forces cdapp
  back to true (provided further dedns *will* be stacked (ie, isave is true)).
  We use the switch at the end of the state machine's main loop to do this.  
  Note that we must be careful, since we can exit at any time and we must 
  ensure that the status is valid for any continues or redos.  We err on the
  side of caution, so the RA phase may be done when it is not (formally)
  required.

  Note that in some styles we don't save deductions initially, since we do a 
  C-style table lookahead at the point where we switch from R-style to C-style
  (which effectively forces cdapp to true).  (We could also insist that *all*
  dedns *are* applied, and do a C-style lookahead at any point where we detect
  `missed' dedns.)  Of course, if the dedn stack is large enough, then we'll 
  never lose dedn's; but we could end up having to process *very* large dedn
  stacks, which can be expensive. */

  switch(style)
    {
    case 0: isave = FALSE;  cdapp = FALSE;  break;
    case 1: isave = TRUE;                   break;
    case 2: isave = TRUE;                   break;
    case 5: isave = TRUE;                   break;
    case 6: isave = FALSE;  cdapp = FALSE;  break;
    case 7: isave = FALSE;  cdapp = FALSE;  break;
    case 8: isave = TRUE;                   break;
    }

  /* Combine the style with the mode into the machine's starting state.  Prime
  machine with `dummy'/`null' action & `success' result. */

  state = 1 + 9*mode + style;

  action = 0;  result = -1;

  /* THE MACHINE ... */

  while (TRUE)
    {
    if (msgctrl) { fflush(stdout); }   /* flush any o/p before proceeding */

    /* lcount tracks which pass through the machine's loop this is.  Then use
    result of last action to get next state & action. */

    lcount++;

    action = al0_act[state][-result];
    state  =  al0_st[state][-result];

    switch(action)
      {

  /* <-- cancel indent */

  /* The null action; allows timing tests, progress messages, etc. */

  case 0:  result = -1;  break;

  /* Make some R-style definitions.  al0_rdefn() can return -1 (`nothing'
  happened), 0 (unable to make definition), or >0 (finite result).  Note that
  *all* finite `index' return values are `filtered' through the check phase
  (action #6), to prevent `problems'. */

  case 1:
    if (( result = al0_rdefn(rfactor, rfill, isave) ) > 0)  { result = -2; }
    break;

  /* Perform lookahead, in R-style *only*, if enabled.  This lookahead is used
  when we run out of table space, and it could allow us to continue *without*
  running a compaction.  However, we elect not to detect this state of affairs
  in the current version.  Instead, we'll *always* try to compact, and we'll 
  check after doing that whether or not there's any space available in the 
  table (however it was obtained).  Note that lookahead (& any coincidence 
  processing) does *not* alter nextdf or knr/knh (except in the collapse to 1
  case, of course), although it may render them redundant.  Since this is a 
  *lookahead* only, there is never any need to stack deductions.  We don't try
  to trap an invalid lahead value.  If we don't recognise it, we just quietly
  do nothing.

  Note that we can be as `sloppy' as we wish, in the sense that all we require
  is one or more coincs to free up some table space.  The current options all
  look only for immediate consequences of the current table, and don't worry
  about consequent consequences.  There are a great variety of other ways to
  do lookahead.  For example: we could repeatedly run _rl() until there's no
  further improvement; or we could bail out early, after a burst of 
  `significant' progress or if we're achieving `nothing'. */

  case 2:

  switch(lahead)
    {
    case 1:
      result = al0_rl(knr, nextdf-1, FALSE);
      if (msgctrl)
        {
        msgnext = msgincr;		/* start new count */
        ETINT;
        printf("L1: a=%"PC" r=%"PC" h=%"PC" n=%"PC";", 
                                                     nalive, knr, knh, nextdf);
        printf(" l=%"PS" c=+%4.2f;", lcount, deltatime);
        printf(" m=%"PC" t=%"PB"\n", maxcos, totcos);
        begintime = endtime;
        }
      break;
    case 2:
      result = al0_cl(1, nextdf-1, FALSE);
      if (msgctrl)
        {
        msgnext = msgincr;
        ETINT;
        printf("L2: a=%"PC" r=%"PC" h=%"PC" n=%"PC";", 
                                                     nalive, knr, knh, nextdf);
        printf(" l=%"PS" c=+%4.2f;", lcount, deltatime);
        printf(" m=%"PC" t=%"PB"\n", maxcos, totcos);
        begintime = endtime;
        }
      break;
    case 3:
      result = al0_rl(1, nextdf-1, FALSE);
      if (msgctrl)
        {
        msgnext = msgincr;
        ETINT;
        printf("L3: a=%"PC" r=%"PC" h=%"PC" n=%"PC";", 
                                                     nalive, knr, knh, nextdf);
        printf(" l=%"PS" c=+%4.2f;", lcount, deltatime);
        printf(" m=%"PC" t=%"PB"\n", maxcos, totcos);
        begintime = endtime;
        }
      break;
    case 4:
      result = al0_cl(knr, nextdf-1, FALSE);
      if (msgctrl)
        {
        msgnext = msgincr;
        ETINT;
        printf("L4: a=%"PC" r=%"PC" h=%"PC" n=%"PC";", 
                                                     nalive, knr, knh, nextdf);
        printf(" l=%"PS" c=+%4.2f;", lcount, deltatime);
        printf(" m=%"PC" t=%"PB"\n", maxcos, totcos);
        begintime = endtime;
        }
      break;
    default:
      result = -1;
      break;
    }

  if (result > 0)  { result = -2; }

  break;

  /* Perform compaction (any style) if it's allowed & then check whether the
  table has any space left.  If so, then continue, else return overflow.  Note
  that compaction does *not* alter nalive, but may change (reduce) knr / knh /
  nextdf.  It makes `free' space *available*, but it does not *create* it; 
  coincidences (normal, or in lookahead) do that. */

  case 3:

  if (nalive >= maxrow)  { result = 0;  goto tail_tail; }
  else if ( (double)(nextdf-1 - nalive)/(double)(nextdf-1) 
                                                     >= (double)comppc/100.0 )
    {
    al0_compact();
    if (msgctrl)
      {
      msgnext = msgincr;
      ETINT;
      printf("CO: a=%"PC" r=%"PC" h=%"PC" n=%"PC";",
                                                     nalive, knr, knh, nextdf);
      printf(" l=%"PS" c=+%4.2f;", lcount, deltatime);
      printf(" m=%"PC" t=%"PB"\n", maxcos, totcos);
      begintime = endtime;
      }
    }

  if (nextdf <= maxrow)  { result = -1; }
  else                   { result = 0;  goto tail_tail; }

  break;

  /* Do some C-style definitions / deduction processing.  al0_cdefn() can 
  return -1 (`nothing' happened), 0 (unable to make definition), or >0 
  (finite result - potential index, needs checking).  */

  case 4:
    if ((result = al0_cdefn(cfactor)) > 0)  { result = -2; }
    break;

  /* Do a C-style complete table lookahead.  This is run when we're switching
  styles from R- to C-style, or when we're `starting' C-style with an already
  existing table.  Since we maybe haven't being processing definitions, we
  need to run through the entire table.  We treat each table entry as a
  deduction and stack any new deductions.  A subsequent C-style defn/dedn
  pass will clear the stack, as usual; we can now enter C-style, and be
  confident of any C-style result.

  Actually, calling this lookahead is a misnomer.  It more correctly might be
  thought of as either a `check' (when we have a finite result but cannot
  guarantee that all definitions have been processed, or when we call the
  enumerator in the redo mode) or a `prime' (when we're switching to C-style)
  phase. */

  case 5:

  if (( result = al0_cl(1, nextdf-1, TRUE) ) > 0)  { result = -2; }
  if (msgctrl)
    {
    msgnext = msgincr;
    ETINT;
    printf("CL: a=%"PC" r=%"PC" h=%"PC" n=%"PC";", nalive, knr, knh, nextdf);
    printf(" l=%"PS" c=+%4.2f;", lcount, deltatime);
    printf(" m=%"PC" t=%"PB"\n", maxcos, totcos);
    begintime = endtime;
    }

  break;

  /* We have a finite result; triggered by knr=nextdf, knh=nextdf, or a
  collapse to 1 in coinc processing.  Check that it's a valid index; it may be
  a multiple, or the table may have holes.  If knr=nextdf, then all relators
  close against all cosets, so we need only check whether or not the table has
  any holes.  If knh=nextdf, then the table is hole-free, so we need to check
  that all table entries have been scanned in all edp.  Note that we have to
  check for a `clean' C-style termination *before* we may bump knh; this is an
  artifact of the overloading of knh's meaning. */

  case 6:

  if (knr == nextdf && rhfree)  
    { ; }					/* ok, fall through */
  else if (knh == nextdf && cdapp && !disded && topded < 0)
    { ; }					/* ok, fall through */
  else if (knr == nextdf)
    {
    al0_upknh();				/* check for holes ... */
    if (msgctrl)
      {
      msgnext = msgincr;
      ETINT;
      printf("UH: a=%"PC" r=%"PC" h=%"PC" n=%"PC";", nalive, knr, knh, nextdf);
      printf(" l=%"PS" c=+%4.2f;", lcount, deltatime);
      printf(" m=%"PC" t=%"PB"\n", maxcos, totcos);
      begintime = endtime;
      }
    if (knh < nextdf)				/* ... table is incomplete */
      { result = -256;  goto tail_tail; }
    }
  else if (knh == nextdf)
    {
    /* Apply all remaining cosets.  Note that knh=nextdf, so there are no
    holes & no defns will (can) be made.  Since we are only interested in
    moving knr up to nextdf (perhaps finding coincs), we turn mendel off; if 
    it's on (& left on), it can cause a dramatic slow-down. */

    li = mendel;  mendel = FALSE;
    al0_rdefn(-1,FALSE,FALSE);
    mendel = li;

    if (msgctrl)
      {
      msgnext = msgincr;
      ETINT;
      printf("RA: a=%"PC" r=%"PC" h=%"PC" n=%"PC";", nalive, knr, knh, nextdf);
      printf(" l=%"PS" c=+%4.2f;", lcount, deltatime);
      printf(" m=%"PC" t=%"PB"\n", maxcos, totcos);
      begintime = endtime;
      }
    }
  else
    { result = -4097;  goto tail_tail; }	/* can't happen */

  result = nalive;  goto tail_tail;

  break;					/* not really needed */

  /* If start or redo, scan & close the subgroup generators on coset #1.  Note
  that we can get coincidences, or collapses, or overflows here.  We treat an 
  overflow as fatal, and return a special value (-260) to alert the caller to 
  the fact that the subgroup generators have not been fully processed (so we 
  *must* (re)start, we can't continue).  Note that knr = knh = 1 here, and 
  they are *not* changed (we do no scanning against the relators, so they 
  can't go up, and coset 1 is never redundant).  Thus the only finite index we
  can get is a collapse to 1, and this is a valid result.

  Note that closing subgroup generators against coset 1 is done R-style, in
  that we (maybe) stack up definitions/deductions for later action, and make
  as many definitions as required to (immediately) fill any empty relator 
  table positions.  If the enumeration is a (pure) C-style one, we should 
  process each definition (ie, stacked deduction) *immediately*, since we 
  should only make definitions if there's nothing else we can do.  For the 
  moment, we don't worry about his `complication'.

  As this phase *must* be successfully completed before a continue is allowed,
  and it need not be the 1st phase (it can be the 2nd in redo), a loop limit 
  could cause an early return.  So we make sure the sgdone flag is correctly
  (re)set at all times. */

  case 7:

  if (nsgpg > 0)
    {
    for (i = 1; i <= nsgpg; i++)
      {
      pj = &(subggen[subgindex[i]]);  pk = pj-1 + subglength[i];
      if ((result = al0_apply(1,pj,pk,TRUE,isave)) >= 0)  { break; }
      }

    if (msgctrl)
      {
      msgnext = msgincr;
      ETINT;
      printf("SG: a=%"PC" r=%"PC" h=%"PC" n=%"PC";", nalive, knr, knh, nextdf);
      printf(" l=%"PS" c=+%4.2f;", lcount, deltatime);
      printf(" m=%"PC" t=%"PB"\n", maxcos, totcos);
      begintime = endtime;
      }

    sgdone = TRUE;
    if     (result == 0)  { result = -260;  sgdone = FALSE;  goto tail_tail; }
    else if (result > 0)  { result = -2; }
    }
  else
    { 
    result = -1;		/* `default' result */
    sgdone = TRUE;		/* ok, since nothing to do */
    }

  break;

  /* If start or redo (modes 0/2) and in an appropriate style (ie, 1st step
  will be a C-style scan), and requested (nrinsgp > 0), then scan & close the
  first nrinsgp group relators against coset 1.  Similar comments to those for
  the subgroup generators apply here, although an overflow does *not* force a
  restart here. */

  case 8:

  if (nrinsgp > 0)
    {
    for (i = 1; i <= nrinsgp; i++)
      {
      j = (mendel ? rellen[i]/relexp[i] : 1);
      for (k = 0; k < j; k++)
        {
        pj = &(relators[relind[i]+k]);  pk = pj-1 + rellen[i];
        if ((result = al0_apply(1,pj,pk,TRUE,isave)) >= 0)  { break; }
        }

      if (result >= 0)  { break; }
      }

    if (msgctrl)
      {
      msgnext = msgincr;
      ETINT;
      printf("RS: a=%"PC" r=%"PC" h=%"PC" n=%"PC";", nalive, knr, knh, nextdf);
      printf(" l=%"PS" c=+%4.2f;", lcount, deltatime);
      printf(" m=%"PC" t=%"PB"\n", maxcos, totcos);
      begintime = endtime;
      }

    if (result > 0)  { result = -2; }
    }
  else  { result = -1; }		/* `default' result */

  break;

  /* Do some R*-style definitions / deduction processing.  al0_rpefn() can 
  return -1 (`nothing' happened), 0 (unable to make definition), or >0 (finite
  result - potential index, needs checking).  Note the row-filling argument
  and the fact that we don't use (need?) isave, since dedn stacking is 
  mandatory here. */

  case 9:
    if ((result = al0_rpefn(rfactor, rfill)) > 0)  { result = -2; }
    break;

  /* If we get here, something's a touch awry. */

  default:  result = -4096;  goto tail_tail;

  /* --> restore indent */

      }				/* end of "switch(action)" */

    /* At this point, we have just completed action in state, and are about to
    `leave' state via one of its exit paths (selected by the (state, result)
    pair).  Now is the time to perform any action specific to this point.  
    Note that checks for any limits are done *after* this, so we're guaranteed
    that the (updated) status will be correct on any `early' exit. */

    switch(state)
      {
      case 32:
        if (result == -1)  { cdapp = TRUE; }
        break;
      case 38:
        if (result == -1)  { cdapp = TRUE; }
        break;
      case 45:
        if (result == 0)   { isave = TRUE; }
        break;
      case 46:
        if (result == -1)  { cdapp = TRUE; }
        break;
      case 47:
        if (result == -1)  { cdapp = TRUE; }
        break;
      case 55:
        if (result == -1)  { cdapp = TRUE; }
        break;
      case 56:
        if (result == -1)  { cdapp = TRUE; }
        break;
      case 58:
        if (result == -1 || result == 0)  { cdapp = FALSE; }
        break;
      case 59:
        if (result == -1)  { cdapp = TRUE; }
        break;
      }

    /* Any loop limit in force? */

    if (llimit > 0 && lcount >= llimit)
      { result = -259;  break; }
    }						/* end of "while(TRUE)" */

  /* We've either jumped here (finite result, overflow, error), or we've
  broken out the main loop (iterations limit).  We simply update the total 
  time for this call & return the `status'. */

  tail_tail:

  endtime    = al0_clock();
  totaltime += al0_diff(begintime,endtime);

  return(result);
  }

