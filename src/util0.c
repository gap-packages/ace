
/*****************************************************************************

  util0.c
  2 Nov 03, 23 May 14
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

These are some utilities for Level 0 of ACE.

*****************************************************************************/

#include "al0.h"

#include <time.h>		/* for clock() */

/*******************************************************************************
The clock() function is used to "return an approximation of processor time used"
by enumerations and between messages (if enabled) by taking the differences 
between beginning & ending times.  Clock() is used since it's part of the C99
standard, and so should be available on non-POSIX machines.  The clock_t result
type is typically a 32- or 64-bit (unsigned) integer, and is converted to
seconds by dividing by CLOCKS_PER_SEC (aka CPS).  CPS is 10^6 for POSIX systems,
but other values may be encountered (eg, 10^3 in Windows).  Note that the
resolution of the clock() values is not *necessarily* CPS.  On many systems, the
returned values step up in 10 mSec increments (ie, 100 Hz system clock).

Note that clock() values can wrap around.  On a 32-bit system with CPS = 10^6,
the function will return the same value approximately every 72 minutes, and with
CPS = 10^3 the rollover time is about 7 weeks.  On 64-bit systems, wrap around
is ... not an issue.

al0_clock() returns the current value of clock(), in seconds, as a double, and
al0_diff() returns the difference between two al0_clock() values.  If CLK32 is 
defined then clock_t is taken to be an unsigned 32-bit integer, and we try to 
ensure we pick up the correct times and cope with (up to) 1 rollover.  If CLK64 
is defined we assume that clock_t is large enough to ignore rollover.

notes:
- Use the test4.c test programme to check out your compiler/OS combination.  If
  you get an overflow at (or before) ~72 mins, try CLK32.  If not, try CLK64.
- We're assuming unsigned.  If signed, negative times might pop up, and you'd
  need to rework things.  Note that casting (signed) numbers to longer (signed)
  types will sign-extend, so they're still negative.
- When using CLK32, provided that the run is short (wrt to the rollover interval
  of 71m35s) or that progress messages are output frequently enough, then times
  and differences will be correct.  On long runs with few messages, then they
  may be incorrect; they may be too short by N x 71m35s.
- Use the test0a.c 'Level 0 demonstration' programme to investigate how these
  routines behave in practice.
- You can, of course, use CLK32 on 64-bit machines, or CLK64 on 32-bit machines,
  if you want to experiment.
*******************************************************************************/

#if defined (CLK32)

double al0_clock(void)
  { return (double)(UINT32_MAX & clock())/(double)CLOCKS_PER_SEC; }

static double clkroll = ((double)65536*(double)65536)/(double)CLOCKS_PER_SEC;

double al0_diff(double c1, double c2)
  {
  if (c2 >= c1)  { return (c2-c1); }
  else           { return (clkroll - c1 + c2); }
  }

#elif defined (CLK64)

double al0_clock(void)
  { return (double)clock()/(double)CLOCKS_PER_SEC; }

double al0_diff(double c1, double c2)
  { return (c2-c1); }

#else

#error You must define CLK32 or CLK64

#endif

/*****************************************************************************
  void al0_init(void)

One-off initialisation of the Level 0 stuff.  Ensures a valid initial state, 
and sets defaults (default setting is roughly equivalent to the "def" option 
of Level 2).  Does *not* allocate / free memory, so it's up to the user (in 
practice, usually the Level 1 wrapper routines) to make sure memory's 
allocated and to properly free it to prevent memory leakage.  It's not really 
necessary to set *everything* here, but we do anyway, since we adhere to the 
P^3 Principle (ie, paranoia prevents problems).
*****************************************************************************/

void al0_init(void)
  {
  /* Set output to line-buffered.  Warning: may not work as advertised on some
  systems, hence the fflush(stdout) scattered through the code. */

  setvbuf(stdout, NULL, _IOLBF, 0);

  begintime = endtime = deltatime = totaltime = 0.0;
  msgctrl   = FALSE;
  msgincr   = msgnext = -1;

  mendel = FALSE;
  rfill  = TRUE;
  pcomp  = FALSE;

  maxrow  = 0;
  rfactor = 200;
  cfactor = 1000;
  comppc  = 10;
  nrinsgp = 0;
  lahead  = 1;

  llimit = lcount = 0;

  nalive = maxcos = totcos = 1;

  chead = ctail = 0;

  pdefn   = FALSE;
  pdsiz   = 0;
  ffactor = 0.0;
  pdqcol  = NULL;
  pdqrow  = NULL;
  toppd   = botpd = 0;

  dedsiz  = 0;
  dedrow  = NULL;
  dedcol  = NULL;
  topded  = -1;
  dedmode = 0;
  disded  = FALSE;

  edp = edpbeg = edpend = NULL;

  ncol    = 0;
  colptr  = NULL;
  col1ptr = col2ptr = NULL;
  invcol  = NULL;

  ndrel  = 0;
  relind = relexp = rellen = relators = NULL;

  nsgpg   = 0;
  subggen = subgindex = subglength = NULL;
  sgdone  = FALSE;

  knr    = knh = 1;
  nextdf = 2;
  }

/*****************************************************************************
  Logic al0_compact(void)

Remove unused rows from the coset table, by closing up all used rows to the 
front.  (This is *not* the same as putting the table into its standard form.) 
To maintain data-structure consistency, the pdq is cleared & any stored 
deductions/coincidences should be discarded.  The pdq entries don't matter, 
but throwing away unprocessed deductions or coincidences is *not* a good 
thing.  It is the *caller's* responsibility to ensure that this routine isn't 
called when there are outstanding deductions/coincidences or, if it is, that 
`appropriate' action is taken.  We return TRUE if we actually did any 
compaction, else FALSE.

In fact, we fully process all coincidences immediately.  So, outside of the 
coincidence processing routine, the coinc queue is always empty.  Since 
al0_compact isn't called during coincidence handling, we're ok there.  As for 
deductions, we *could* work thro the queue repeatedly as we compact, resetting
the stored coset numbers to their adjusted values, but we don't (v. 
expensive).  We just throw any outstanding deductions away, noting this in 
disded.  We worry later (if we get a finite result) about whether or not we
have to do any extra work to check whether this cavalier attitude was 
`justified'.

Note that this routine is called `on-the-fly' by some of the Level 2 options. 
It can also be called directly by the rec[over] option.
*****************************************************************************/

Logic al0_compact(void)
  {
  SInt col;
  Coset i, j, irow, knra, knha;

  /* If table is already compact, do nothing.  Else, clear any preferred 
  definitions on the queue and throw away (after logging) any outstanding 
  deductions. */

  if (nalive == nextdf-1)  { return(FALSE); }

  toppd = botpd = 0;
  if (topded >= 0)  { disded = TRUE;  topded = -1; }

  /* Zero the counters for knr/knh adjustment.  Note that we can't adjust
  these as we go, since it's their *original* values which are relevant. */

  knra = knha = 0;

  /* Set irow to the lowest redundant coset (which is *never* #1), and then
  compact the CT.  Note that, since there are no active coinc, irow *can't*
  appear as an entry in the table. */

  for (irow = 2; irow < nextdf; irow++)  { if (COL1(irow) < 0)  { break; } }

  for (i = irow; i < nextdf; i++)
    {
    if (COL1(i) < 0) 
      { if (i <= knr)  { knra++; }  if (i <= knh)  { knha++; } }
    else 
      {					/* Convert row i to row irow. */
      for (col = 1; col <= ncol; col++) 
        {
        if ((j = CT(i, col)) != 0) 
          {
          if (j == i)  { j = irow; }
          else         { CT(j, invcol[col]) = irow; }
          }
        CT(irow, col) = j;
        }
      irow++;
      }
    }

  knr -= knra;    knh -= knha;		/* Adjust counters */
 
  nextdf = irow;			/* 1st unused row */

  return(TRUE);
  }

/*****************************************************************************
  Logic al0_stdct(void)

This companion programme to compact() puts the table into standard form.  This
form is based on the order of the generators (ie, the cols) in the table, but 
is otherwise fixed for a given group/subgroup; it's independant of the details
of an enumeration.  It allows canonic rep'ves to be picked off by back-tracing
(see al1_bldrep()).  We chose *not* to combine stdct() & compact() into one 
routine, since the core enumerator may compact (more than once) & we don't 
want to impact it's speed with `unnecessary' work.  After an enumeration 
completes, a single call of compact() & then of stdct() gives a hole-free,
standardised table.  We can standardise holey-tables, but the result is only 
unique up to the set of coset labels in use. 

Similar remarks to those in compact() regarding pdefns, dedns, coincs, etc, 
etc apply here.  We return true if we actually change anything, else false.  
We do the work in two stages, since we want to avoid (possibly) throwing away 
dedns if we can avoid it.  Note that we have to do some work even if the table
is already standardised, since there is no quick way to check this.  However,
the termination condition is next=nextdf, and this occurs generally before we 
scan up to row=nextdf, 
*****************************************************************************/

Logic al0_stdct(void)
  {
  Coset row, cos, next, c1, c2, c3, c4;  SInt col, icol, iicol;

  /* Init next to 1st non-redundant coset > 1.  If this dne, there's only one
  active coset & all the rest (if any) are redundant, so CT is standard. */

  next = 1;
  do  { next++; }  while (next < nextdf && COL1(next) < 0);

  if (next == nextdf)  { return(FALSE); }

  /* Find 1st non-std entry, if it exists. */

  for (row = 1; row < nextdf; row++)
    {
    if (COL1(row) >= 0)
      {
      for (col = 1; col <= ncol; col++)
        {
        if (( cos = CT(row,col) ) > 0)
          {
          if      (cos <  next)  { ; }	/* ok */
          else if (cos == next)		/* new next value; maybe finish */
            { 
            do  { next++; }  while (next < nextdf && COL1(next) < 0);
            if (next == nextdf)  { return(FALSE); }
            }
          else  { goto non_std; }	/* table is non-std */
          }
        }
      }
    }

  return(FALSE);		/* Table is standard.  Ever get here? */

  non_std:

  /* Table is non-std, so we'll be changing it.  Clear the preferred defn 
  queue, and throw away (after logging) any outstanding dedns. */

  toppd = botpd = 0;

  if (topded >= 0)  { disded = TRUE;  topded = -1; }

  /* Now work through the table, standardising it.  For simplicity, we 
  `continue' the loops used above, restarting the inner (column) loop. */

  for ( ; row < nextdf; row++)
    {
    if (COL1(row) >= 0)
      {
      for (col = 1; col <= ncol; col++)
        {
        if (( cos = CT(row,col) ) > 0)
          {
          if      (cos <  next)  { ; }
          else if (cos == next)
            { 
            do  { next++; }  while (next < nextdf && COL1(next) < 0);
            if (next == nextdf)  { return(TRUE); }
            }
          else
            { 
            /* At this point, cos > next and we have to swap these rows.  Note
            that all entries in rows <row are <next, and will not be affected.
            We process x/X pairs in one hit (to prevent any nasties), so we 
            skip over any 2nd (in order) occurrence of a generator; the if()
            statement selects bn the 1st occurrence or an involn.  Warning: 
            trying to understand this code can cause wetware malfunction. */

            for (icol = 1; icol <= ncol; icol++)
              {
              iicol = invcol[icol];

              if (icol < iicol)
                {
                c1 = CT(next,icol);
                if      (c1 == next)  { c1 = cos; }
                else if (c1 == cos)   { c1 = next; }

                c2 = CT(cos,icol);
                if      (c2 == next)  { c2 = cos; }
                else if (c2 == cos)   { c2 = next; }

                c3 = CT(next,iicol);
                if      (c3 == next)  { c3 = cos; }
                else if (c3 == cos)   { c3 = next; }

                c4 = CT(cos,iicol);
                if      (c4 == next)  { c4 = cos; }
                else if (c4 == cos)   { c4 = next; }

                CT(next,icol)  = c2;
                if (c2 != 0)   { CT(c2,iicol) = next; }

                CT(cos,icol)   = c1;
                if (c1 != 0)   { CT(c1,iicol) = cos; }

                CT(next,iicol) = c4;
                if (c4 != 0)   { CT(c4,icol) = next; }

                CT(cos,iicol)  = c3;
                if (c3 != 0)   { CT(c3,icol) = cos; }
                }
              else if (icol == iicol)
                {
                c1 = CT(next,icol);
                if      (c1 == next)  { c1 = cos; }
                else if (c1 == cos)   { c1 = next; }

                c2 = CT(cos,icol);
                if      (c2 == next)  { c2 = cos; }
                else if (c2 == cos)   { c2 = next; }

                CT(next,icol) = c2;
                if (c2 != 0)  { CT(c2,icol) = next; }

                CT(cos,icol)  = c1;
                if (c1 != 0)  { CT(c1,icol) = cos; }
                }
              }

            do  { next++; }  while (next < nextdf && COL1(next) < 0);
            if (next == nextdf)  { return(TRUE); }
            }
          }
        }
      }
    }

  return(TRUE);
  }

/*****************************************************************************
  void al0_upknh(void)

Counts knh up to the next incomplete row, skipping redundants.  We either bail
out at an empty table slot, or reach nextdf.  During an enumeration knh is 
maintained by C-style, due to its overloaded meaning (ie, knh & knc).  If we 
can't guarantee that the table is hole-free in an R-style finite result, we 
have to run this check to make sure.

Note: this should not be called carelessly during an enumeration, since it is 
important that knh-based C-style hole filling & deduction stacking/processing 
are done together, due to the overloading of knh's meaning & the fact that it 
triggers a finite result if it hits nextdf.  This should really only be called
when we *know* we have a finite result (to check whether the table is hole-
free), or when we *know* that all definitions have been applied (perhaps in a 
C-style lookahead).

TBA ... this code is normally inlined for speed (see, eg, enum01.inc), so this 
routine is only used once (in al0_enum()).  Inline it & eliminate?
*****************************************************************************/

void al0_upknh(void)
  {
  SInt col;

  for ( ; knh < nextdf; knh++)
    {
    if (COL1(knh) >= 0)
      {
      for (col = 1; col <= ncol; col++)
        { if (CT(knh,col) == 0)  { return; } }
      }
    }
  }

/*******************************************************************************
  void al0_dedn(Coset cos, SInt gen)

Handling the deduction stack is a pain.  The best option, in many cases, seems
to be to throw deductions away if we get too many at any one time (where `too 
many' can be quite `small', eg, <1000), and run an "RA:" or a "CL:" check.  
However, dedmode #4 (which is the default) allows a special stack-handling 
function (ie, this fn) to be called if we try to stack a deduction & can't. 

Currently, in this mode our aim is *never* to lose any deductions, so we expand 
the stack space to accomodate the new element.  We take the opportunity to 
eliminate redundancies from the stack.  The code is essentially that used in 
dedmod #2 in _coinc() (which emulates ACE2).

Note the messaging code, since we're interested in what the stack actually 
`looks' like when it overflows!  Some ad hoc tests show that redundancies are 
common (in collapses).  Duplicates (incl. `inverted' duplicates) are not, and 
it's expensive to process these, so we don't bother trying to track them.

Warning: this is the *only* place in the core enumerator where we make a system 
call (apart from output & date/time calls, if messaging is active; if these fail
we've got real problems), and it's one which could fail.  There is *no* 
mechanism in ACE Level 0 for handling these sorts of errors, so we do the best 
we can to recover.  Note also that there is no cap on the amount of space which 
we'll (try to) allocate; so this could all come crashing down in a heap.

TBA ... I'm not sure how robust this realloc() code is.  Might be an idea to do
some tests & then rework it.
*******************************************************************************/

void al0_dedn(Coset cos, SInt gen)
  {
  SInt i,j, dead = 0;

  dedsiz *= 2;
  if ( (dedrow = (Coset *)realloc(dedrow, dedsiz*sizeof(Coset))) == NULL ||
       (dedcol = (SInt *)realloc(dedcol, dedsiz*sizeof(SInt))) == NULL )
    {
    /* Our attempt to allocate more space failed, and we lost the existing
    stack.  Print out a nasty message (if messaging is on), and tidy up.  Note
    that the enumerator works correctly with dedsiz=0, but discards *all* 
    deductions (& does so forever, since 2*0 = 0). */

    if (dedrow != NULL)  { free(dedrow); }
    if (dedcol != NULL)  { free(dedcol); }

    dedsiz = 0;  topded = -1;  disded = TRUE;

    if (msgctrl)  { printf("DS: Can't realloc, all deductions discarded\n"); }

    return;
    }

  /* Is is actually *worth* doing this?  In a big collapse, the proportion of 
  coinc dedns can be high; but these are skipped over when encountered in 
  _cdefn(), so why go to the expense of a (linear) pass & data move.  It helps
  to keep the stack size down (the stack can, potentially, reach the same size
  as the entire CT).  So we have a time vs memory trade-off.  In practice the
  stack is doubled each time, so there are only a few calls to this fn, and it
  works very well. */

  j = -1;  i = 0;
  while (i <= topded && COL1(dedrow[i]) >= 0)  { j++;  i++; }
  for ( ; i <= topded; i++)
    {
    if (COL1(dedrow[i]) >= 0)
          { dedrow[++j] = dedrow[i];  dedcol[j] = dedcol[i]; }
    else  { dead++; }               	/* track redundancies discarded */
    }
  topded = j;

  /* Now add the original cause of the problem.  There's no need to check for 
  an overflow, since we're guaranteed to have enough space at this point. */

  dedrow[++topded] = cos;  dedcol[topded] = gen;

  if (msgctrl)
    {
    msgnext = msgincr;
    ETINT;
    printf("DS: a=%"PC" r=%"PC" h=%"PC" n=%"PC";", nalive, knr, knh, nextdf);
    printf(" l=%"PS" c=+%4.2f;", lcount, deltatime);
    printf(" s=%"PS" d=%"PS" c=%"PS"\n", dedsiz, topded+1, dead);
    begintime = endtime;
    }
  }

/*****************************************************************************
  void al0_rslt(Coset rslt)

Pretty-print the result of a core enumerator run, and some gross statistics.
*****************************************************************************/

void al0_rslt(Coset rslt)
  {
  if (rslt >= 1)  { printf("INDEX = %"PC, rslt); }
  else
    {
    switch(rslt)
      {
      case -4097:  printf("BAD FINITE RESULT");   break;
      case -4096:  printf("BAD MACHINE STATE");   break;
      case  -514:  printf("INVALID MODE/STYLE");  break;
      case  -513:  printf("INVALID STYLE");       break;
      case  -512:  printf("INVALID MODE");        break;
      case  -260:  printf("SG PHASE OVERFLOW");   break;
      case  -259:  printf("ITERATION LIMIT");     break;
      case  -256:  printf("INCOMPLETE TABLE");    break;
      case     0:  printf("OVERFLOW");            break;
         default:  printf("UNKNOWN ERROR (%"PC")", rslt);  break;
      }
    }

  if (rslt <= -512)  { printf("\n"); }
  else
    { 
    printf(" (a=%"PC" r=%"PC" h=%"PC" n=%"PC";", nalive, knr, knh, nextdf);
    printf(" l=%"PS" c=%4.2f; m=%"PC" t=%"PB")\n", 
                                            lcount, totaltime, maxcos, totcos);
    }
  }

