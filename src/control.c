
/*******************************************************************************

  control.c
  1 Nov 03, 22 Apr 14
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


This is Level 1 of ACE; ie, an `easy to use' wrapper round the core enumerator.
Note that we usually choose to free & then re-malloc() space for the data 
structures.  This is simple, but may be inefficient on a long series of runs.  
It would be more efficient to keep track of how much memory is currently 
allocated (for each structure), and only free/malloc if the new structure is 
*bigger*.

*******************************************************************************/

#include "al1.h"

/*****************************************************************************
This is all the stuff declared in al1.h
*****************************************************************************/

SInt   workspace, workmult;
Entry *costable;
BInt   tabsiz;
SInt  *currrep, repsiz, repsp;
Logic  asis;
char  *grpname;
Wlist *rellst;
SInt   trellen, ndgen, *gencol, *colgen;
Logic *geninv, galpha;
char   algen[28];
SInt   genal[27];
char  *subgrpname;
Wlist *genlst;
SInt   tgenlen;

/*****************************************************************************
These are the Level 0 parameters `aliased' in Level 1
*****************************************************************************/

SInt  rfactor1, cfactor1;
SInt  pdsiz1, dedsiz1;
Coset maxrow1;
SInt  ffactor1, nrinsgp1;

/*****************************************************************************
  static void al1_freered(Wlist *w)

Freely reduce all the words in a word list.  Can reduce words to zero length; 
we leave these in, since they'll be removed (& deallocated) by remempt() 
later.  We keep it simple, and make multiple passes through a word until there
are no changes.
*****************************************************************************/

static void al1_freered(Wlist *w)
  {
  Wlelt *p;  Logic done;  SInt i,j;

  if (w == NULL || w->len == 0)  { return; }

  for (p = w->first; p != NULL; p = p->next)
    {
    do
      {
      done = TRUE;

      for (i = 1; i <= p->len-1; i++)
        {
        if (p->word[i] == -p->word[i+1])
          {
          for (j = i; j <= p->len-2; j++)  { p->word[j] = p->word[j+2]; }
          p->len -= 2;
          done = FALSE;
          break;
          }
        }
      }
    while (!done);
    }
  }

/*****************************************************************************
  static void al1_cycred(Wlist *w)

Cyclically reduce all the words in a word list.  Since this is run *after* 
freered(), it can't introduce any 0-length words (think about it).
*****************************************************************************/

static void al1_cycred(Wlist *w)
  {
  Wlelt *p;  Logic done;  SInt j;

  if (w == NULL || w->len == 0)  { return; }

  for (p = w->first; p != NULL; p = p->next)
    {
    do
      {
      done = TRUE;

      if ( (p->len >= 2) && (p->word[1] == -p->word[p->len]) )
        {
        for (j = 1; j <= p->len-2; j++)  { p->word[j] = p->word[j+1]; }
        p->len -= 2;
        done = FALSE;
        }
      }
    while (!done);
    }
  }

/*****************************************************************************
  static void al1_remempt(Wlist *w)

Removes & deallocates zero-length or null words from the list.  We KISS, and 
do a `copy', dropping any empty words.  We make no attempt to detect / remove 
duplicate words.
*****************************************************************************/

static void al1_remempt(Wlist *w)
  {
  Wlelt *newf, *newl, *old, *tmp;  SInt length;

  if (w == NULL || w->len == 0)  { return; }

  newf = newl = NULL;  length = 0;

  for (old = w->first; old != NULL; )
    {
    tmp = old;  old = old->next;

    if (tmp->word == NULL || tmp->len == 0)	/* blow away */
      {
      if (tmp->word != NULL)  { free(tmp->word); }
      free(tmp);
      }
    else					/* move to `new' list */
      {
      if (newf == NULL)  { newf = newl = tmp;  tmp->next = NULL; }
      else               { newl->next = tmp;  newl = tmp;  tmp->next = NULL; }

      length++;
      }
    }

  w->first = newf;  w->last = newl;  w->len = length;
  }

/*****************************************************************************
  static void al1_sort(Wlist *w)

Sort word list into nondecreasing length order, using a stable (as regards 
words of the same length) insertion sort.  Note that the list may contain 
duplicates, but is guaranteed *not* to contain any empty words.  We trace 
through the original list, stripping elements off the front & inserting them 
in the new list in their correct place.  Note the speculative check to see if 
we can tag the next element on at the end of the new list, instead of having 
to traverse the list looking for its proper place; this means that already 
sorted (or partially sorted) lists process fast.
*****************************************************************************/

static void al1_sort(Wlist *w)
  {
  Wlelt *newf, *newl, *old, *tmp, *curr, *currp;

  if (w == NULL || w->len < 2)  { return; }

  /* The list contains >1 word.  We move the first word to the new list, 
  remove it from the old list & make the new list `correct'. */

  newf = newl = w->first;  old = w->first->next;  newl->next = NULL;

  while (old != NULL)
    {
    tmp = old;  old = old->next;

    if (tmp->len >= newl->len)			/* tag onto the end */
      { newl->next = tmp;  tmp->next = NULL;  newl = tmp; }
    else if (tmp->len < newf->len)		/* tag onto the front */
      { tmp->next = newf;  newf = tmp; }
    else
      {
      /* At this point we have to scan the new list looking for tmp's
      position; this *cannot* be the first or last, because of the preceding 
      checks.  Further the new list must have at least two elements in it by 
      now (think about it). */

      currp = newf;  curr = newf->next;

      while (tmp->len >= curr->len)
        { currp = curr;  curr = curr->next; }

      tmp->next = curr;  currp->next = tmp;
      }
    }

  w->first = newf;  w->last  = newl;
  }

/*****************************************************************************
  static Logic al1_chkinvol(void)

First stage of involution checking / column allocation.  Builds up the initial
version of the geninv[] array, based on the relator list and the asis flag.  
If asis is false, any xx/x^2 (or whatever) sets x to an involution.  If asis 
is true, only a relator flagged as an invol does the trick.
*****************************************************************************/

static Logic al1_chkinvol(void)
  {
  SInt i;  Wlelt *p;

  if (geninv != NULL)  { free(geninv); }
  if (( geninv = (Logic *)malloc((ndgen+1)*sizeof(Logic)) ) == NULL)
    { return(FALSE); }

  geninv[0] = FALSE;					/* P.P.P. */
  for (i = 1; i <= ndgen; i++)  { geninv[i] = FALSE; }

  if (rellst != NULL && rellst->len > 0)
    {
    for (p = rellst->first; p != NULL; p = p->next)
      {
      if (p->len == 2 && p->word[1] == p->word[2])
        {
        if (asis)  { if (p->invol)  { geninv[ abs(p->word[1]) ] = TRUE; } }
        else                        { geninv[ abs(p->word[1]) ] = TRUE; }
        }
      }
    }

  return(TRUE);
  }

/*****************************************************************************
  static Logic al1_cols(void)

At this stage, geninv contains a list of the generators we would *like* to 
treat as involutions, based on the presentation & the asis flag.  We now 
allocate the generators to columns, honouring geninv & the order of entry, as 
far as we can.  We *must* ensure that the first two columns are either a 
generator & its inverse, or two involutions.  Once all this has been done, 
geninv & the columns are *fixed* for the entire run.  The invcol & gencol /
colgen arrays are created here; note the offsetting of the data in gencol, to 
cope with -ve generator nos (inverses).
*****************************************************************************/

static Logic al1_cols(void)
  {
  SInt i,j;

  /* First, we dispose of the `anomalous' case of one generator */

  if (ndgen == 1)
    {
    geninv[1] = FALSE;  ncol = 2;

    if (invcol != NULL)  { free(invcol); }
    if (gencol != NULL)  { free(gencol); }
    if (colgen != NULL)  { free(colgen); }
    if ( (invcol = (SInt *)malloc(3*sizeof(SInt))) == NULL ||
         (gencol = (SInt *)malloc(3*sizeof(SInt))) == NULL ||
         (colgen = (SInt *)malloc(3*sizeof(SInt))) == NULL )
      { return(FALSE); }

    invcol[0] = 0;				/* P.P.P. */
    invcol[1] = 2;				/* col 2 is inv of col 1 */
    invcol[2] = 1;				/* col 1 is inv of col 2 */

    gencol[0] = 2;				/* -gen #1 is col #2 */
    gencol[1] = 0;				/* P.P.P. */
    gencol[2] = 1;				/* +gen #1 is col #1 */

    colgen[0] = 0;				/* P.P.P. */
    colgen[1] = +1;				/* col 1 is + gen 1 */
    colgen[2] = -1;				/* col 2 is - gen 1 */

    return(TRUE);
    }

  /* As ndgen > 1, we can honour geninv.  Allocate the required space, since 
  we now know that ncol will be 2*ndgen - #involns. */

  ncol = 2*ndgen;
  for (i = 1; i <= ndgen; i++)  { if (geninv[i])  { ncol--; } }

  if (invcol != NULL)  { free(invcol); }
  if (gencol != NULL)  { free(gencol); }
  if (colgen != NULL)  { free(colgen); }
  if ( (invcol = (SInt *)malloc((ncol+1)*sizeof(SInt))) == NULL ||
       (gencol = (SInt *)malloc((2*ndgen+1)*sizeof(SInt))) == NULL ||
       (colgen = (SInt *)malloc((ncol+1)*sizeof(SInt))) == NULL )
    { return(FALSE); }

  invcol[0] = 0;  gencol[ndgen] = 0;  colgen[0] = 0;		/* P.P.P. */

  /* We can honour the generator ordering if the first generator is not an
  involution, or if both the first two are. */

  if ( !geninv[1] || (geninv[1] && geninv[2]) )
    {
    j = 0;
    for (i = 1; i <= ndgen; i++)
      {
      if (geninv[i])				/* involution, 1 col */
        {
        j++;  invcol[j] = j;  colgen[j] = +i;
        gencol[ndgen+i] = j;  gencol[ndgen-i] = j;
        }
      else					/* noninvolution, 2 cols */
        {
        j++;  invcol[j] = j+1;  gencol[ndgen+i] = j;  colgen[j] = +i;
        j++;  invcol[j] = j-1;  gencol[ndgen-i] = j;  colgen[j] = -i;
        }
      }

    return(TRUE);
    }

  /* We have to shuffle the columns.  At this point, generator #1 is an
  involution & #2 is not (think about it); we swap gen'rs 1 & 2, and then 
  honour the order. */

  invcol[1] = 2;  invcol[2] = 1;  invcol[3] = 3;

  gencol[ndgen+1] = 3;  gencol[ndgen-1] = 3;
  gencol[ndgen+2] = 1;  gencol[ndgen-2] = 2;

  colgen[1] = +2;  colgen[2] = -2;  colgen[3] = +1;

  j = 3;
  for (i = 3; i <= ndgen; i++)			/* any more gen'rs? */
    {
    if (geninv[i])				/* involution, 1 col */
      {
      j++;  invcol[j] = j;  colgen[j] = +i;
      gencol[ndgen+i] = j;  gencol[ndgen-i] = j;
      }
    else					/* noninvolution, 2 cols */
      {
      j++;  invcol[j] = j+1;  gencol[ndgen+i] = j;  colgen[j] = +i;
      j++;  invcol[j] = j-1;  gencol[ndgen-i] = j;  colgen[j] = -i;
      }
    }

  return(TRUE);
  }

/*****************************************************************************
  static void al1_getlen(void)

Compute the total length of the relators and the generators.
*****************************************************************************/

static void al1_getlen(void)
  {
  Wlelt *p;

  trellen = 0;
  if (rellst != NULL && rellst->len > 0)
    { for (p = rellst->first; p != NULL; p = p->next) { trellen += p->len; } }

  tgenlen = 0;
  if (genlst != NULL && genlst->len > 0)
    { for (p = genlst->first; p != NULL; p = p->next) { tgenlen += p->len; } }
  }

/*****************************************************************************
  static void al1_baseexp(Wlelt *e)

Compute exponent of word *e.  btry is current attempt at base length.  This 
counts up, so get exp correct (ie, as large as possible).  Originally used 
internally to save storage space (but not time); now used for edps & print-
out.  Note that geninv is now frozen & any involutary X's changed to x's, so 
we do not need to worry about these when trying to find the max possible 
exponent.
*****************************************************************************/

static void al1_baseexp(Wlelt *e)
  {
  SInt i, j, btry;

  for (btry = 1; btry <= e->len/2; btry++) 
    {
    if (e->len % btry == 0)		/* possible base length */
      { 
      e->exp = e->len / btry;
      for (i = 1; i <= btry; i++) 
        {                		/* for each gen in possible base */
        for (j = i + btry; j <= e->len; j += btry) 
          {                   		/* for each poss copy */
          if (e->word[i] != e->word[j])
            { goto eLoop; }  		/* mismatch, this e->exp failed */
          }
        }
      return;                		/* this e->exp is the exponent */
      }
    eLoop: ;                     	/* try next potential exponent */
    }
  e->exp = 1;                		/* nontrivial exponent not found */
  }

/*****************************************************************************
  static void al1_getexp(void)

Compute exponents of all words in both lists.
*****************************************************************************/

static void al1_getexp(void)
  {
  Wlelt *p;

  if (rellst != NULL && rellst->len > 0)
    { for (p = rellst->first; p != NULL; p = p->next)  { al1_baseexp(p); } }

  if (genlst != NULL && genlst->len > 0)
    { for (p = genlst->first; p != NULL; p = p->next)  { al1_baseexp(p); } }
  }

/*****************************************************************************
  static void al1_xtox(void)

Change any involutary X to x.
*****************************************************************************/

static void al1_xtox(void)
  {
  Wlelt *p;  SInt i;

  if (rellst != NULL && rellst->len > 0)
    {
    for (p = rellst->first; p != NULL; p = p->next)
      {
      if (p->word != NULL && p->len > 0)
        {
        for (i = 1; i <= p->len; i++)
          {
          if (p->word[i] < 0 && geninv[-p->word[i]])
            { p->word[i] = -p->word[i]; }
    } } } }

  if (genlst != NULL && genlst->len > 0)
    {
    for (p = genlst->first; p != NULL; p = p->next)
      {
      if (p->word != NULL && p->len > 0)
        {
        for (i = 1; i <= p->len; i++)
          {
          if (p->word[i] < 0 && geninv[-p->word[i]])
            { p->word[i] = -p->word[i]; }
    } } } }
  }

/*****************************************************************************
  static Logic al1_setrel(void)

Setup the relators for the enumerator.  Note how we double up the relators, so
we can do `cyclic' scans efficiently.  If ndrel=0, we could skip this & leave 
the last setup present, but we choose to tidy up.
*****************************************************************************/

static Logic al1_setrel(void)
  {
  Wlelt *p;  SInt i, j, first, second;

  if (relind != NULL)  { free(relind); }
  if (( relind = (SInt *)malloc((ndrel+1)*sizeof(SInt)) ) == NULL)
    { return(FALSE); }
  relind[0] = -1;					/* P.P.P. */

  if (relexp != NULL)  { free(relexp); }
  if (( relexp = (SInt *)malloc((ndrel+1)*sizeof(SInt)) ) == NULL)
    { return(FALSE); }
  relexp[0] = 0;					/* P.P.P. */

  if (rellen != NULL)  { free(rellen); }
  if (( rellen = (SInt *)malloc((ndrel+1)*sizeof(SInt)) ) == NULL)
    { return(FALSE); }
  rellen[0] = 0;					/* P.P.P. */

  if (relators != NULL)  { free(relators); }
  if (( relators = (SInt *)malloc(2*trellen*sizeof(SInt))) == NULL)
    { return(FALSE); }

  if (rellst != NULL && rellst->len > 0)
    {
    second = 0;  i = 1;

    for (p = rellst->first; p != NULL; p = p->next)
      {
      rellen[i] = p->len;
      relexp[i] = p->exp;
      first = second;
      second = first + p->len;
      relind[i] = first;
      for (j = 1; j <= p->len; j++)
        { relators[first++] = relators[second++] = p->word[j]; }
      i++;
      }
    }

  return(TRUE);
  }

/*****************************************************************************
  static Logic al1_setgen(void)

Build the generator array (one copy only, since there are no cyclic scans).  
Again, if nsgpg=0 we could skip this.
*****************************************************************************/

static Logic al1_setgen(void)
  {
  Wlelt *p;  SInt i, j, first;

  if (subgindex != NULL)  { free(subgindex); }
  if (( subgindex = (SInt *)malloc((nsgpg+1)*sizeof(SInt)) ) == NULL)
    { return(FALSE); }
  subgindex[0] = -1;					/* P.P.P. */

  if (subglength != NULL)  { free(subglength); }
  if (( subglength = (SInt *)malloc((nsgpg+1)*sizeof(SInt)) ) == NULL)
    { return(FALSE); }
  subglength[0] = 0;					/* P.P.P. */

  if (subggen != NULL)  { free(subggen); }
  if (( subggen = (SInt *)malloc(tgenlen*sizeof(SInt)) ) == NULL)
    { return(FALSE); }

  if (genlst != NULL && genlst->len > 0)
    {
    first = 0;  i = 1;

    for (p = genlst->first; p != NULL; p = p->next)
      {
      subglength[i] = p->len;
      subgindex[i] = first;
      for (j = 1; j <= p->len; j++)
        { subggen[first++] = p->word[j]; }
      i++;
      }
    }

  return(TRUE);
  }

/*****************************************************************************
  static Logic al1_bldedp(void)

Build the edp data structure by scanning through the appropriate portion of 
relators[] array for each relator.  Note that *if* x is to be treated as an 
involution, then relators of the form xx are *not* included, since they yield 
nothing.  However, relators of the form xx *must* be included if x/X has more 
than 1 column allocated in the table (ie, it is *not* treated as an 
involution).  At this stage, relators[] is still in the form of +/- gen'r nos.
Note that generators with single cols are being treated as involutions, and
any X's have been changed to x's, so we do not need to worry about picking up 
inverses of 1-column generators.
*****************************************************************************/

static Logic al1_bldedp(void)
  {
  SInt start, col, gen, rel, b,e,i;

  if (edpbeg != NULL)  { free(edpbeg); }
  if (edpend != NULL)  { free(edpend); }
  if (edp != NULL)     { free(edp); }

  if ( (edpbeg = (SInt *)malloc((ncol + 1)*sizeof(SInt))) == NULL ||
       (edpend = (SInt *)malloc((ncol + 1)*sizeof(SInt))) == NULL ||
       (edp = (SInt *)malloc(2*trellen*sizeof(SInt))) == NULL )
    { return(FALSE); }
  edpbeg[0] = edpend[0] = -1;				/* P.P.P. */

  start = 0;
  for (col = 1; col <= ncol; col++)
    {
    edpbeg[col] = start;		/* index of first edp, this col */
    gen = colgen[col];

    for (rel = 1; rel <= ndrel; rel++)
      {
      /* b points to the beginning & e to the end of the base of (the first
      copy of) relator rel. */

      b = relind[rel];
      e = b-1 + rellen[rel]/relexp[rel];

      for (i = b; i <= e; i++)
        {
        if (relators[i] == gen)
          {
          if ( !(b == e && relexp[rel] == 2 && invcol[col] == col) )
            {
            edp[start++] = i;
            edp[start++] = rellen[rel];
            }
          }
        }
      }

    if (start == edpbeg[col])		/* none found */
      { edpbeg[col] = edpend[col] = -1; }
    else
      { edpend[col] = start - 2; }	/* index of last edp, this col */
    }

  return(TRUE);
  }

/*****************************************************************************
  static void al1_transl(void)

Translate the relators & generators from arrays in terms of generators & 
inverses to arrays in terms of their associated column numbers in the coset 
table.
*****************************************************************************/

static void al1_transl(void)
  {
  SInt i;

  for (i = 0; i < 2*trellen; i++)
    { relators[i] = gencol[ndgen+relators[i]]; }

  for (i = 0; i < tgenlen; i++)
    { subggen[i] = gencol[ndgen+subggen[i]]; }
  }

/*****************************************************************************
  Coset al1_start(SInt mode)

This is a wrapper for the enumerator (ie, al0_enum()).  The mode parameter 
indicates what we want to do; for the moment it is the same as al0_enum()'s 
mode parameter, and is used to determine how much `set-up' we have to do.  
(The order in which this setting-up is done is *important*, since there are 
dependencies between the various components.)  The style parameter for the 
call will be built from the values of rfactor1/cfactor1.  Several other of the
Level 0 parameters are `aliased', to enable us to set them `conveniently'.  
The return value is either a Level 1 error (ie, <= -8192), or is that returned
by _enum() (ie, > -8192):

  -8192  disallowed mode
  -8193  memory problem
  -8194  table too small

Note: this routine (& all of Level 1) is written to be as general-purpose as 
possible.  In particular, it is *not* assumed that it will be driven by the 
Level 2 interactive interface.  So some of the code may seem unnecessary, or 
needlessly complicated.

Warning: this wrapper routine prevents some of the more obvious `errors' that 
may occur when continuing/redoing enumerations.  However, it cannot pick up 
all such errors; either be very careful, or use the Level 2 interactive 
interface.  It is the caller's responsibility to ensure that call sequences 
are valid.

Warning: this routine may invalidate the current table, without explicitly 
noting this fact.  You *must* check the return value, and only `believe' the 
table if this is >= -259 (ie, if the enumerator is called and if it does 
something ok)
*****************************************************************************/

Coset al1_start(SInt mode)
  {
  SInt i, style;  Coset rtn;

  if (mode < 0 || mode > 2)  { return(-8192); }

  /* If the mode is start or redo, then we have a (possibly) new or (possibly)
  expanded (ie, *additional* relators/generators) presentation; we have to do 
  all the setup associated with the relator and generator lists.  If the mode 
  is continue, we simply fall through. */

  if (mode == 0 || mode == 2)
    {
    /* If asis if false, then we freely/cyclically reduce the relators and
    freely reduce the generators.  (This may introduce (additional) empty 
    and/or duplicate words.)  We then remove any empty words, irrespective of 
    the value of asis; duplicates are not removed.  If asis is false, we sort 
    both lists.  We *always* (re)set ndrel & nsgpg, since it is not incumbent 
    on a caller of _start() to set (& reset) these correctly, and the length 
    of the lists may have changed anyway. */

    if (!asis)
      { al1_freered(rellst);  al1_freered(genlst);  al1_cycred(rellst); }

    al1_remempt(rellst);  al1_remempt(genlst);

    if (!asis)  { al1_sort(rellst);  al1_sort(genlst); }

    if (rellst == NULL)  { ndrel = 0; }
    else                 { ndrel = rellst->len; }
    if (genlst == NULL)  { nsgpg = 0; }
    else                 { nsgpg = genlst->len; }
    }

  /* If we're in start mode, we need to build a list of which generators are 
  to be *treated* as involutions & do a column allocation (possibly changing 
  this list).  These are *fixed* over a run (incl any redos), even if later 
  relators / values of asis would have changed it. */

  if (mode == 0)
    {
    if (!al1_chkinvol())  { return(-8193); }
    if (!al1_cols())      { return(-8193); }
    }

  /* If we're in start mode, then we have to build the empty table; the block
  of memory has already been allocated.  tabsiz is the max number of cosets 
  given the number of columns.  Note that coset #0 dne, and the extra rounding
  down by 1 (for safety; pure paranoia, not really needed).  Note the error if
  there's not enough memory for at least 2 rows, and the fact that size_t is 
  an unsigned type.  Thus, it can't go -ve, so we need to check it's size
  *before* decrementing it. */

  if (mode == 0)
    {
    tabsiz = ((size_t)workspace*(size_t)workmult)/(size_t)ncol;
    if (tabsiz < 4)  { return(-8194); }
    tabsiz -= 2;

    if (colptr != NULL)  { free(colptr); }
    if (( colptr = (Entry **)malloc((ncol + 1)*sizeof(Entry *)) ) == NULL)
      { tabsiz = 0;  maxrow = 1;  return(-8193); }

    /* We now chop up our block of memory into (tabsiz+1)-sized chunks, one
    for each column of the table.  Whether or not this is the best way to do 
    things in moot (cf, caching).  Recall that coset #0 is unused, and note 
    that a column can exceed 4G bytes space. */

    colptr[0] = NULL;
    for (i = 1; i <= ncol; i++)
      { colptr[i] = costable + (size_t)(i-1)*(size_t)(tabsiz+1); }
    col1ptr = colptr[1];
    col2ptr = colptr[2];
    }

  /* In start/redo mode, we now have to (re)build the data structures
  associated with the relators & generators. */

  if (mode == 0 || mode == 2)
    {
    /* The values in geninv have now been decided, and will be frozen for this
    run.  We now go through the relators/generators and change X to x if x is 
    to be *treated* as an involution. */

    al1_xtox();

    /* Calculate the total length of the relators & generators, and their
    correct exponents. */

    al1_getlen();  al1_getexp();

    /* Build the doubled-up list of relators.  Build the list of generators.
    Build the edp's.  Change relator/generator lists to column-based form. */

    if (!al1_setrel())  { return(-8193); }
    if (!al1_setgen())  { return(-8193); }
    if (!al1_bldedp())  { return(-8193); }

    al1_transl();
    }

  /* Having now done all the mode-specific setup, we embark on setting-up
  those Level 0 parameters which are aliased at Level 1 (ie, those which are 
  not set *directly* by the user).  We *assume* that the caller hasn't done 
  anything stupid, and try to honour the parameters requested.  This may be 
  automatic, involve changing the enumerator's state on the fly, cause an 
  error return, or be silently ignored ... */

  /* Pd's are not preserved between calls, but we may need to honour a new
  value of pdsiz.  Values <=0 translate to the default of 256 and >0 is 
  honoured.  It is up to the caller to ensure that pdsiz1 is a power of 2 & is
  >=2. We don't bother malloc'ing if we already have enough memory.  Note that
  pdsiz is initialised to 0, so we are guaranteed to allocate list space the
  first time through. */

  toppd = botpd = 0;

  if (pdsiz1 <= 0)  { pdsiz1 = 256; }

  if      (pdsiz1 <  pdsiz)  { pdsiz = pdsiz1; }
  else if (pdsiz1 == pdsiz)  { ; }
  else
    {
    if (pdqrow != NULL)  { free(pdqrow); }
    if (pdqcol != NULL)  { free(pdqcol); }
    if ( (pdqcol = (SInt *)malloc(pdsiz1*sizeof(SInt))) == NULL ||
         (pdqrow = (Coset *)malloc(pdsiz1*sizeof(Coset))) == NULL )
      { pdsiz = 0;  return(-8193); }

    pdsiz = pdsiz1;
    }

  /* A fill factor request of <= 0 picks up the default, anything else is
  honoured.  Levels 1/2 use ffactor1, which is always integral, so we need to
  convert to float; in general, we can convert (int)<->(float) without any
  problems, since ffactor1 is a `small' integer. */

  if (ffactor1 <= 0)
    { ffactor1 = 0;  ffactor = (float)((int)((5*(ncol + 2))/4)); }
  else
    { ffactor = (float)ffactor1; }

  /* Deductions *may* be preserved betweens calls; we need to be careful to
  preserve them if we're in continue mode, but we are free to empty the stack
  in start/redo mode (if we choose).  We honour size increases using realloc
  (which acts like malloc if the existing pointer is null); this preserves the
  stack, in the absence of allocation errors.  We honour size reductions 
  simply by changing dedsiz, but we take care to note if we have to discard 
  any deductions.  dedsiz <=0 means the `smallish' default of 1000, and >0 is
  honoured.  Comments similar to those for pdsiz1 apply. */

  if (dedsiz1 <= 0)  { dedsiz1 = 1000; }

  if (dedsiz1 < dedsiz)
    {						/* lost any deductions? */
    if (topded >= dedsiz1)  { topded = dedsiz1-1;  disded = TRUE; }
    dedsiz = dedsiz1;
    }
  else if (dedsiz1 == dedsiz)  { ; }
  else
    {
    if ( (dedrow = (Coset *)realloc(dedrow, dedsiz1*sizeof(Coset))) == NULL ||
         (dedcol = (SInt *)realloc(dedcol, dedsiz1*sizeof(SInt))) == NULL )
      {
      if (topded >= 0)  { disded = TRUE; }
      topded = -1;  dedsiz = 0;  return(-8193);
      }
    dedsiz = dedsiz1;
    }

  /* If nrinsgp1 <0 or nrinsgp >ndrel, then the default is to use all the
  relators.  Otherwise the request is honoured. */

  if (nrinsgp1 < 0 || nrinsgp1 > ndrel)
    { nrinsgp1 = -1;  nrinsgp = ndrel; }
  else
    { nrinsgp = nrinsgp1; }

  /* If maxrow1 <= 1, or >= the number of rows allowed by the allocated 
  memory, then maxrow defaults to the allocated memory size; else if it's >= 
  the current value of maxrow, then the request is honoured.  Otherwise, the
  request is honoured in start mode, and is honoured in redo & continue modes
  *provided* that it is at least as large as nextdf; it not, it's (re)set to
  nextdf-1 (here, maxrow >= 2, so we're OK as regards always allowing at least
  2 rows in the table).  Note that maxrow1 is initialised to 0 & nextdf to 2,
  so we're ok the first time through.  Recall that tabsiz is a BInt, while
  maxrow(1) is a Coset. */

  if (maxrow1 <= 1 || maxrow1 >= tabsiz)  { maxrow1 = 0;  maxrow = tabsiz; }
  else if (maxrow1 >= maxrow)             { maxrow = maxrow1; }
  else
    {
    /* now: 1 < maxrow1 < tabsiz and maxrow1 < maxrow */
    if (mode == 0)  { maxrow = maxrow1; }	/* start mode */
    else					/* redo/continue modes */
      {
      if (maxrow1 >= nextdf)  { maxrow = maxrow1; }
      else
        /* now: 2 <= maxrow1 < nextdf <= maxrow+1 <= tabsiz+1 */
        { maxrow = nextdf-1; }			/* (re)set to CT size */
      }
    }

  /* Pick up the required style & set the blocking factors. */

  if (rfactor1 < 0)
    {
    if (cfactor1 < 0)					/* R/C-style */
      { rfactor = -rfactor1;  cfactor = -cfactor1;  style = 0; }
    else if (cfactor1 == 0)				/* R*-style */
      { rfactor = -rfactor1;  cfactor = 0;  style = 1; }
    else						/* Cr-style */
      { rfactor = -rfactor1;  cfactor = cfactor1;  style = 2; }
    }
  else if (rfactor1 == 0)
    {
    if (cfactor1 < 0)					/* C*-style */
      {
      /* C* is not yet implemented.  For the moment, just use C-style. */

      rfactor = 0;  cfactor = -cfactor1;  style = 5;
      }
    else if (cfactor1 == 0)			/* R/C-style (defaulted) */
      {
      /* R/C-style with defaulted parameters, which try to `balance' the R- &
      C-style activity.  We set C-style to 1000 definitions, and assume that 1
      in 2 of the positions in a relator yield a definition, hence the 2000 
      (ie, 2x1000).  Note the care to prevent rfactor=0, as we're doing 
      integer divisions.  If there are no relators, we'll simply fill the 
      columns of each row, hence the 1000/ncol. */

      if (ndrel == 0)  { rfactor = 1 + 1000/ncol; }
      else             { rfactor = 1 + 2000/(1 + trellen); }
      cfactor = 1000;  style = 0;			/* Nota bene */
      }
    else						/* C-style */
      { rfactor = 0;  cfactor = cfactor1;  style = 5; }
    }
  else
    {
    if (cfactor1 < 0)					/* Rc-style */
      { rfactor = rfactor1; cfactor = -cfactor1; style = 6;
      }
    else if (cfactor1 == 0)				/* R-style */
      { rfactor = rfactor1;  cfactor = 0;  style = 7; }
    else						/* CR-style */
      { rfactor = rfactor1;  cfactor = cfactor1;  style = 8; }
    }

  /* And away we go ... */

  if (msgctrl)	{ al1_prtdetails(1); }		/* normal message control */

  rtn = al0_enum(mode,style);

  return(rtn);
  }

