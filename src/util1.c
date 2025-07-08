
/*****************************************************************************

  util1.c
  2 Nov 03, 19 Apr 14
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


These are the utilities for Level 1 of ACE.

*****************************************************************************/

#include "al1.h"

#include <ctype.h>

/*****************************************************************************
  void al1_init(void)

One-off initialisation of the Level 1 stuff, and all lower levels.  Note that 
there is no need to initialise, for example, genal[].  &algen[1] has to be a 
printable string.
*****************************************************************************/

void al1_init(void)
  {
  al0_init();

  workspace = DEFWORK;
  workmult  = 1;
  costable  = NULL;
  tabsiz    = 0;

  currrep = NULL;
  repsiz  = repsp = 0;

  asis = FALSE;

  grpname = NULL;
  rellst  = NULL;
  trellen = 0;

  ndgen    = 0;
  geninv   = NULL;
  gencol   = colgen   = NULL;
  galpha   = FALSE;
  algen[0] = algen[1] = '\0';

  subgrpname = NULL;
  genlst     = NULL;
  tgenlen    = 0;

  rfactor1 = cfactor1 = 0;
  pdsiz1   = dedsiz1  = 0;
  maxrow1  = ffactor1 = 0;
  nrinsgp1 = -1;
  }

/*******************************************************************************
  void al1_prtdetails(SInt bits)

This prints out details of the Level 0 & 1 settings, in a form suitable for 
reading in by applications using the core enumerator plus its wrapper (eg, ACE
Level 2).  If bits = 0, then enum, rel, subg & gen are printed.  If bits = 1 
then *all* of the presentation and the enumerator control settings are printed; 
this allows the run to be duplicated at a later date.  If bits is 2-5, then only
enum, rel, subg & gen, respectively, are printed.  This routine is really 
intended for the ACE Level 2 interface, where some items (ie, commands) cannot 
be interrogated by invoking them with an empty argument.  However it is put here
(at Level 1), since it is a useful utility for any application.

If messaging in on, this routine (with bits = 1) is called by al1_start() after 
all the setup & just before the call to al0_enum().  So it shows what the 
parameters used for an enumeration actually were.  They may not match what you 
thought, since the Level 1 wrapper (which interfaces between applications (ie, 
ACE's Level 2 interactive interface) and the Level 0 enumerator) trys to prevent
errors, and may occasionally ignore or change something.  If you call this after
changing parameters, but before calling _start(), the values may *not* reflect 
the new values.  If you want to see what the Level 2 parameters are (as opposed 
to the current Level 0/1 parameters), use the `empty argument' form of the 
appropriate commands.  The Level 2 "sr:N;" command invokes this function with 
bits = N.

To *exactly* duplicate a run, you may need to duplicate the entire sequence of
commands since ACE was started, the execution environment, and use the same 
executable; but that's a project for some rainy Sunday afternoon sometime in 
the future.  However do note that the allocation of generators to columns may 
have upset your intended handling of involutions and/or the ordering of the
generators.  In particular, the value of asis is the *current* value, which 
may not match that when the call to _start() which allocated columns &
determined which generators will be involutions was made.
*****************************************************************************/

void al1_prtdetails(SInt bits)
  {
  if (bits < 2)
    { printf("  -- Run Parameters ----------------------------------\n"); }

  if (bits == 0 || bits == 1 || bits == 2)
    {
    if (grpname != NULL)  { printf("Group Name: %s;\n", grpname); }
    else                  { printf("Group Name: G;\n"); }
    }

  if (bits == 1)
    {
    if (ndgen > 0)
      {
      printf("Group Generators: ");
      if (!galpha)  { printf("%"PS, ndgen); }
      else          { printf("%s", &algen[1]); }
      printf(";\n");
      }
    }

  if (bits == 0 || bits == 1 || bits == 3)
    {
    if (rellst != NULL)
      { 
      printf("Group Relators: ");
      al1_prtwl(rellst, 16);
      printf(";\n");
      }
    else  { printf("Group Relators: ;\n"); }
    }

  if (bits == 0 || bits == 1 || bits == 4)
    {
    if (subgrpname != NULL)  { printf("Subgroup Name: %s;\n", subgrpname); }
    else                     { printf("Subgroup Name: H;\n"); }
    }

  if (bits == 0 || bits == 1 || bits == 5)
    {
    if (genlst != NULL)
      { 
      printf("Subgroup Generators: ");
      al1_prtwl(genlst, 21);
      printf(";\n");
      }
    else  { printf("Subgroup Generators: ;\n"); }
    }

  if (bits == 1)
    {
    switch (workmult)
      {
      case 1:     printf("Wo:%"PS";",  workspace);  break;
      case KILO:  printf("Wo:%"PS"K;", workspace);  break;
      case MEGA:  printf("Wo:%"PS"M;", workspace);  break;
      case GIGA:  printf("Wo:%"PS"G;", workspace);  break;
      }
    printf(" Max:%"PC";", maxrow);
    if (msgctrl)  { printf(" Mess:%"PS";", msgincr); }
    else          { printf(" Mess:0;"); }
    printf(" Loop:%"PS";\n", llimit);

    if (asis)    { printf("As:1;"); }
    else         { printf("As:0;"); }
    if (pcomp)   { printf(" Path:1;"); }
    else         { printf(" Path:0;"); }
    if (rfill)   { printf(" Row:1;"); }
    else         { printf(" Row:0;"); }
    if (mendel)  { printf(" Mend:1;"); }
    else         { printf(" Mend:0;"); }
    printf(" No:%"PS"; Look:%"PS"; Com:%"PS";\n", nrinsgp, lahead, comppc);

    /* Note that we printout using the aliases, since we want to know (or, at 
    least, be able to deduce) what style was used.  Note that, although 
    ffactor is a float, the Level 1 (& Level 2) interfaces use ffactor1, which
    is an int.  So we need to convert for printout, to maintain the ability to
    read the output back in. */

    printf("C:%"PS"; R:%"PS";", cfactor1, rfactor1);
    printf(" PDef:%d; PSiz:%"PS";", pdefn, pdsiz);
    printf(" Fi:%d; DMod:%"PS"; DSiz:%"PS";\n", (int)ffactor, dedmode, dedsiz);
    }

  if (bits < 2)
    { printf("  ----------------------------------------------------\n"); }
  }

/*****************************************************************************
  void al1_rslt(Coset rslt)

Pretty-print the result of a run of the wrapper al1_start().  If there were no
problems at Level 1, this will just be the result of the call to al0_enum(), 
printed via al0_rslt().
*****************************************************************************/

void al1_rslt(Coset rslt)
  {
  if (rslt > -8192)  { al0_rslt(rslt); }
  else
    {
    switch(rslt)
      {
      case -8194:  printf("TABLE TOO SMALL\n");             break;
      case -8193:  printf("MEMORY PROBLEM\n");              break;
      case -8192:  printf("INVALID MODE\n");                break;
         default:  printf("UNKNOWN ERROR (%"PC")\n", rslt);  break;
      }  
    }
  }

/*****************************************************************************
These are the utilities for the simple list manipulation package used to
handle the group's relators & the subgroup's generators.  Note that it is up 
to the caller to catch any errors (flagged by the return values).
*****************************************************************************/

/*****************************************************************************
  Wlist *al1_newwl(void)

Creates a new (empty) word list.  Returns NULL on failure.
*****************************************************************************/

Wlist *al1_newwl(void)
  {
  Wlist *p = (Wlist *)malloc(sizeof(Wlist));

  if (p != NULL)
    { p->len = 0;  p->first = p->last = NULL; }

  return(p);
  }

/*****************************************************************************
  Wlelt *al1_newelt(void)

Creates a new (empty) word-list element.  Returns NULL on failure.
*****************************************************************************/

Wlelt *al1_newelt(void)
  {
  Wlelt *p = (Wlelt *)malloc(sizeof(Wlelt));

  if (p != NULL)
    {
    p->word  = NULL;
    p->len   = p->exp = 0;
    p->invol = FALSE;
    p->next  = NULL;
    }

  return(p);
  }

/*****************************************************************************
  void al1_addwl(Wlist *l, Wlelt *w)

Adds a word to a word list.  l must be non-null, but may be empty.
*****************************************************************************/

void al1_addwl(Wlist *l, Wlelt *w)
  {
  if (w == NULL || w->len == 0)  { return; }	/* ignore null/empty words */

  if (l->len == 0)  { l->first = w; }		/* add to start of list */
  else              { l->last->next = w; }	/* add to end of list */

  l->last = w;
  l->len++;
  }

/*****************************************************************************
  void al1_concatwl(Wlist *l, Wlist *m);

Concatenate m's list to l's, and delete m's header node.  Note that l is 
guaranteed non-null, but may be empty.  m may be null, empty, or contain data.
*****************************************************************************/

void al1_concatwl(Wlist *l, Wlist *m)
  {
  if      (m == NULL)    { return; }
  else if (m->len == 0)  { free(m);  return; }

  /* If we get here, m contains data ... */

  if (l->len == 0)					/* l is empty */
    { l->len = m->len;  l->first = m->first;  l->last = m->last; }
  else							/* l is non-empty */
    { l->len += m->len;  l->last->next = m->first;  l->last = m->last; }

  free(m);
  }

/*****************************************************************************
  void al1_emptywl(Wlist *l)

Delete the list of words in l, leaving l as an empty list.  Does *not* delete 
the storage for l.
*****************************************************************************/

void al1_emptywl(Wlist *l)
  {
  Wlelt *p, *q;

  if (l == NULL || l->len == 0)  { return; }

  for (p = l->first; p != NULL; )
    {
    q = p->next;

    if (p->word != NULL)  { free(p->word); }
    free(p);

    p = q;
    }

  l->len   = 0;
  l->first = l->last = NULL;
  }

/*****************************************************************************
  void al1_prtwl(Wlist *l, SInt n)
        
Pretty-prints a list of group relators or subgroup generators within the 
allowed (ie, LLL) number of columns (if it can).  n is the current output 
column.  The printout does *not* include any trailing ";" or "\n", and lines 
after the first are indented by 2 positions.  Words are printed in exp'l form.
If no enumeration has yet been run, exp is at its default of 1, so a printout 
will not be `exponentiated'.  Note that relators of the form xx are *always* 
printed out in the form (x)^2 *if* they were entered thus; in all other cases 
they are printed as xx.  This is to preserve the ability to specify whether or
not a generator should be treated as an involution when asis is true.

Note that if _start() has been called in start/redo mode, then the relator & 
generator lists are guaranteed to be free of empty words (although this
routine cleverly skips over these, in case it's called in other situations).  
They may contain duplicates, which are not detected (and could be beneficial 
in some cases, I guess). 

The column counting is a bit `rough', and does not correct for exponents or
generator numbers which are more than 1 digit wide.  Further, there's an extra
trailing space after a word of numeric generators.
*****************************************************************************/

void al1_prtwl(Wlist *l, SInt n)
  {
  Wlelt *e;  SInt elen, eexp, i, len, c;

  if (l == NULL || l->len == 0)  { return; }

  for (e = l->first; e != NULL; e = e->next) 
    {
    elen = e->len;  eexp = e->exp;	/* Alias e->len & e->exp ... */

    if (elen == 2 && e->word[1] == e->word[2])
      {					/* ... adjust them if involn */
      if (e->invol)  { eexp = 2; }
      else           { eexp = 1; }
      }

    len = elen/eexp;				/* base word length */
    if (len == 0)  { continue; }		/* silently ignore */

    if (!galpha) 
      {						/* numeric generators */
      if (eexp == 1) 
        { 
        n += 2 + len*2;				/* +2 for \ , *2 for \ n */
        if (n > LLL)  { printf("\n  ");  n = 2+2 + len*2; } 
        }
      else 
        {
        n += 2+4 + len*2; 			/* 4 for ()^e */ 
        if (n > LLL)  { printf("\n  ");  n = 4+4 + len*2; } 
        printf("(");  
        }

      for (i = 1; i <= len; i++)  { printf("%"PS" ", e->word[i]); }
      }
    else 
      {              				/* alphabetic generators */
      if (eexp == 1) 
        { 
        n += 2 + len;
        if (n > LLL)  { printf("\n  ");  n = 2+1 + len; } 
        }
      else 
        {
        n += 2+4 + len;          		/* 4 for ()^x */ 
        if (n > LLL)  { printf("\n  ");  n = 3+4 + len; } 
        printf("(");  
        }

      for (i = 1; i <= len; i++) 
        { 
        c = e->word[i];
        printf("%c", (char)( (c > 0) ? algen[c] : toupper(algen[-c]) ));
        }
      }

    if (eexp != 1)        { printf(")^%"PS, eexp); }
    if (e->next != NULL)  { printf(", "); }
    }
  }

/*******************************************************************************
These are the utilities for handling coset representatives and printing the CT.
*******************************************************************************/

/*****************************************************************************
  Logic al1_addrep(SInt col)

Add #col to the current rep've, possibly extending its storage.  Fails if we 
can't allocate memory.  Assumes currrep/repsp/repsiz are valid.
*****************************************************************************/

Logic al1_addrep(SInt col)
  {
  if (currrep == NULL)
    {
    repsp = 8;
    if (( currrep = (SInt *)malloc(repsp*sizeof(SInt)) ) == NULL)
      { repsiz = repsp = 0;  return(FALSE); }
    }
  else if (repsiz == repsp)		/* current entries are 0..repsiz-1 */
    {
    repsp *= 2;
    if (( currrep = (SInt *)realloc(currrep, repsp*sizeof(SInt)) ) == NULL)
      { repsiz = repsp = 0;  return(FALSE); }
    }

  currrep[repsiz++] = col;
  return(TRUE);
  }

/*****************************************************************************
  Logic al1_bldrep(Coset cos)

Traces back through the table, building up a rep've of #cos in currrep.  The 
rep've is in terms of column numbers, and is guaranteed to be the `canonic' 
rep've (ie, first in `length + col order' order) in terms of the *current* 
table.  The table may or may not be compact/standard.  If the table is compact
& standard, then the rep've is guaranteed to be `really' canonic, independant 
of the details of the enumeration (but dependant on the col allocation).  
(Note that rep'ves are Schreier reps in this case; ie, any prefix of a rep is 
a rep.)   Fails if _addrep() fails.

The order of the columns is *not* constrained in any way (apart from the col 
1/2 stuff), so we have to be careful to pick up the 1st col (ie, scol) in 
order (*after* they have been inverted) if more than one entry in a row is 
minimal.

Note that our ability to backtrace is predicated on the fact that the first 
definition of a coset is always in terms of a lower-numbered coset, and during
coinc processing we keep the lower-numbered coset & move data from the higher 
to the lower.  So each coset's row, apart from #1, *must* contain a lower-
numbered entry.  In this routine we *assume* that this property of the table 
has not been compromised in any way; if it has, then the behaviour is 
undefined.
*****************************************************************************/

Logic al1_bldrep(Coset cos)
  {
  SInt col, scol;  Coset low, slow, i;

  repsiz = 0;
  if (cos <= 1 || cos >= nextdf || COL1(cos) < 0)  { return(TRUE); }

  low = slow = cos;
  while (low > 1)
    {
    scol = 0;
    for (col = 1; col <= ncol; col++)
      {
      if ((i = CT(low,col)) > 0)
        {
        if (i < slow)				/* Lower row number found */
          { slow = i;  scol = col; }
        else if (i == slow && scol != 0)	/* Same row & slow < low */
          {					/* ... earlier column? */
          if (invcol[col] < invcol[scol])  { scol = col; }
          }
        }
      }

    /* Add it (increases repsiz); note the column inversion.  Failure in
    _addrep() sets repsiz to 0 */

    if (!al1_addrep(invcol[scol]))  { return(FALSE); }

    low = slow;
    }

  /* Reverse representative (note: col inversion already done).  A rep can't
  be longer than the number of cosets, so we can safely reuse i as index. */

  for (i = 1; i <= repsiz/2; i++) 
    {
    col = currrep[i-1];  scol = currrep[repsiz-i];
    currrep[i-1] = scol;  currrep[repsiz-i] = col;
    }

  return(TRUE);
  }

/*****************************************************************************
  Coset al1_trrep(Coset cos)

Traces currrep, starting at #cos.  Returns 0 on redundant cosets, on empty 
slot, or if there's no rep've.
*****************************************************************************/

Coset al1_trrep(Coset cos)
  {
  SInt i;

  if (repsiz == 0)  { return(0); }

  for (i = 0; i < repsiz; i++)
    {
    if ((COL1(cos) < 0) || ((cos = CT(cos,currrep[i])) == 0))
      { return(0); }
    }

  return(cos);
  }

/*****************************************************************************
  Coset al1_ordrep(void)

Traces currrep repeatedly until we arrive back at #1, or an empty slot.  The 
number of times round the loop is the order; return 0 if the tracing doesn't 
complete or the rep is empty.  Note that termination is guaranteed, since the 
table is finite & each col can contain each value at most once.  The order is 
at most the number of active cosets.
*****************************************************************************/

Coset al1_ordrep(void)
  {
  Coset i,j;

  if (repsiz == 0)  { return(0); }

  for (i = j = 1;  ; j++)
    {
    if ((i = al1_trrep(i)) == 1)  { return(j); }
    else if (i == 0)              { return(0); }
    }

  return(0);		/* Can't get here; prevent compiler whinging */
  }

/*****************************************************************************
  void al1_prtct(Coset f, Coset l, Logic c, Logic or)

This is a general-purpose coset table printer.  It prints rows from f[irst] to
l[ast] inclusive.  On a bad value, we try to do the `right' thing.  If c[oinc]
is true then the print-out includes coincident rows, else not.  If or is true 
then the order and a representative are printed.  The rep've is found via a 
backtrace of the table; if the table is in standard form, this rep will be 
minimal & the `first' in `order' (length + *column* order).  Note that the 
table may or may not have been compacted and/or standardised.

Warnings/Notes:
i) If you print entries >999999, then the neatly aligned columns will be lost,
although the entries *will* be spaced (C's printf() enforces this).
ii) _bldrep() can fail.  Most probably due to a lack of memory, but also if 
the table is `corrupt' or it is called `inappropriately'.  In this situation 
we should perhaps alert the user, but we choose simply to print `?'s instead!
*****************************************************************************/

void al1_prtct(Coset f, Coset l, Logic c, Logic or)
  {
  SInt i, j;  Coset row;

  if (f < 1)         { f = 1; }
  if (l > nextdf-1)  { l = nextdf-1; }
  if (f > l)         { return; }

  /* heading row ... */
  printf(" coset |");
  if (!galpha)
    { for (i = 1; i <= ncol; i++)  { printf(" %6"PS, colgen[i]); } }
  else
    {
    for (i = 1; i <= ncol; i++) 
      { printf("      %c", 
           (colgen[i] > 0) ? algen[colgen[i]] : toupper(algen[-colgen[i]])); }
    }
  if (or)  { printf("   order   rep've"); }
  printf("\n");

  /* divider row ... */
  printf("-------+");
  for (i = 1; i <= ncol; i++)  { printf("-------"); }
  if (or)   { printf("-----------------"); }
  printf("\n");

  /* Set start row, and skip over coincs if they're not wanted. */

  row = f;
  if (!c)  { while (row < nextdf && COL1(row) < 0)  { row++; } }

  while (row <= l)
    {
    printf("%6"PC" |", row);
    for (i = 1; i <= ncol; i++)  { printf(" %6"PC, (Coset)CT(row,i)); }
    if (or && row != 1)
      {
      if (COL1(row) < 0)  { printf("       -   -"); }
      else
        {
        if (al1_bldrep(row))
          {
          printf(" %7"PC"   ", al1_ordrep());
          for (i = 0; i < repsiz; i++)
            {
            j = colgen[currrep[i]];		/* generator number */
            if (!galpha)
              { printf("%"PS" ", j); }
            else
              { printf("%c", (j > 0) ? algen[j] : toupper(algen[-j])); }
            }
          }
        else  { printf("       ?   ?"); }
        }
      }
    printf("\n");

    /* If we're printing *all* rows, we can just incr row.  If not, we also
    have to jump over non-redundant rows. */

    row++;
    if (!c)  { while (row < nextdf && COL1(row) < 0)  { row++; } }
    }
  }

