
/**************************************************************************

	coinc.c
	Colin Ramsay
	17 Jun 98

	ADAPTIVE COSET ENUMERATOR, Version 2.000

	Copyright 1998 
	Centre for Discrete Mathematics and Computing,
	Department of Mathematics and 
	Department of Computer Science & Electrical Engineering,
	The University of Queensland, QLD 4072.

This is the coincidence handling for the coset enumerator.  Rewritten by 
Colin Ramsay, based on code by George Havas et al.

**************************************************************************/

#include "tc.h"

	/******************************************************************
	During the special coincidence processing of columns 1 & 2, at most
	two further coincidences can be pending at any one time.  These are
	stored in low1s/high1s & low2s/high2s.  This macro saves a (new)
	coincidence in a free slot.
	******************************************************************/

#define	SAVECOLS12(low,high)                               \
  if (low != high)                                         \
    {                                                      \
    if (low > high)                                        \
      { SWAP(low, high); }                                 \
    if (low1s != 0 && low1s == low && high1s == high)      \
      { ; }                                                \
    else if (low2s != 0 && low2s == low && high2s == high) \
      { ; }                                                \
    else                                                   \
      {                                                    \
      if (low1s == 0)                                      \
        { low1s = low;  high1s = high; }                   \
      else                                                 \
        { low2s = low;  high2s = high; }                   \
      }                                                    \
    }	

	/******************************************************************
	Trace back through coincidence list to find which coset 'path' is
	ultimately equal to.  Reset all cosets along trail to this to speed
	up future processing.  (cf. Union Find problem)
	******************************************************************/

#define COMPRESSPATH(path)       \
  if ((i = CT(path,1)) < 0)      \
    {                            \
    i = -i;                      \
    l = path;                    \
    while ((j = CT(i,1)) < 0)    \
      { i = -j; }                \
    j = CT(path,1);              \
    while ((j = CT(-j,1)) < 0)   \
      { CT(l,1) = -i;  l = -j; } \
    path = i;                    \
    }

extern void tc_text(int);
extern void ditrace(char*, int, int, int, int, int);

	/******************************************************************
	ctail (chead) is the tail (head) of the coincidence queue. During 
	coincidence processing CT(high,2) is used to link the coincidence 
	queue together.  CT(high,1) contains minus the equivalent (lower 
	numbered) coset (the minus sign acting as a redundancy flag).
	******************************************************************/

static int chead, ctail;

	/******************************************************************
	static void tc_check_index1(void)

	This routine is called only by tc_coinc_cols12, and only when 
	nalive = 1 & CT(1,1) is defined.  Thus, if all (other) entries in
	coset 1's row are defined (or are coincident with defined entries),
	then the index must be 1; i.e., _all_ the cosets are coincident!
	******************************************************************/

static void tc_check_index1(void)
  {
  int i, j;

  for (j = 2; j <= ncol; j++) 
    { 
    if (CT(1,j) != 0) 
      { continue; }

    /* If CT(1,j)==0, look down column j for a non-zero entry */

    for (i = 2; i < nextdf; i++) 
      { 
      if (CT(i,j) != 0) 
        { goto conti; }
      }
    return; 			/* column j has no defined entry */

    conti:			/* continue, to next column */
      ;				/* prevent non-ANSII warning ! */
    }

  /* index 1: set flags; set all entries in first row to 1; return */

  index1 = TRUE; 
  compct = TRUE; 
  nextdf = 2;
  for (i = 1; i <= ncol; i++) 
    { CT(1,i) = 1; }
  }

	/******************************************************************
	void tc_coinc_cols12(int low, int high)

	Process columns 1 and 2 of cosets low = high and its consequences.
	While handling the coincidences coming from the processing of the 
	first 2 columns and the possible coincidences arising from them, we
	have at most 2 more unprocessed coincidences which we need to queue
	somewhere to have their columns 1 and 2 processed later.  Thus we 
	set aside 4 locations (low1s, high1s; low2s, high2s) to store such 
	coincident cosets as may arise.
	******************************************************************/

void tc_coinc_cols12(int low, int high)
  {
  int i, j, l, lowi;
  int low1, low2, high1, high2, low1s, low2s, high1s, high2s;

  low1s = low2s = high1s = high2s = 0; 

  for ( ; ; ) 
    {
    COMPRESSPATH(low); 
    COMPRESSPATH(high);
 
    /* start to process cols 1 & 2 of low and high */

    if (low != high) 
      { 
      if (low > high) 
        { SWAP(low,high); } 
      high1 = CT(high,1); 
      high2 = CT(high,2);

      /* mark high coincident with low and queue the coincidence */

      CT(high,1) = -low; 
      if (chead == 0) 
        { chead = high; }
      else 
        { CT(ctail,2) = high; }
      ctail = high; 
      CT(high,2) = 0;

      /* look at the consequences of column 1 of rows low and high */

      if (high1 != 0) 
        { 
        j = invcol[1];

        /* delete ct(high1, j) at this stage rather than replace by low to
	avoid having two occurrences of low in the one column. */

        if (high1 != high) 
          { CT(high1,j) = 0; }
        else 
          { high1 = low; }

        if ((low1 = CT(low, 1)) != 0) 
          { SAVECOLS12(low1, high1); } 
        else 
          {
          CT(low,1) = high1;		/* note the deduction. */
          DITRACE(("coinc 1", low, 1, high1, 0, 0));
          if (!rtenum) 
            { SAVEDED(low,1); }
          }
 
        if ((lowi = CT(low,1)) != 0 && CT(lowi,j) == 0 && lowi != high) 
          { CT(lowi,j) = low; }
        }

      /* look at the consequences of column 2 of rows low and high */

      if (high2 != 0) 
        { 
        j = invcol[2];

        /* delete ct(high2, j) at this stage rather than replace by low to
	avoid having two occurrences of low in the one column. */

        if (high2 != high) 
          { CT(high2,j) = 0; }
        else 
          { high2 = low; }
 
        if ((low2 = CT(low,2)) != 0) 
          { SAVECOLS12(low2,high2); } 
        else 
          {
          CT(low,2) = high2;		/* note the deduction.  */
          DITRACE(("coinc 2", low, 2, high2, 0, 0));
          if (!rtenum)
            { SAVEDED(low,2); }
          }
 
        if ((lowi = CT(low,2)) != 0 && CT(lowi,j) == 0 && lowi != high) 
          { CT(lowi,j) = low; }
        }

      nalive--;
      if (nalive == 1 && CT(1,1) != 0) 
        { 
        tc_check_index1();  
        if (index1)  
          { return; }
        }

      if (msgctrl != 0 && nalive == nextout) 
        { 
        nextout -= msgincr; 
        if (nalive != lastout) 
          { 
          lastout = nalive; 
          tc_text(11); 
          } 
        }
      }

    /* remove coset high from stored up coincidences to be processed */

    if (low1s != 0) 
      { 
      if (low1s == high) 
        { low1s = low; }
      if (high1s == high) 
        { high1s  = low; }
      if (low1s >= high1s) 
        {
        if (low1s == high1s) 
          { low1s = 0; }
        else 
          { SWAP(low1s, high1s); }
        } 
      }

    if (low2s != 0) 
      { 
      if (low2s == high) 
        { low2s = low; }
      if (high2s == high) 
        { high2s = low; }
      if (low2s >= high2s)
        { 
        if (low2s == high2s) 
          { low2s = 0; }
        else 
          { SWAP(low2s, high2s); } 
        }
      }

    /* find the next coincident cosets to process. */

    if (low1s != 0) 
      { 
      low = low1s; 
      low1s = 0; 
      high = high1s;
      } 
    else if (low2s != 0) 
      { 
      low = low2s; 
      low2s = 0; 
      high = high2s;  
      }
    else 
      { break; }
    }
  }

	/******************************************************************
	void tc_coinc(int low, int high)

	Process the primary coincidence low = high and its consequences.
	This routine uses the idea described by Beetham ("Space saving in 
	coset enumeration", Durham Proceedings) but not the data structure.
 	It uses the data structure used in CDHW, with some modifications.

	Note: check through this sometime to see that under all possible
	values of high/highi/low/lowi we do the right thing and pick up
	all possible consequent coincidences.  Also check for both i = j
	& i != j.
	******************************************************************/

void tc_coinc(int low, int high)
  { 
  int i, j, lowi, highi;

  compct = FALSE; 
  ctail = chead = 0;
  if (low > high) 
    { SWAP(low, high);}
  if (msgctrl != 0)		/* round down to multiple of msgincr */
    { nextout = (nalive/msgincr)*msgincr; }
  DITRACE(("coinc 3", low, high, nalive, 0, 0));

  /* process columns 1 and 2 of the primary coincidence. */

  tc_coinc_cols12(low,high); 
  if (index1) 
    { return; }

  for( ; ; ) 
    {
    /* process columns 3 to ncol of the coincidence high=low. */

    for (i = 3; i <= ncol; i++) 
      {	
      /* highi - column i entry of coset high */
      if ((highi = CT(high, i)) == 0) 
        { continue; }
      j = invcol[i];

      /* delete CT(highi,j) at this stage rather than replace by low to
      avoid having two occurrences of low in the one column. */

      if (highi != high) 
        { CT(highi,j) = 0; }
      else 
        { highi = low; }

      /* lowi - column i entry for coset low */
      if ((lowi = CT(low,i)) != 0) 
        {
        if (lowi == high) 
          { lowi = low; }

        /* we have found a possibly new coincidence highi=lowi. */

        tc_coinc_cols12(lowi, highi); 
        if (index1) 
          { return; }
        } 
      else 
        {			/* mark new ded'n for later processing */
        DITRACE(("coinc 4", low, i, highi, 0, 0));
        CT(low,i) = highi; 
        if (!rtenum) 
          { SAVEDED(low,i); }
        }

      if ((lowi = CT(low, i)) != 0 && CT(lowi, j) == 0)  
        { CT(lowi, j) = low; }
      }

    /* Get next coincidence from the queue for processing.  ?low < high. */

    if ((chead = CT(high,2)) != 0) 
      { 
      high = chead;
      low = -CT(high,1);
      } 
    else
      { break; }
    }

  if (msgctrl != 0) 		/* round up to multiple of msgincr */
    { nextout = (nalive/msgincr + 1)*msgincr; }

  /* Adjust if row knc has become redundant.  Originally, this had knc--,
  but it was flagged with '?'s.  Knc++ is correct, and quicker! */

  while (CT(knc,1) < 0) 
    { knc++; }

  /* Delete all entries referencing dead cosets from the deduction 
  stack.  What about inverse references ??? */

  for (i = maxrow, j = maxrow+1; i >= topded; i--) 
    {
    if (CT(CT(i, 1), 1) >= 0)		/* CT(i,1) > 0 ?? */ 
      { 
      CT(--j, 1) = CT(i, 1); 
      CT(j, 2) = CT(i, 2);
      }
    }

  topded = j;
  DITRACE(("coinc 5", 0, 0, 0, 0, 0));
  }

