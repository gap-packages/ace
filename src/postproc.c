
/**************************************************************************

        postproc.c
        Colin Ramsay
        17 Jun 98

	ADAPTIVE COSET ENUMERATOR, Version 2.000

	Copyright 1998 
	Centre for Discrete Mathematics and Computing,
	Department of Mathematics and 
	Department of Computer Science & Electrical Engineering,
	The University of Queensland, QLD 4072.

This is the post (enumeration) processing stuff for the coset enumerator.
Rewritten by Colin Ramsay, based on code by George Havas et al.

Note: there are several repeated bits of code here that could profitably
be pulled out as #define's.

**************************************************************************/

#include <ctype.h>

#include "tc.h"

extern Wordlhdr new_empty_word_list(void);
extern void add_word_to_word_list(Wordlhdr, Wordlptr);
extern void tc_coinc(int, int);
extern void tc_compact(void);
extern void tc_error(char*);
extern void tc_text(int);

	/******************************************************************
	mst[] has 2(nalive+1) (why +1?) positions, and is treated
	as a 2-column array of nalive+1 rows.  1 <= i <= nalive is the
	coset (row) & 1 <= j <= 2 is the column.  Column 1 is the `parent'
	coset & column 2 is the generator, such that parent*gen = coset.
	While mst[] is being built in tc_mst(), column 2 contains control
	stuff for the (non-recursive) tree building.  If we allocated a few
	extra spaces, we could eliminate the `-3' here, for speed?
	******************************************************************/

#define	MST(i,j)  mst[i+i+j-3]

	/******************************************************************
	int tc_tracew(int n, int *beg, int *end)

	Trace coset n applied to word (in terms of columns) which starts at
	beg and ends at end.  Return the coset at which the trace ends, or
	0.
	******************************************************************/

int tc_tracew(int n, int *beg, int *end)
  { 
  int *k;

  for (k = beg; k <= end; k++) 
    { 
    if ((n = CT(n, *k)) == 0) 
      { break; }
    }

  return (n);
  }

	/******************************************************************
	static void tc_mst(void)

	Build a minimal spanning tree in mst[] and set mstflg TRUE.
	Since this is a BFS, we can pick off minimal coset representatives.
	What does this do on an incomplete table ?!
	******************************************************************/

static void tc_mst(void)
  {
  int i, j, k, kn, col, nal, thisl, nextl;

  /* Delete any previous MST, and allocate (cleared) space for this one. */

  if (mst != NULL)
    { free(mst); }
  if ((mst = (int *)calloc(2*nalive + 2, sizeof(int))) == NULL)
    { tc_error("unable to allocate space for MST");}

  MST(1, 1) = -1;

  /* nal is the number of the cosets in the tree.  thisl is the branch
  being processed.  nextl is the number of the previous coset in the tree. 
  kn is the number of the current coset being scanned. */

  mstlevel = 1;			/* tracks length of longest representant */
  for (nal = 1, thisl = 1, nextl = 0; nal <= nalive; ) 
    {
    kn = thisl;
    thisl = MST(kn, 2);		/* col 2 links rows in order of form'n */

    for (col = 1; col <= ncol; col++) 
      {
      j = CT(kn, col);
      if (j < 0) 		/* coincident coset, ignore it */
        { break; }
      if (j == 0) 		/* incomplete coset table */
        { continue; }
      if (MST(j, 1) != 0)	/* coset j is already in tree */
        { continue; }

      MST(j, 1) = kn;		/* add a new coset in the tree ... */
      MST(j, 2) = nextl;
      nextl = j; 
      nal++;
      }

    if (nal >= nalive)		/* all cosets in the tree?  */
      { break; }
 				/* move on to next known coset in tree */
    if (thisl != 0) 
      { continue; }
    if (nextl <= 0) 
      { break; }
    				/* move onto next (level) branch in tree */
    thisl = nextl; 
    nextl = 0; 
    mstlevel++;
    }

  /* We have built the tree.  Put the column (ie, generator) numbers in 
  column 2.  We used this space previously to help build the tree. */

  for (i = 2; i <= nalive; i++) 
    {
    j = MST(i, 1);
    if (j == 0) 
      { tc_error("There is a zero entry in mst"); }
    else
      {
      for (k = 1; k <= ncol; k++)
        {
	if (CT(j, k) == i) 
          { 
          MST(i, 2) = k; 
          break; 
          }
        }
      }
    }

  mstflg = TRUE;
  }

	/******************************************************************
	static void tc_cosrep(int coseth, int *order)

	Find the coset representative of coseth, and store it in 
	crepx[1..].  Store the length of the coset representative in 
	crepx[0].   Space for crepx[] must be allocated before 
	calling this function.  Also find the order of the rep, which may
	or may not be actually used.
	******************************************************************/

static void tc_cosrep(int coseth, int *order)
  {
  int e, i, j, kn, rep;

  if (!mstflg) 
    { tc_error("no minimal spanning tree in cosrep"); } 
  if (coseth > nalive || coseth < 2) 
    { tc_error("invalid coset in cosrep"); }
  
  for (rep = coseth, i = 0; rep > 1; rep = MST(rep, 1)) 
    { crepx[++i] = MST(rep, 2); }	/* in terms of column nos */

  /* Now i is the length of the coset rep. Reverse crepx[] */

  crepx[0] = i;
  for (j = 1, e = i; j <= i / 2; j++, e--) 
    { SWAP(crepx[j], crepx[e]) }

  /* find the order of the coset representative. */

  for (kn = coseth, j = 2;  ; j++) 
    {
    /* trace out the coset rep in the coset table once more */

    kn = tc_tracew(kn, &crepx[1], &crepx[i]);
    if (kn < 1)
      {					/* we reached undefined entry */ 
      *order = 0; 
      break; 
      }
    else if (kn == 1) 
      {
      /* j is the order of the coset rep modulo the subgroup, if the
      enumeration has completed.  If not, it's a multiple thereof (?). */

      *order = j;
      break;
      }
    }

  /* convert the representative from column numbers into generators */

  for (j = 1; j <= i; j++)
    { crepx[j] = colgen[crepx[j]]; }
  }

	/******************************************************************
	static void tc_print_cosrep(void)

	Print coset rep stored in crepx[].  This has been filled by 
	tc_cosrep(), with length in [0] & data in [1...].  Note that, since
	the coset > 1, the rep is at least 1 gen long.
	******************************************************************/

static void tc_print_cosrep(void)
  {
  int j, k;

  if (ngennum != 0) 			/* numeric generators */
    { 
    fprintf(fop, "%d", crepx[1]);
    for (j = 2; j <= crepx[0]; j++) 
      { fprintf(fop, " %d", crepx[j]); }
    }
  else					/* alpha generators */
    {
    for (j = 1; j <= crepx[0]; j++) 
      { 
      k = crepx[j];
      fprintf(fop, "%c", (k > 0) ? algen[k] : toupper(algen[-k])); 
      }
    }
  }

	/******************************************************************
	void tc_print_ct(int n1, int n2, int n3)

	If n1=0, print all of the coset table.  If n1 is positive, print 
	(n1..n2 by n3) rows of the coset table.  If n1 is negative, print 
	(-n1..n2 by n3) rows of the table with coset representatives.
	******************************************************************/

void tc_print_ct(int n1, int n2, int n3)
  { 
  int i, j, row1, row2, step, o;

  tc_compact(); 
  if (!mstflg && n1 <= 0) 
    { tc_mst(); }			/* construct ms tree */

  fprintf(fop, "       "); 		/* above coset number */
  if (ngennum != 0) 			/* numeric generators */
    { 
    for (i = 1; i <= ncol; i++) 
      { fprintf(fop, "%7d", colgen[i]); }
    }
  else
    {
    for (i = 1; i <= ncol; i++) 
      { fprintf(fop, "      %c", (colgen[i] > 0) 
        ? algen[colgen[i]] : toupper(algen[-colgen[i]])); }
    }
  if (n1 < 0) 
    { fprintf(fop,"    coset rep"); }
  fprintf(fop,"\n");

  if (n1 < 0) 
    { 
    if (crepx != NULL)
      { free(crepx); }
    if ((crepx = (int *)malloc(sizeof(int)*(mstlevel+1))) == NULL)
      { tc_error("unable to allocate space for coset representative");}
    }

  if (n1 == 0) 
    { 
    row1 = 1; 
    row2 = nalive; 
    step = 1; 
    } 
  else 
    { 
    row1 = abs(n1); 
    row2 = MIN0(abs(n2), nalive); 
    step = abs(n3); 
    }

  for (i = row1; i <= row2; i += step) 
    {
    fprintf(fop, "%6d:", i);
    for (j = 1; j <= ncol; j++) 
      { fprintf(fop, "%7d", CT(i,j)); }
    if (n1 < 0 && i != 1) 
      {  
      fprintf(fop, "    ");
      tc_cosrep(i, &o); 
      tc_print_cosrep();
      }
    fprintf(fop, "\n");
    }
  }

	/******************************************************************
	void tc_add_cc(int n)

	Calculate & print rep of coset n and add rep to the subgrp gens.
	Set coset n coincident with 1, and `run' the enumeration.  Check to
	see if there's a collapse to index 1.  (Could the enumeration
	complete, but to some other index?)
	******************************************************************/

void tc_add_cc(int n)
  {
  Wordlel *p;
  int i;

  if (crepx != NULL)
    { free(crepx); }
  if ((crepx = (int *)malloc(sizeof(int)*(mstlevel+1))) == NULL)
    { tc_error("unable to allocate space for coset representative");}

  /* Get the coset representative & its length.  i returns the order 
  of the coset representative, but we ignore it here. */

  tc_cosrep(n, &i);

  /* allocate space for the new subg generator (ie, cosrep), and add
  it to the list of generators */

  p = (Wordlel *)malloc(sizeof(Wordlel));
  p->wordgen = crepx;			/* both of these start at [1] ! */
  p->len = crepx[0]; 
  crepx = NULL;				/* prevent later deallocation ! */
  p->next = NULL; 
  p->exp = 1;

  nsgpg++;

  if (sghdr == NULL)
    { sghdr = new_empty_word_list(); }
  add_word_to_word_list(sghdr, p);

  /* initialise variables for coinc processing.  */

  indexc = mstflg = FALSE;
  topded = maxrow + 1;
  /* msgctrl = 0; */			/* turn message control off ?? */

  /* process the coincidence, and check for total collapse */

  tc_coinc(1, n);
  if (index1) 
    { 
    tc_text(12); 
    indexc = TRUE;
    }
  }

	/******************************************************************
	static Logic tc_normal(int n)

	Coset n is normalising if for the subgroup H = < w_1,...,w_s >, 
	i*w_j = i (for all j=1..s).  If n is normalising return T, else F.
	******************************************************************/

static Logic tc_normal(int n)
  { 
  int s, *beg, *end;

  for (s = 1; s <= nsgpg; s++) 
    {
    beg = &subggen[subgindex[s]]; 
    end = beg - 1 + subglength[s];
    if (n != tc_tracew(n, beg, end)) 
      { return FALSE; }
    }

  return TRUE;
  }

	/******************************************************************
	void tc_sc(int n)

	Print the stabilising cosets of the subgrp.  n>0 prints the first n
	stabilising cosets. n<0 prints the first |n|, with coset reps.  n=0
	prints all stabilising cosets, plus their representatives.  Note
	that we could have a line overflow here.
	******************************************************************/

void tc_sc(int n)
  {
  int i, n1, o;
  Logic found;

  tc_compact();			/* compact table & construct ms tree */
  if (!mstflg && n <= 0) 
    { tc_mst(); }

  if (n <= 0) 
    { 
    if (crepx != NULL)
      { free(crepx); }
    if ((crepx = (int *)malloc(sizeof(int)*(mstlevel+1))) == NULL)
      { tc_error("unable to allocate space for coset representative");}
    }

  n1 = (n == 0 || abs(n) > nalive) ? nalive : abs(n);

  /* print stabilising cosets. */

  found = FALSE;
  for (i = 2; i <= n1; i++) 
    {
    if (tc_normal(i)) 
      { 				/* i is stabilising */
      if (!found) 
        { 
        if (n <= 0)
          { fprintf(fop, "Stabilising cosets (plus reps):\n"); }
        else
          { fprintf(fop, "Stabilising cosets:\n"); }
        found = TRUE; 
        } 
      fprintf(fop, " %d", i);
      if (n <= 0)
        { 
        tc_cosrep(i, &o);
        fprintf(fop, "  ");
        tc_print_cosrep();
        fprintf(fop, "\n"); 
        }
      }
    }
  if (!found) 
    { fprintf(fop, "No stabilising cosets\n"); }
  else if (n > 0)
    { fprintf(fop, "\n"); }
  }

	/******************************************************************
	void tc_o(int o)

	Find cosets with order a multiple of abs(o) modulo the subgroup.
	If o == 0, print all orders modulo the subgroup.  Otherwise, print
	the first (o positive) or all (o negative) coset numbers with
	order multiple of |o|, with their reps.
	******************************************************************/

void tc_o(int  o)
  { 
  int i, order;
  Logic found;

  if (index1) 
    { tc_error("index 1 in OO"); }
  if (!ctflg) 
    { tc_error("no coset table in OO"); }

  tc_compact();			/* compact table & construct ms tree */ 
  if (!mstflg) 
    { tc_mst(); }

  if (crepx != NULL)
    { free(crepx); }
  if ((crepx = (int *)malloc(sizeof(int)*(mstlevel+1))) == NULL)
    { tc_error("unable to allocate space for coset representative");}

  found = FALSE;
  for (i = 2; i <= nalive; i++) 
    {
    tc_cosrep(i, &order); 
    if (o == 0) 
      { fprintf(fop, "coset %d, order %d\n", i, order); } 
    else if (order % abs(o) == 0) 
      {
      fprintf(fop, "coset %d, order %d, and rep ", i, order);
      tc_print_cosrep();
      fprintf(fop, "\n");

      found = TRUE;
      if (o > 0) 
        { break; }
      }
    }

  if (!found && o != 0)
    { fprintf(fop, "no cosets with order a multiple of %d\n", abs(o)); }
  }  

	/******************************************************************
	void tc_normcl(Logic build)

	Check normal closure.  Trace i^-1 * s * i and i * s * i^-1 for all 
	group generators i and all subgroup generators s, noting whether 
	we get back to coset 1 or not.

	The original code to attempt closure (if build is TRUE) has been
	archived in cutoutA.c (used old data structures).  The user needs
	to rerun the enumerations after adding new subgrp gens.

	Note: our printout could be more informative here!
	******************************************************************/

void tc_normcl(Logic build)
  {
  int i, j, k, conji, s, length;
  int *beg, *begs, *end, *ends;
  Wordlel *p;

  newsgpar = 0;
  for (conji = 1; conji >= -1; conji -= 2)	/* s^i then s^-i */
    {
    for (i = 1; i <= ndgen; i++)		/* for all grp gens */
      {
      k = CT(1, (conji > 0) ? gencol[i] : gencol[-i]);
      for (s = 1; s <= nsgpg; s++)		/* for all subgrp gens */
        {
        beg = &subggen[subgindex[s]];
        end = beg - 1 + subglength[s];
        if (tc_tracew(k, beg, end) == k) 
          { continue; }
        else 
          {				/* output this conjugate. */
	  if (ngennum != 0) 
            { fprintf(fop, "Group generator %d^%d does not normalise"
                            " subgroup generator %d\n", i, conji, s); }
	  else 
            { fprintf(fop, "Group generator %c^%d does not normalise"
                     " subgroup generator %d\n", algen[i], conji, s); }

          /* try to force closure by adding this conjugate */

          if (build) 
            {				/* build new subgroup generator */
            length = 2 + subglength[s];	/* 2 pos'ns, i^1 & i^-1 */
            p = (Wordlptr)malloc(sizeof(*p));
            p->exp = 1; 
            p->next = 0; 
            p->len = length;
            p->wordgen = (int*)malloc((length+1)*sizeof(int));
            p->wordgen[length] = i*conji; 
            p->wordgen[1] = -i*conji; 
            for (begs = &subggen[subgindex[s]], 
                   ends = beg - 3 + length, j = 2; 
                 begs <= ends; begs++, j++) 
              { p->wordgen[j] = colgen[*begs]; }
            nsgpg++; 
            newsgpar++; 
            indexc = FALSE;
            add_word_to_word_list(sghdr, p);
	    }
          }
        }
      }
    }
  }

