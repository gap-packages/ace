
/**************************************************************************

        ex0d.c
        Colin Ramsay (cram@csee.uq.edu.au)
        13 Feb 99

        ADAPTIVE COSET ENUMERATOR, Version 3.000

        Copyright 1999
        Centre for Discrete Mathematics and Computing,
        Department of Mathematics and 
        Department of Computer Science & Electrical Engineering,
        The University of Queensland, QLD 4072.
	(http://www.csee.uq.edu.au/~havas/cdmc.html)

This example of how to drive ACE Level 0, is intended as a template for
full scale testing on a `serious' example.  The code is grouped up using
ifdef's into blocks testing various thing.  You should only run one block
at a time!  The example (from GH) is interesting, since it has a large
collapse (& thus a large deduction stack), but _not_ to the final result.
It then finds a lot of `little' coincidences before it gets the final
result, so there's a lot of al0_coinc calls with `large' stacks.

Note the sneaky games we can play with maxrow/dedsiz/pdsiz & nsgpg,
provided that we don't overflow the initial allocation of memory, or upset
an enumeration in progress!

Warning: be VERY CAREFUL if you try to play the sort of games we play when
doing a `randomly' constructed run; it is very easy to do something which
invalidates the result or crashes the enumerator.  In fact, the code here
is _not_ guaranteed to be error-free/crash-proof.

**************************************************************************/

#include "al0.h"

#include <sys/types.h>
#include <time.h>

int style;			/* Feeling lazy, so use a global */

	/******************************************************************
	Routine to dump the first & last x rows of the table.
	******************************************************************/

void dmp_tab(int cnt)
  {
  int i,j;

  fprintf(fop, "        |      a      A      b      B      c      C\n");
  fprintf(fop, " -------+------------------------------------------\n");

  if (2*cnt >= nextdf)
    {
    for (i = 1; i <= nextdf; i++)
      {
      fprintf(fop, " %6d |", i);
      for (j = 1; j <= ncol; j++)
        { fprintf(fop, "%7d", CT(i,j)); }
      if (i == knr)
        { fprintf(fop, "  knr"); }
      if (i == knh)
        { fprintf(fop, "  knh"); }
      if (i == nextdf)
        { fprintf(fop, "  nextdf"); }
      fprintf(fop, "\n");
      }
    }
  else
    {
    for (i = 1; i <= cnt; i++)
      {
      fprintf(fop, " %6d |", i);
      for (j = 1; j <= ncol; j++)
        { fprintf(fop, "%7d", CT(i,j)); }
      if (i == knr)
        { fprintf(fop, "  knr"); }
      if (i == knh)
        { fprintf(fop, "  knh"); }
      if (i == nextdf)
        { fprintf(fop, "  nextdf"); }
      fprintf(fop, "\n");
      }
    fprintf(fop, "     -- |\n");
    for (i = nextdf-cnt+1; i <= nextdf; i++)
      {
      fprintf(fop, " %6d |", i);
      for (j = 1; j <= ncol; j++)
        { fprintf(fop, "%7d", CT(i,j)); }
      if (i == knr)
        { fprintf(fop, "  knr"); }
      if (i == knh)
        { fprintf(fop, "  knh"); }
      if (i == nextdf)
        { fprintf(fop, "  nextdf"); }
      fprintf(fop, "\n");
      }
    }
  }

	/******************************************************************
	Enumeration: cfd0R7 (order 2^17 = 131072);
	Group Generators: abc;
	Group Relators: aBCbac, bACbaacA, accAABab;
	******************************************************************/

void bld_pres(void)
  {
  int *p, trellen;

  pdqcol = (int *)malloc(pdsiz*sizeof(int));
  pdqrow = (int *)malloc(pdsiz*sizeof(int));

  dedrow = (int *)malloc(dedsiz*sizeof(int));
  dedcol = (int *)malloc(dedsiz*sizeof(int));

  ncol = 6;

  colptr = (int **)malloc((ncol + 1)*sizeof(int **));
  p = (int *)malloc(ncol*(maxrow+1)*sizeof(int));
  colptr[0]  = NULL;
  colptr[1]  = p;                  	/* a */
  colptr[2]  = p + (maxrow+1);   	/* A */
  colptr[3]  = p + 2*(maxrow+1);   	/* b */
  colptr[4]  = p + 3*(maxrow+1);  	/* B */
  colptr[5]  = p + 4*(maxrow+1);   	/* c */
  colptr[6]  = p + 5*(maxrow+1);  	/* C */
  col1ptr = colptr[1];
  col2ptr = colptr[2];

  invcol = (int *)malloc((ncol+1)*sizeof(int));
  invcol[0]  = 0;
  invcol[1]  = 2;   invcol[2]  = 1;
  invcol[3]  = 4;   invcol[4]  = 3;
  invcol[5]  = 6;   invcol[6]  = 5;

  ndrel = 3;
  trellen = 6+8+8;

  relexp = (int *)malloc((ndrel+1)*sizeof(int));
  relexp[0] = 0;
  relexp[1] = relexp[2] = relexp[3] = 1;

  rellen = (int *)malloc((ndrel+1)*sizeof(int));
  rellen[0] = 0;
  rellen[1] = 6;
  rellen[2] = rellen[3] = 8;

  /* The string stored in relators[] is:
	0-5   &  6-11		aBCbac aBCbac
	12-19 & 20-27		bACbaacA bACbaacA
	28-35 & 36-43		accAABab accAABab
  */

  relators = (int *)malloc(2*trellen*sizeof(int));
  relators[0] = relators[6]  = 1;
  relators[1] = relators[7]  = 4;
  relators[2] = relators[8]  = 6;
  relators[3] = relators[9]  = 3;
  relators[4] = relators[10] = 1;
  relators[5] = relators[11] = 5;

  relators[12] = relators[20] = 3;
  relators[13] = relators[21] = 2;
  relators[14] = relators[22] = 6;
  relators[15] = relators[23] = 3;
  relators[16] = relators[24] = 1;
  relators[17] = relators[25] = 1;
  relators[18] = relators[26] = 5;
  relators[19] = relators[27] = 2;

  relators[28] = relators[36] = 1;
  relators[29] = relators[37] = 5;
  relators[30] = relators[38] = 5;
  relators[31] = relators[39] = 2;
  relators[32] = relators[40] = 2;
  relators[33] = relators[41] = 4;
  relators[34] = relators[42] = 1;
  relators[35] = relators[43] = 3;

  relind = (int *)malloc((ndrel+1)*sizeof(int));
  relind[0] = -1;
  relind[1] = 0;
  relind[2] = 12;
  relind[3] = 28;

  edp = (int *)malloc(2*trellen*sizeof(int));
  edp[0]  = 0;   edp[1]  = 6;		/* a */
  edp[2]  = 4;   edp[3]  = 6;		/* a */
  edp[4]  = 16;  edp[5]  = 8;		/* a */
  edp[6]  = 17;  edp[7]  = 8;		/* a */
  edp[8]  = 28;  edp[9]  = 8;		/* a */
  edp[10] = 34;  edp[11] = 8;		/* a */
  edp[12] = 13;  edp[13] = 8;		/* A */
  edp[14] = 19;  edp[15] = 8;		/* A */
  edp[16] = 31;  edp[17] = 8;		/* A */
  edp[18] = 32;  edp[19] = 8;		/* A */
  edp[20] = 3;   edp[21] = 6;		/* b */
  edp[22] = 12;  edp[23] = 8;		/* b */
  edp[24] = 15;  edp[25] = 8;		/* b */
  edp[26] = 35;  edp[27] = 8;		/* b */
  edp[28] = 1;   edp[29] = 6;		/* B */
  edp[30] = 33;  edp[31] = 8;		/* B */
  edp[32] = 5;   edp[33] = 6;		/* c */
  edp[34] = 18;  edp[35] = 8;		/* c */
  edp[36] = 29;  edp[37] = 8;		/* c */
  edp[38] = 30;  edp[39] = 8;		/* c */
  edp[40] = 2;   edp[41] = 6;		/* C */
  edp[42] = 14;  edp[43] = 8;		/* C */

  edpbeg = (int *)malloc((ncol + 1)*sizeof(int));
  edpend = (int *)malloc((ncol + 1)*sizeof(int));
  edpbeg[0] = -1;  edpend[0]  = -1;
  edpbeg[1] = 0;   edpend[1] = 10;	/* a */
  edpbeg[2] = 12;  edpend[2] = 18;	/* A */
  edpbeg[3] = 20;  edpend[3] = 26;	/* b */
  edpbeg[4] = 28;  edpend[4] = 30;	/* B */
  edpbeg[5] = 32;  edpend[5] = 38;	/* c */
  edpbeg[6] = 40;  edpend[6] = 42;	/* C */

  nsgpg = 0;
  }

	/******************************************************************
        Subgroup name: Index 2^17, 2^14, 2^3 or 1 (with a collapse) ;
        Subgroup generators: bc, ABAAbcabC, AcccacBcA;

	We build the subgroup using all of these.  Then we have a choice 
	of 4 subgroups/indices simply by setting nsgpg to 0, 1, 2 or 3.
	This is not recommended practice, but is extremely cute!
	******************************************************************/

void bld_sgp(void)
  {
  int tsgenlen;

  nsgpg = 3;
  tsgenlen = 2+9+9;

  /*
  if (subggen != NULL)
    { free(subggen); }
  */
  subggen = (int *)malloc(tsgenlen*sizeof(int));
  subggen[0] = 3;
  subggen[1] = 5;

  subggen[2]  = 2;
  subggen[3]  = 4;
  subggen[4]  = 2;
  subggen[5]  = 2;
  subggen[6]  = 3;
  subggen[7]  = 5;
  subggen[8]  = 1;
  subggen[9]  = 3;
  subggen[10] = 6;

  subggen[11] = 2;
  subggen[12] = 5;
  subggen[13] = 5;
  subggen[14] = 5;
  subggen[15] = 1;
  subggen[16] = 5;
  subggen[17] = 4;
  subggen[18] = 5;
  subggen[19] = 2;

  /*
  if (subgindex != NULL)
    { free(subgindex); }
  */
  subgindex = (int *)malloc((nsgpg + 1)*sizeof(int));
  subgindex[0] = -1;
  subgindex[1] = 0;
  subgindex[2] = 2;
  subgindex[3] = 11;

  /*
  if (subglength != NULL)
    { free(subglength); }
  */
  subglength = (int *)malloc((nsgpg + 1)*sizeof(int));
  subglength[0] = 0;
  subglength[1] = 2;
  subglength[2] = 9;
  subglength[3] = 9;
  }

	/******************************************************************
        void bld_rnd(void)

	We want to ensure good test coverage here, but we also want to get
	a result ocassionally.  Note that we must _never_ let maxrow get
	less than the amount of the table already used!  Ditto dedsiz vis-
	a-vis topded.  Note the setting of the style global!
	******************************************************************/

void bld_rnd(void)
  {
  int tmp;

  mendel = rand()%2;			/* 0 or 1 */
  rfill  = rand()%2;			/* 0 or 1 */
  pcomp  = rand()%2;			/* 0 or 1 */

  if ((tmp = 475728 + 16*rand()) >= nextdf)
    { maxrow = tmp; }			/* 475728 ... 1000000 */

  tmp = rand()%4;			/* -:0:+ :: 1:1:2 */
  if (tmp == 0)
    { rfactor = 0; }
  else if (tmp == 1)
    { rfactor = -(rand()%4000); }
  else
    { rfactor = rand()%4000; }

  tmp = rand()%4;			/* -:0:+ :: 1:1:2 */
  if (tmp == 0)
    { cfactor = 0; }
  else if (tmp == 1)
    { cfactor = -(rand()%8000); }
  else
    { cfactor = rand()%8000; }

  /* We have to translate our Level 2 style indicator to an actual style
  number for Level 0, and make both factors >= 0. */

  if (rfactor < 0)
    {
    if (cfactor < 0)
      { rfactor = -rfactor;  cfactor = -cfactor;  style = 0; }
    else if (cfactor == 0)
      { rfactor = -rfactor;  style = 1; }
    else
      { rfactor = -rfactor;  style = 2; }
    }
  else if (rfactor == 0)
    {
    if (cfactor < 0)
      { cfactor = -cfactor;  style = 3;
      }
    else if (cfactor == 0)		/* 4 is 0, with `defaults' */
      { rfactor = 87;  cfactor = 1000;  style = 0; }
    else
      { style = 5; }
    }
  else
    {
    if (cfactor < 0)
      { cfactor = -cfactor;  style = 6; }
    else if (cfactor == 0)
      { style = 7; }
    else
      { style = 8; }
    }

  comppc  = rand()%101;			/* 0 ... 100 */
  if (comppc%2 == 1)			/* if odd ... */
    { comppc = comppc%10; }		/*   `encourage' compaction */

  nrinsgp = rand()%(ndrel+1);		/* 0 ... ndrel */
  lahead  = rand()%5;			/* 0 ...     4 */

  tlimit = rand()%13 - 1;		/* -1 ... 12 */

  hlimit = rand()%102 - 1;		/* -1 ... 100 */
  if (hlimit >= 0 && hlimit < 55)
    { hlimit = 55; }			/* too small is a nuisance */

  llimit = rand()%101;			/* 0 ... 100 */

  pdefn   = rand()%4;			/* 0 ...  3 */
  ffactor = (float)(rand()%640)/10.0;	/* 0 ... 63.9 */

  if ((tmp = 1699 + 3*rand()) >= topded)
    { dedsiz = tmp; }			/* 1699 ... 100000 */
  dedmode = rand()%5;			/*    0 ...      4 */
  }

	/******************************************************************
        int main(void)
	******************************************************************/

int main(void)
  {
  int mode, rslt, tmp;

  al0_init();

  fprintf(fop, "\n%s (Level 0): example programme \"d\"\n", ACE_VER);
  fprintf(fop, "Start time: %s", al0_date());

  srand((unsigned int)time(NULL)); 

  maxrow = 1000000;			/* wo ~ 6,000,012 */
  dedsiz = 100000;			/* 100000 physical/allowed dedns */
  pdsiz = 2048;				/* Max we'll be using! */
  bld_pres();
  bld_sgp();

#ifdef RUNA
  /* This is an attempt to emulate the equivalent "fel;" ACE 2.001 
  enumeration, over "fi:1,24;" (see test00), and the effects of pdsiz,
  dedsiz & dedmode.  Note that the first run is a `priming' run, to ensure
  that the memory is actually allocated.  Note that if dedsiz is too small,
  deductions are discarded & a "RA:" phase is instituted; this is a poor 
  man's way of implementing adaptive mode! */

  msgctrl = TRUE;
  /* msghol  = TRUE; */
  msgincr = 50000;

  /* pcomp  = TRUE; */

  pdsiz = 8; 
  /* pdsiz = 256; */
  /* pdsiz = 2048; */

  /* dedsiz = 1000; */
  dedsiz = 10000; 
  /* dedsiz = 100000; */

  fprintf(fop, "\n** pcomp = %d, pdsiz = %d, dedsiz = %d\n", 
               pcomp, pdsiz, dedsiz);

  fprintf(fop, "======== #1 ========\n");

  nsgpg = 1;
  rfactor = 0;
  cfactor = 25000;

  fprintf(fop, "** pdefn = %d\n", pdefn);

  al0_rslt(al0_enum(0,5));
  dmp_tab(8);
  al0_dump(TRUE);

  al0_rslt(al0_enum(0,5));
  STATDUMP;

  fprintf(fop, "======== #2 ========\n");

  pdefn = 3;				/* As in ACE2 */
  fprintf(fop, "** pdefn = %d\n", pdefn);

  dedmode = 0;
  fprintf(fop, "** dedmode = %d\n", dedmode);

  for (ffactor = 1.0; ffactor <= 16.0; ffactor += 0.5)
    {
    fprintf(fop, "** ffactor = %3.1f\n", ffactor);
    al0_rslt(al0_enum(0,5));
    STATDUMP;
    }

  fprintf(fop, "======== #3 ========\n");

  dedmode = 1;
  fprintf(fop, "** dedmode = %d\n", dedmode);

  for (ffactor = 1.0; ffactor <= 16.0; ffactor += 0.5)
    {
    fprintf(fop, "** ffactor = %3.1f\n", ffactor);
    al0_rslt(al0_enum(0,5));
    STATDUMP;
    }

  fprintf(fop, "======== #4 ========\n");

  dedmode = 2;
  fprintf(fop, "** dedmode = %d\n", dedmode);

  for (ffactor = 1.0; ffactor <= 16.0; ffactor += 0.5)
    {
    fprintf(fop, "** ffactor = %3.1f\n", ffactor);
    al0_rslt(al0_enum(0,5));
    STATDUMP;
    }

  fprintf(fop, "======== #5 ========\n");

  dedmode = 3;
  fprintf(fop, "** dedmode = %d\n", dedmode);

  for (ffactor = 1.0; ffactor <= 16.0; ffactor += 0.5)
    {
    fprintf(fop, "** ffactor = %3.1f\n", ffactor);
    al0_rslt(al0_enum(0,5));
    STATDUMP;
    }

  fprintf(fop, "======== #6 ========\n");

  dedmode = 4;
  fprintf(fop, "** dedmode = %d\n", dedmode);

  for (ffactor = 1.0; ffactor <= 16.0; ffactor += 0.5)
    {
    fprintf(fop, "** ffactor = %3.1f\n", ffactor);
    al0_rslt(al0_enum(0,5));
    STATDUMP;
    }
#endif

#ifdef RUNB
  /* This set of runs does an hlt enumeration; note the effect of the 
  mendel flag here; if it's on, we never need to lookahead!  Good example
  of the rfill/pcomp/mendel flags.  Also, lahead = 4 v. interesting! */

  msgctrl = TRUE;
  /* msghol  = TRUE; */
  msgincr = 50000;

  /* rfill  = FALSE; */
  /* pcomp  = TRUE; */
  /* mendel = TRUE; */

  fprintf(fop, "\n======== #1 ========\n");

  nsgpg = 1;
  rfactor = 5000;
  cfactor = 0;

  al0_rslt(al0_enum(0,7));
  dmp_tab(8);
  al0_dump(TRUE);

  for (lahead = 0; lahead <= 4; lahead ++)
    {
    fprintf(fop, "** lahead = %d\n", lahead);
    al0_rslt(al0_enum(0,7));
    STATDUMP;
    }
#endif

#ifdef RUNC
  /* This set of runs tries out CR-style. rf=35, cf=1000 & ff=6-10
  are `close' to optimal.  By tinkering with msgincr/dedsiz you can monitor
  large collapses & coinc/dedn processing therein (try 5000/10000). */

  msgctrl = TRUE;
  msgincr = 100000;
  /*hlimit  = 100;*/

  fprintf(fop, "\n======== #1 ========\n");

  nsgpg = 0;

  cfactor = 1000;
  rfactor = 35;

  nrinsgp = ndrel;

  pdefn   = 3;			/* ACE2 style ... */
  ffactor = 10.0;
  pdsiz   = 256;

  dedsiz = 1000;

  al0_rslt(al0_enum(0,8));
  dmp_tab(8);
  al0_dump(TRUE);
  STATDUMP;

  fprintf(fop, "\n======== #2 ========\n");

  for (rfactor = 30; rfactor <= 40; rfactor++)
    {
    fprintf(fop, "** rfactor = %d ...\n", rfactor);
 
  for (cfactor = 750; cfactor <= 1250; cfactor += 250)
    {
    fprintf(fop, "** cfactor = %d ...\n", cfactor);
 
  for (ffactor = 5.0; ffactor <= 10.0; ffactor += 0.5)
    {
    fprintf(fop, "** ffactor = %3.1f ...\n", ffactor);
    al0_rslt(al0_enum(0,8));
    }
    }
    }
#endif

#ifdef RUND
  /* This gives the system a good thrashing, using a run consisting of a
  `random' series of continues/redos.  Note that we must take care to
  ensure that a valid `start' phase is performed, so that the subgroup
  generators are processed.  We also make sure that a valid `redo' phase 
  is done at the end. */

  msgctrl = TRUE;
  msghol  = TRUE;
  msgincr = 10000;

  nsgpg = rand()%4; 

  tmp = rand()%10;		/* change pdsiz to 2^(11 - tmp) */
  while (tmp-- > 0)
    { pdsiz /= 2; }

  /* Initial start call. */

  fprintf(fop, "\n======== #1 ========\n");

  do
    {
    bld_rnd();

    fprintf(fop, "*** Start mode, style = %d ***\n", style);
    al0_dump(FALSE);
    rslt = al0_enum(0,style);
    al0_rslt(rslt);
    STATDUMP;
    }
  while(rslt <= -260);			/* need to catch subgroup gens! */
  dmp_tab(8);

  /* Succession of continue/redo calls. */

  fprintf(fop, "\n======== #2 ========\n");

  while(rslt <= 0)
    {
    bld_rnd();

    if ((mode = rand()%10) == 0)	/* continue:redo :: 9:1 */
      { 
      mode = 2;
      fprintf(fop, "*** Redo mode, style = %d ***\n", style);
      }
    else
      {
      fprintf(fop, "*** Continue mode, style = %d ***\n", style);
      mode = 1;
      }

    al0_dump(FALSE);
    rslt = al0_enum(mode,style);
    al0_rslt(rslt);
    STATDUMP;
    }

  /* Final redo (ie, check) call. */

  fprintf(fop, "\n======== #3 ========\n");

  do
    {
    bld_rnd();

    tlimit = -1;
    hlimit = -1;

    fprintf(fop, "*** Check mode, style = %d ***\n", style);
    al0_dump(FALSE);
    rslt = al0_enum(2,style);
    al0_rslt(rslt);
    STATDUMP;
    }
  while(rslt <= 0);
#endif

#ifdef RUNE
  /* This tests the `default' mode (ie, R/C-style).  Good illustration of
  the various dedn stack options; in particular, compacting it is bad. 
  Note the aliasing of dedsiz, since dedmode=4 can change it. */

  msgctrl = TRUE;
  msgincr = 35000;
  /*hlimit  = 80;*/

  fprintf(fop, "\n======== #1 ========\n");

  nsgpg = 2;
  maxrow = 250000;

  al0_rslt(al0_enum(0,0));
  al0_dump(TRUE);
  dmp_tab(8);
  STATDUMP;

  for (tmp = 100; tmp <= 100000; tmp *= 10)
    {
    for (dedmode = 0; dedmode <= 4; dedmode++)
      {
      dedsiz = tmp;
      fprintf(fop, "*** dedsiz = %d, dedmode = %d\n", dedsiz, dedmode);
      al0_rslt(al0_enum(0,0));
      }
    }
#endif

#ifdef RUNF
  /* This run tests the ability to add subgroup generators. */

  msgctrl = TRUE;
  /*hlimit  = 100;*/

  fprintf(fop, "\n======== #1 ========\n");

  cfactor = 950;
  rfactor = 36;
  nrinsgp = ndrel;

  pdefn   = 3;
  pdsiz   = 256;
  ffactor = 8.0;

  nsgpg = 0;
  msgincr = 50000;
  al0_rslt(al0_enum(0,8));
  STATDUMP;

  nsgpg = 1;
  msgincr = 10000;
  al0_rslt(al0_enum(2,8));
  STATDUMP;

  nsgpg = 2;
  msgincr = 1000;
  al0_rslt(al0_enum(2,8));
  STATDUMP;

  nsgpg = 3;
  msgincr = 100;
  al0_rslt(al0_enum(2,8));
  STATDUMP;
#endif

#ifdef RUNG
  /* This run tests tinkering with the pdl size.  Note that this overflows
  if we don't make preferred definitions!  Note also the ability (currently
  mostly commented out!) to find out the overhead of the (timing-based) 
  messaging & (limit-based) termination tests vis-a-vis the ct/rt 
  `blocking' factors; note also how totcos changes as we alter ct/rt! */

  fprintf(fop, "\n======== #1 ========\n");

  msgctrl = TRUE;
  /*msghol  = TRUE;*/
  msgincr = 50000;

  /* This `activates' these features, but ensures that they will never be
  `triggered'. */

  /*hlimit  = 100;*/
  /*tlimit  = 100;*/
  /*llimit  = 100000;*/

  cfactor = 950;
  rfactor = 36;

  nrinsgp = ndrel;
  nsgpg   = 0;

  dedmode = 4;
  dedsiz  = 250;

  pdefn = 0;
  al0_rslt(al0_enum(0,8));
  STATDUMP;

  fprintf(fop, "\n======== #2 ========\n");

  pdefn = 3;
  ffactor = 6.0;

  cfactor = 95;
  rfactor = 4;
  fprintf(fop, "*** cfactor = %d, rfactor = %d\n", cfactor, rfactor);

  for (pdsiz = 2048; pdsiz >= 2; pdsiz /= 2)
    {
    dedsiz = 250;
    fprintf(fop, "*** pdsiz = %d\n", pdsiz);
    al0_rslt(al0_enum(0,8));
    }

  fprintf(fop, "\n======== #3 ========\n");

  cfactor = 950;
  rfactor = 36;
  fprintf(fop, "*** cfactor = %d, rfactor = %d\n", cfactor, rfactor);

  for (pdsiz = 2048; pdsiz >= 2; pdsiz /= 2)
    {
    dedsiz = 250;
    fprintf(fop, "*** pdsiz = %d\n", pdsiz);
    al0_rslt(al0_enum(0,8));
    }

  fprintf(fop, "\n======== #4 ========\n");

  cfactor = 9500;
  rfactor = 360;
  fprintf(fop, "*** cfactor = %d, rfactor = %d\n", cfactor, rfactor);

  for (pdsiz = 2048; pdsiz >= 2; pdsiz /= 2)
    {
    dedsiz = 250;
    fprintf(fop, "*** pdsiz = %d\n", pdsiz);
    al0_rslt(al0_enum(0,8));
    }

#endif

  fprintf(fop, "\n====================\n");

  fprintf(fop, "\nStop time: %s\n", al0_date());

  return(0);
  }

