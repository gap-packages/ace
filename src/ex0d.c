
/*******************************************************************************

  ex0d.c
  13 Feb 99, 30 Apr 14
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


This example of how to drive ACE Level 0 can be used as a template for full-
scale testing on a 'serious' example.  The code is blocked up into functions,
each testing various things.  The block to run is select by the programme's
argument.

The example presentation (from George Havas) is interesting, since it has a 
large collapse (and thus a large deduction stack), but *not* to the final 
result.  It then finds a lot of 'little' coincidences before it gets the final
result, so there're a lot of al0_coinc() calls with 'large' stacks.

Note the sneaky games we can play with, eg, the maxrow/dedsiz/pdsiz and nsgpg 
parameters, provided that we don't overflow the initial allocation of memory, 
or upset an enumeration in progress.

WARNING: be very careful if you try to play the sort of games we play when doing
a 'randomly' constructed run.  It is very easy to do something which invalidates
the result or crashes the enumerator.  In fact, all the code in this example is 
*not* guaranteed to be error-free/crash-proof, and is somewhat 'fragile'.

notes:
- Some of the doRun*() routines can generate lotsa output.
- There is nothing magic about many of the parameters used.  You can adjust them
  as you see fit; many suggested values can be commented in/out.
- The first run in a series of runs can be regared as a 'priming' run, in the 
  sense that although the memory for the table has been allocated it is not 
  resident until it's 'used'.  So the first run takes longer than the subsequent
  ones.
- With messaging on, setting msgincr to a 'large' value suppresses the 'normal'
  definition/deduction/coincidence lines and leaves the 'higher level' lines
  such as SG, CL, CO, RA, DS, etc.
- With messaging on, setting msgincr to a 'small' value (eg, 1) allows most (eg,
  all) of the enumerator's actions to be monitored.  It can, however, generate
  *lots* of output.

*******************************************************************************/

#include "al0.h"

#include <time.h>

/*******************************************************************************
Useful stuff adapted from other levels and/or previous versions of ACE ...
*******************************************************************************/

SInt mode;                         /* cf. the "mode" argument to al1_start() */
SInt style;                        /* cf. the "style" local in al1_start() */

char *datetime(void)               /* from Level 2 */
  {
  time_t t = time(NULL);
  return ctime(&t);
  }

/*******************************************************************************
From ACE 3.001, Level 0.  Note that the values printed are those at the time of
the call.  These may not reflect the values used for a start/continue/redo; for
these, call al0_dump() *immediately* beforehand and/or afterwards.
*******************************************************************************/

void al0_dump(void)             
  {
  printf("begintime=%4.2f endtime=%4.2f deltatime=%4.2f totaltime=%4.2f\n",
                                      begintime, endtime, deltatime, totaltime);

  printf("msgctrl=%d msgincr=%"PS" msgnext=%"PS, msgctrl, msgincr, msgnext);
  printf(" mendel=%d rfill=%d pcomp=%d\n", mendel, rfill, pcomp);

  printf("maxrow=%"PC" rfactor=%"PS" cfactor=%"PS"", maxrow, rfactor, cfactor);
  printf(" comppc=%"PS" nrinsgp=%"PS" lahead=%"PS"\n", comppc, nrinsgp, lahead);

  printf("llimit=%"PS" lcount=%"PS, llimit, lcount);
  printf(" nalive=%"PC" maxcos=%"PC" totcos=%"PB"\n", nalive, maxcos, totcos);

  printf("chead=%"PC" ctail=%"PC, chead, ctail);
  if (chead == 0) { printf(" (empty)"); }
  else            { printf(" (non-empty)"); }
  printf(" pdefn=%d pdsiz=%"PS" ffactor=%3.1f\n", pdefn, pdsiz, ffactor);

  printf("toppd=%"PS" botpd=%"PS"", toppd, botpd);
  if (toppd == botpd) { printf(" (empty)"); }
  else                { printf(" (non-empty)"); }
  printf(" knr=%"PC" knh=%"PC" nextdf=%"PC"\n", knr, knh, nextdf);

  printf("dedsiz=%"PS" dedmode=%"PS" disded=%d topded=%"PS"", 
                                               dedsiz, dedmode, disded, topded);
  if (topded < 0) { printf(" (empty)\n"); }
  else            { printf(" (non-empty)\n"); }

  if (edp == NULL) { printf("edp=NULL"); }
  else             { printf("edp=non-NULL"); }
  printf(" ncol=%"PS, ncol);
  if (colptr == NULL) { printf(" colptr[]=NULL\n"); }
  else                { printf(" colptr[]=non-NULL\n"); }

  printf("ndrel=%"PS, ndrel);
  if (relators == NULL) { printf(" relators[]=NULL"); }
  else                  { printf(" relators[]=non-NULL"); }
  printf(" nsgpg=%"PS" sgdone=%d", nsgpg, sgdone);
  if (subggen == NULL) { printf(" subggen[]=NULL\n"); }
  else                 { printf(" subggen[]=non-NULL\n"); }

  }

/*******************************************************************************
Routine to dump the first & last x rows of the table.  Indicates the knr, knh &
nextdf (*not* part of the table) rows if present, and includes any redundant
(ie, coincident) cosets.
*******************************************************************************/

void dmp_tab(int cnt)
  {
  Coset i;
  SInt j;

  printf("        |      a      A      b      B      c      C\n");
  printf(" -------+------------------------------------------\n");

  if (2*cnt >= nextdf)
    {
    for (i = 1; i <= nextdf; i++)
      {
      printf(" %6"PC" |", i);
      for (j = 1; j <= ncol; j++) { printf(" %6"PC, (Coset)CT(i,j)); }
      if (i == knr)   { printf("  knr"); }
      if (i == knh)   { printf("  knh"); }
      if (i == nextdf){ printf("  nextdf"); }
      printf("\n");
      }
    }
  else
    {
    for (i = 1; i <= cnt; i++)
      {
      printf(" %6"PC" |", i);
      for (j = 1; j <= ncol; j++) { printf(" %6"PC, (Coset)CT(i,j)); }
      if (i == knr)    { printf("  knr"); }
      if (i == knh)    { printf("  knh"); }
      if (i == nextdf) { printf("  nextdf"); }
      printf("\n");
      }
    printf("     -- |\n");
    for (i = nextdf-cnt+1; i <= nextdf; i++)
      {
      printf(" %6"PC" |", i);
      for (j = 1; j <= ncol; j++) { printf(" %6"PC, (Coset)CT(i,j)); }
      if (i == knr)   { printf("  knr"); }
      if (i == knh)   { printf("  knh"); }
      if (i == nextdf){ printf("  nextdf"); }
      printf("\n");
      }
    }
  }

/*******************************************************************************
Enumeration: cfd0R7 (order 2^17 = 131072);
Group Generators: abc;
Group Relators: aBCbac, bACbaacA, accAABab;

Note that the coset table memory is allocated here, since we have to know the 
number of table columns, as well as maxrow, before we can do this.
*******************************************************************************/

void bld_pres(void)
  {
  Entry *p;
  SInt trellen;

  ncol = 6;

  p = (Entry *)malloc(ncol*(maxrow+1)*sizeof(Entry));
  colptr = (Entry **)malloc((ncol + 1)*sizeof(Entry *));

  colptr[0]  = NULL;                       /* not used */
  colptr[1]  = p;                          /* a */
  colptr[2]  = p + (maxrow+1);             /* A */
  colptr[3]  = p + 2*(maxrow+1);           /* b */
  colptr[4]  = p + 3*(maxrow+1);           /* B */
  colptr[5]  = p + 4*(maxrow+1);           /* c */
  colptr[6]  = p + 5*(maxrow+1);           /* C */

  col1ptr = colptr[1];
  col2ptr = colptr[2];

  invcol = (SInt *)malloc((ncol+1)*sizeof(SInt));

  invcol[0]  = 0;                          /* not used */
  invcol[1]  = 2;   invcol[2]  = 1;
  invcol[3]  = 4;   invcol[4]  = 3;
  invcol[5]  = 6;   invcol[6]  = 5;

  ndrel = 3;
  trellen = 6+8+8;

  relexp = (SInt *)malloc((ndrel+1)*sizeof(SInt));

  relexp[0] = 0;                           /* not used */
  relexp[1] = relexp[2] = relexp[3] = 1;

  rellen = (SInt *)malloc((ndrel+1)*sizeof(SInt));

  rellen[0] = 0;                           /* not used */
  rellen[1] = 6;
  rellen[2] = rellen[3] = 8;

  /* The string stored in relators[] is:
       0-5   &  6-11       aBCbac aBCbac
       12-19 & 20-27       bACbaacA bACbaacA
       28-35 & 36-43       accAABab accAABab */

  relators = (SInt *)malloc(2*trellen*sizeof(SInt));

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

  relind = (SInt *)malloc((ndrel+1)*sizeof(SInt));

  relind[0] = -1;                          /* not used */
  relind[1] = 0;
  relind[2] = 12;
  relind[3] = 28;

  edp = (SInt *)malloc(2*trellen*sizeof(SInt));

  edp[0]  = 0;   edp[1]  = 6;                /* a */
  edp[2]  = 4;   edp[3]  = 6;                /* a */
  edp[4]  = 16;  edp[5]  = 8;                /* a */
  edp[6]  = 17;  edp[7]  = 8;                /* a */
  edp[8]  = 28;  edp[9]  = 8;                /* a */
  edp[10] = 34;  edp[11] = 8;                /* a */
  edp[12] = 13;  edp[13] = 8;                /* A */
  edp[14] = 19;  edp[15] = 8;                /* A */
  edp[16] = 31;  edp[17] = 8;                /* A */
  edp[18] = 32;  edp[19] = 8;                /* A */
  edp[20] = 3;   edp[21] = 6;                /* b */
  edp[22] = 12;  edp[23] = 8;                /* b */
  edp[24] = 15;  edp[25] = 8;                /* b */
  edp[26] = 35;  edp[27] = 8;                /* b */
  edp[28] = 1;   edp[29] = 6;                /* B */
  edp[30] = 33;  edp[31] = 8;                /* B */
  edp[32] = 5;   edp[33] = 6;                /* c */
  edp[34] = 18;  edp[35] = 8;                /* c */
  edp[36] = 29;  edp[37] = 8;                /* c */
  edp[38] = 30;  edp[39] = 8;                /* c */
  edp[40] = 2;   edp[41] = 6;                /* C */
  edp[42] = 14;  edp[43] = 8;                /* C */

  edpbeg = (SInt *)malloc((ncol + 1)*sizeof(SInt));
  edpend = (SInt *)malloc((ncol + 1)*sizeof(SInt));

  edpbeg[0] = -1;  edpend[0]  = -1;
  edpbeg[1] = 0;   edpend[1] = 10;        /* a */
  edpbeg[2] = 12;  edpend[2] = 18;        /* A */
  edpbeg[3] = 20;  edpend[3] = 26;        /* b */
  edpbeg[4] = 28;  edpend[4] = 30;        /* B */
  edpbeg[5] = 32;  edpend[5] = 38;        /* c */
  edpbeg[6] = 40;  edpend[6] = 42;        /* C */
  }

/*******************************************************************************
Subgroup name: Index 2^17, 2^14, 2^3 or 1 (with a collapse);
Subgroup generators: bc, ABAAbcabC, AcccacBcA;

We build the subgroup using all three of these.  Then we have a choice of 4 
subgroups/indices simply by setting nsgpg to 0, 1, 2 or 3.  This is not 
recommended practice, but is extremely cute.
*******************************************************************************/

void bld_sgp(void)
  {
  SInt tgenlen;

  nsgpg = 3;
  tgenlen = 2+9+9;

  subggen = (SInt *)malloc(tgenlen*sizeof(SInt));
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

  subgindex = (SInt *)malloc((nsgpg + 1)*sizeof(SInt));
  subgindex[0] = -1;                           /* not used */
  subgindex[1] = 0;
  subgindex[2] = 2;
  subgindex[3] = 11;

  subglength = (SInt *)malloc((nsgpg + 1)*sizeof(SInt));
  subglength[0] = 0;                           /* not used */
  subglength[1] = 2;
  subglength[2] = 9;
  subglength[3] = 9;
  }

/*******************************************************************************
Each of the doRun*() routines wraps up the details of a particular test / 
demonstration of ACE Level 0.  They all assume that things have been setup /
initialised 'appropriately' ...
*******************************************************************************/

/*******************************************************************************
doRunA() uses style 5 ("pure c", "felsch") and investigates the effects, over a 
range of fill factors, of varying pdsiz, dedsiz & dedmode (and other stuff).
Note that if dedsiz is too small, deductions are discarded & an "RA:" phase is 
instituted; this is a poor man's way of implementing adaptive mode (see pre-ACE 
3.001 versions).
*******************************************************************************/

void doRunA(void)
  {
  printf("\n");
  printf("Investigates Felsch style for the index 2^14 enumeration\n");
  printf("over <bc>, using a selection of different parameters.\n");

  mode = 0;                    /* start mode */
  style = 5;                   /* Felsch */

  nsgpg = 1;                   /* index 2^14, over <bc> */

  rfactor = 0;
  cfactor = 25000;

  msgctrl = TRUE;
  msgincr = 50000;

/*pdsiz = 8;    */
  pdsiz = 256;  
/*pdsiz = 2048; */

/*dedsiz = 1000;   */
/*dedsiz = 10000;  */
  dedsiz = 100000; 

  dedmode = 4;

  pdefn = FALSE;
  ffactor = 1.0;
  nrinsgp = 0;

  printf("\n");
  printf("** Basic Felsch ...\n");
  printf("\n");
  al0_dump();

  printf("\n");
  al0_rslt(al0_enum(mode, style));
  printf("\n");
  dmp_tab(8);

  pdefn = TRUE;
  ffactor = 0.0;
  nrinsgp = ndrel;

  printf("\n");
  printf("** Enhanced Felsch ...\n");
  printf("\n");
  al0_dump();

  printf("\n");
  al0_rslt(al0_enum(mode, style));
  printf("\n");
  dmp_tab(8);

  msgincr = 10000000;

  for (dedmode = 0; dedmode <= 4; dedmode++)
    {
    printf("\n");
    printf("** dedmode = %"PS" ...\n", dedmode);
    printf("\n");

    for (ffactor = 1.0; ffactor <= 16.0; ffactor += 1.0)
      {
      printf("** ffactor = %3.1f ...\n", ffactor);
      al0_rslt(al0_enum(0,5));
      }    
    }

  dedmode = 4;

  printf("\n");
  printf("** dedmode = %"PS" ...\n", dedmode);
  printf("\n");

  for (ffactor = 4.0; ffactor <= 6.0; ffactor += 0.1)
    {
    printf("** ffactor = %3.1f ...\n", ffactor);
    al0_rslt(al0_enum(0,5));
    }    
  }

/*******************************************************************************
This set of runs does an hlt enumeration.  Note the effect of the mendel flag 
here; if it's on, we never need to lookahead.  Good example of the rfill, pcomp
& mendel flags.  Also, lahead = 4 is v. interesting.
*******************************************************************************/

void doRunB(void)
  { 
  int m, r, p;

  printf("\n** Does an HLT enumeration, with varying parameters.\n");

  msgctrl = TRUE;
/*msgincr = 50000;*/
  msgincr = 5000000;

  nsgpg = 1;
  rfactor = 5000;
  cfactor = 0;

  mode = 0;
  style = 7;

  for (m = 0; m <= 1; m++)
    {
    if (m) { mendel = TRUE; } else { mendel = FALSE; }
    for (r = 0; r <= 1; r++)
      {
      if (r) { rfill = TRUE; } else { rfill = FALSE; }
      for (p = 0; p <= 1; p++)
        {
        if (p) { pcomp = TRUE; } else { pcomp = FALSE; }

  printf("\n");
  printf("mendel=%d  rfill=%d  pcomp=%d ...\n", mendel, rfill, pcomp);
  printf("\n");

  for (lahead = 0; lahead <= 4; lahead ++)
    {
    printf("** lahead = %"PS" ...\n", lahead);
    al0_rslt(al0_enum(mode, style));
    }

    }  }  }
  }

/*******************************************************************************
This set of runs tries out CR-style.  Parameters of rf=35, cf=1000 and ff=7.5
are 'good', but not the best possible.  By tinkering with msgincr/dedsiz you can
monitor large collapses and coinc/dedn processing therein (try 5000/10000; note,
lots of output).
*******************************************************************************/

void doRunC(void)
  {
  printf("\n");
  printf("This set of runs tries out CR-style.\n");

  msgctrl = TRUE;
  msgincr = 100000;

  nsgpg = 0;

  cfactor = 1000;
  rfactor = 35;

  nrinsgp = ndrel; 

  pdefn   = TRUE;               
  ffactor = 7.5;
  pdsiz   = 256;

  dedsiz = 1000;

  printf("\n");
  al0_dump();
  printf("\n");
  al0_rslt(al0_enum(0,8));

  msgctrl = FALSE;
/*msgincr = 5000;*/
/*dedsiz = 10000;*/

  for (rfactor = 30; rfactor <= 40; rfactor += 2)
    {
    printf("\n");
    printf("** rfactor = %"PS" ...\n", rfactor);
 
  for (cfactor = 750; cfactor <= 1250; cfactor += 100)
    {
    printf("\n");
    printf("** cfactor = %"PS" ...\n", cfactor);
 
  for (ffactor = 5.0; ffactor <= 10.0; ffactor += 1.0)
    {
    printf("** ffactor = %3.1f ...\n", ffactor);
    al0_rslt(al0_enum(0,8));
    }
    }
    }
  }

/*******************************************************************************
This is a utility routine for doRunD().  Do *not* use elsewhere without thinking
carefully and then editing this routine.

We want to ensure good test coverage in doRunD(), but we also want to get a 
result occasionally.  Note the setting of the style global via cfactor and
rfactor, as in al1_start().  The values of msgctrl, msgincr, nsgpg, pdsiz,
dedsiz, dedsiz and the mode are set (randomly) in doRunD().
*******************************************************************************/

void bld_rnd(void)
  {
  int tmp;

  if (rand()%2) { mendel = TRUE; } else { mendel = FALSE; }
  if (rand()%2) { rfill  = TRUE; } else { rfill  = FALSE; }
  if (rand()%2) { pcomp  = TRUE; } else { pcomp  = FALSE; }

  tmp = rand()%4;                           /* -ve:0:+ve :: 1:1:2 */
  if      (tmp == 0) { rfactor = 0; }
  else if (tmp == 1) { rfactor = -1 -(rand()%4000); }
  else               { rfactor =  1 + rand()%4000; }

  tmp = rand()%4;                           /* -ve:0:+ve :: 1:1:2 */
  if      (tmp == 0) { cfactor = 0; }
  else if (tmp == 1) { cfactor = -1 -(rand()%8000); }
  else               { cfactor =  1 + rand()%8000; }

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
    if (cfactor < 0)                        /* 3 (C*) --> 5 (C) */
      { cfactor = -cfactor;  style = 5; }
    else if (cfactor == 0)                  /* 4 --> 0, with 'defaults' */
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

  comppc = rand()%101;                      /* 0 ... 100 */
  if (comppc%2 == 1)                        /* if odd ... */
    { comppc = comppc%10; }                 /*   'encourage' compaction */

  nrinsgp = rand()%(ndrel+1);               /* 0 ... ndrel */
  lahead  = rand()%5;                       /* 0 ... 4 */

  llimit = rand()%101;                      /* 0 ... 100 */

  if (rand()%2) { pdefn  = TRUE; } 
  else          { pdefn  = FALSE; }
  ffactor = (float)(rand()%640)/10.0;       /* 0.0 ... 63.9 */

  dedmode = rand()%5;                       /* 0 ... 4 */
  }

/*******************************************************************************
This gives the system a good thrashing, using a run consisting of a start, a
random' series of continues & redos, and a final redo.  Note that we must take 
care to ensure that a valid 'start' phase is performed, so that the subgroup
generators are processed.  We also make sure that a valid 'redo' phase is done 
at the end.  The memory allocation is tight for the index 2^17 enum'n, but it 
*can* complete.  More often than not, it'll simply try forever (and be aborted).

TBA: 
- if rand() is 32 bit, then the setting of maxrow & dedsiz is not correct
*******************************************************************************/
 
void doRunD(void)
  {
  int tmp, cnt;
  Coset rslt;

  printf("\n");
  printf("Does an enumeration over <1>, <bc>, <bc, ABAAbcabC> or \n");
  printf("<bc, ABAAbcabC, AcccacBcA> via a random series of continues\n");
  printf("and redos.  May not succeed (ie, loops forever), in which\n");
  printf("case we bail out.\n");

  msgctrl = TRUE;
  msgincr = 10000;

  maxrow = 1000000 - (rand()%524273);       /* 475728 ... 1000000 */

  tmp = rand()%10;                          /* change pdsiz to 2^(11 - tmp) */
  while (tmp-- > 0) { pdsiz /= 2; }

  dedsiz = 100000 - (rand()%98302);         /* 1699 ... 100000 */

  nsgpg = rand()%4;                         /* 0 .. 3 */

  cnt = 0;

  /* Initial start call(s). */

  printf("\n");
  do
    {
    if (++cnt > 250)
      {
      printf("\n** Limit on number of enumeration phases exceeded\n");
      return;
      }

    bld_rnd();
    printf("** Start mode, style = %"PS" ...\n", style);
    rslt = al0_enum(0,style);
    al0_rslt(rslt);
    }
  while (rslt <= -260);                     /* need to catch subgroup gens */

  /* Succession of continue/redo calls. */

  printf("\n");
  while (rslt <= 0)
    {
    if (++cnt > 250)
      {
      printf("\n** Limit on number of enumeration phases exceeded\n");
      return;
      }

    bld_rnd();

    if ((mode = rand()%10) == 0)            /* continue:redo :: 9:1 */
      { 
      mode = 2;
      printf("** Redo mode, style = %"PS" ...\n", style);
      }
    else
      {
      printf("** Continue mode, style = %"PS" ...\n", style);
      mode = 1;
      }

    rslt = al0_enum(mode,style);
    al0_rslt(rslt);
    }

  /* Final redo (ie, check) call(s). */

  printf("\n");
  do
    {
    if (++cnt > 250)
      {
      printf("\n** Limit on number of enumeration phases exceeded\n");
      return;
      }

    bld_rnd();
    printf("** Check mode, style = %"PS" ...\n", style);
    rslt = al0_enum(2,style);
    al0_rslt(rslt);
    }
  while (rslt <= 0);
  }

/*******************************************************************************
This tests the 'default' mode (ie, R/C-style).  Good illustration of the various
deduction stack options.  In particular, compacting it is bad (note the running 
times).   Note the aliasing of dedsiz (via tmp), since dedmode=4 can change it.
*******************************************************************************/

void doRunE(void)
  {
  SInt tmp;

  printf("\n");
  printf("Tests the default (ie, R/C) setting, under various\n");
  printf(" deduction stack sizes and handling modes.\n");

  msgctrl = TRUE;
  msgincr = 50000;

/*nsgpg = 2;*/
  nsgpg = 1;
  maxrow = 600000;

  printf("\n");
  al0_dump();
  printf("\n");
  al0_rslt(al0_enum(0,0));
  printf("\n");
  dmp_tab(8);

/*msgctrl = FALSE;*/
  msgincr = 1000000;

  for (tmp = 10; tmp <= 100000; tmp *= 10)
    {
    for (dedmode = 0; dedmode <= 4; dedmode++)
      {
      dedsiz = tmp;

      if (dedmode == 0)
        {
        printf("\n");
        printf("** dedsiz = %"PS" ...\n", dedsiz);
        }

      printf("** dedmode = %"PS" ...\n", dedmode);
      al0_rslt(al0_enum(0,0));
      }
    }
  }

/*******************************************************************************
This run tests the ability to add subgroup generators.  We use CR-style (ie,
alternate Felsch & HLT passes), and do a redo (*not* a continue) for all but the
1st run (which is a start).
*******************************************************************************/

void doRunF(void)
  {
  printf("\n");
  printf("Successively adds the subgroup generators and does a redo.\n");

  msgctrl = TRUE;

  cfactor = 950;
  rfactor = 36;
  nrinsgp = ndrel;

  pdefn   = TRUE;
  pdsiz   = 256;
  ffactor = 8.0;

  style = 8;                           /* CR-style */
  mode = 0;                            /* start mode */

  nsgpg = 0;
  msgincr = 50000;

  printf("\n");
  al0_dump();
  printf("\n");
  al0_rslt(al0_enum(mode, style));

  mode = 2;                            /* redo mode */

  nsgpg = 1;
  msgincr = 10000;

  printf("\n");
  al0_dump();
  printf("\n");
  al0_rslt(al0_enum(mode, style));

  nsgpg = 2;
  msgincr = 1000;

  printf("\n");
  al0_dump();
  printf("\n");
  al0_rslt(al0_enum(mode, style));
  printf("\n");
  dmp_tab(10);

  nsgpg = 3;
  msgincr = 100;

  printf("\n");
  al0_dump();
  printf("\n");
  al0_rslt(al0_enum(mode, style));
  printf("\n");
  dmp_tab(10);

  printf("\n");
  al0_dump();
  }

/*******************************************************************************
This run tests tinkering with the pdl size.  Note that this overflows if we 
don't make preferred definitions.  Note also totcos changes as we alter ct/rt.
*******************************************************************************/

void doRunG(void)
  {
  printf("\n");
  printf("This run tests tinkering with the pdl size,\n");
  printf("and the values of the R- and C-factors.\n");

  msgctrl = FALSE;

  cfactor = 950;
  rfactor = 36;

  nrinsgp = ndrel;
  nsgpg   = 0;

  dedmode = 4;
  dedsiz  = 250;

  pdefn = FALSE;

  printf("\n");
  printf("** Preferred definitions disabled ...\n");

  printf("\n");
  printf("** cfactor = %"PS", rfactor = %"PS" ...\n", cfactor, rfactor);

  al0_rslt(al0_enum(0,8));

  pdefn = TRUE;
  ffactor = 6.0;

  printf("\n");
  printf("** Preferred definitions enabled ...\n");

  cfactor = 95;
  rfactor = 4;

  printf("\n");
  printf("** cfactor = %"PS", rfactor = %"PS" ...\n", cfactor, rfactor);

  for (pdsiz = 2048; pdsiz >= 2; pdsiz /= 2)
    {
    printf("** pdsiz = %"PS" ...\n", pdsiz);
    al0_rslt(al0_enum(0,8));
    }

  cfactor = 950;
  rfactor = 36;

  printf("\n");
  printf("** cfactor = %"PS", rfactor = %"PS" ...\n", cfactor, rfactor);

  for (pdsiz = 2048; pdsiz >= 2; pdsiz /= 2)
    {
    printf("** pdsiz = %"PS" ...\n", pdsiz);
    al0_rslt(al0_enum(0,8));
    }

  cfactor = 9500;
  rfactor = 360;

  printf("\n");
  printf("** cfactor = %"PS", rfactor = %"PS" ...\n", cfactor, rfactor);

  for (pdsiz = 2048; pdsiz >= 2; pdsiz /= 2)
    {
    printf("** pdsiz = %"PS" ...\n", pdsiz);
    al0_rslt(al0_enum(0,8));
    }
  }

/*******************************************************************************
The main routine parses the argument, sets up the enumerator, and then calls the
requested Level 0 test.
*******************************************************************************/

int main(int argc, char *argv[])
  {
  int a, b;

  if (argc == 2)
    {
    a = atoi(argv[1]);
    if (a < 1 || a > 7)
      {
      printf("** argument must be in range 1...7\n");
      exit(EXIT_FAILURE);
      }
    }
  else
    {
    printf("** usage: ./%s 1...7\n", argv[0]);
    exit(EXIT_FAILURE);
    }

  printf("\n");
  printf("%s (Level 0): example programme \"d\"\n", ACEVER);
  printf("Start time: %s", datetime());
  for (b = 0; b < 5; b++) { printf("---------------"); }
  printf("\n");

  /* Initialise Level 0 & the system, set the *maximum* values we'll be using 
  for some stuff, allocate some stuff, build the presentation and subgroup data
  structures. */

  al0_init();

  srand((unsigned int)time(NULL)); 

  pdsiz = 2048;                            /* max we'll be using */
  pdqrow = (Coset *)malloc(pdsiz*sizeof(Coset));
  pdqcol = (SInt *)malloc(pdsiz*sizeof(SInt));

  dedsiz = 100000;                         /* 100000 physical/allowed dedns */
  dedrow = (Coset *)malloc(dedsiz*sizeof(Coset));
  dedcol = (SInt *)malloc(dedsiz*sizeof(SInt));

  maxrow = 1000000;                        /* ie, workspace ~= 6,000,012 */

  bld_pres();
  bld_sgp();

  /* Run the requested test */

  switch (a)
    {
    case 1:  doRunA();  break;
    case 2:  doRunB();  break;
    case 3:  doRunC();  break;
    case 4:  doRunD();  break;
    case 5:  doRunE();  break;
    case 6:  doRunF();  break;
    case 7:  doRunG();  break;
    }

  printf ("\n");
  for (b = 0; b < 5; b++) { printf("---------------"); }
  printf("\n");
  printf("Stop time: %s", datetime());
  printf ("\n");

  exit(EXIT_SUCCESS);
  }

