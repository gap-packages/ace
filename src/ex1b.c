
/**************************************************************************

        ex1b.c
        Colin Ramsay
        6 Feb 99

        ADAPTIVE COSET ENUMERATOR, Version 3.000

        Copyright 1999 
        Centre for Discrete Mathematics and Computing,
        Department of Mathematics and 
        Department of Computer Science & Electrical Engineering,
        The University of Queensland, QLD 4072.
	(http://www.csee.uq.edu.au/~havas/cdmc.html)

This is a `version' of test013.in, which investigates the variation in
performance as the presentation is varied; ie, reorder/invert/cycle the
relators, and select a different subgroup generator.  Our base-line 
presentation is:
	gr: r,s,t;
	rel: (r^t R^2)^(t^s T^2)=(r^t R^2)^2,
	     (s^r S^2)^(r^t R^2)=(s^r S^2)^2,
	     (t^s T^2)^(s^r S^2)=(t^s T^2)^2;
	gen: t;
We don't investigate alternative base-lines; eg, conjugate with next-in-
order, as opposed to previous-in-order.  We run this in our "hard" 
emulation of "sims:3", ie, ~"hard; mend:0; c:low; r:bigger;".  We limit 
memory tightly, and `forbid' compaction, since we know that it can be done
in a total of <10M cosets.

Best result so far ("sims:3;" works ok too, and is better) ...

**** Run 148 ****
  --- ACE 3.000: Run Parameters ---
Group Name: Trivial;
Group Generators: rst;
Group Relators: rrTRtRsrSSTrtRRssRSrssRSr, ssRSrttSTsRsrSSStsTTStsTT, 
  TrtRRTrtRRttSTsrrTRtStsTT;
Subgroup Name: Identity;
Subgroup Generators: t;
Wo:750M; Max:124999998; Mess:500000000; Ti:-1; Ho:-1; Loop:0
As:0; Path:0; Row:1; Mend:0; No:0; Look:1; Com:100;
C:2; R:64; Fi:10; PMod:0; PSiz:256; DMod:4; DSiz:1000;
  ---------------------------------
SG: a=1 r=1 h=1 n=2; l=1 c=+0.00; m=1 t=1
DS: a=959 r=16 h=1 n=960; l=4 c=+0.00; s=2000 d=1001 c=0
DS: a=1912 r=31 h=1 n=1913; l=4 c=+0.00; s=4000 d=2001 c=0
DS: a=3817 r=63 h=1 n=3818; l=4 c=+0.01; s=8000 d=4001 c=0
INDEX = 1 (a=1 r=2 h=2 n=2; l=3966 c=89.56; m=1102796 t=7756834)

**************************************************************************/

#include "al1.h"

#include <sys/types.h>
#include <time.h>
#include <string.h>

#define R   rellst
#define RL  rellst->last
#define RLW rellst->last->word

void bld_pres(void)
  {
  int i;

  grpname = (char *)malloc(7+1);
  strcpy(grpname, "Trivial");

  ndgen = 3;
  galpha = TRUE;

  for (i = 0; i < 27; i++) 
    { algen[i] = '\0'; }
  algen[1] = 'r';
  algen[2] = 's';
  algen[3] = 't';

  for (i = 0; i < 27; i++) 
    { genal[i] = 0; }
  genal[1] = 1;
  genal[2] = 2;
  genal[3] = 3;

  R = al1_newwl();

  R->first = RL = al1_newelt();
  R->len++;
  RLW = (int*)malloc((25+1)*sizeof(int));
  RLW[0]  = 0;
  RLW[1]  =  3;  RLW[2]  = 3;  RLW[3]  = -2;  RLW[4]  = -3;  RLW[5]  =  2;
  RLW[6]  = -3;  RLW[7]  = 1;  RLW[8]  =  3;  RLW[9]  = -1;  RLW[10] = -1;
  RLW[11] = -2;  RLW[12] = 3;  RLW[13] =  2;  RLW[14] = -3;  RLW[15] = -3;
  RLW[16] =  1;  RLW[17] = 1;  RLW[18] = -3;  RLW[19] = -1;  RLW[20] =  3;
  RLW[21] =  1;  RLW[22] = 1;  RLW[23] = -3;  RLW[24] = -1;  RLW[25] =  3;
  RL->len = 25;
  RL->exp = 1;
  RL->next = NULL;

  RL = (RL->next = al1_newelt());
  R->len++;
  RLW = (int*)malloc((25+1)*sizeof(int));
  RLW[0]  = 0;
  RLW[1]  =  1;  RLW[2]  = 1;  RLW[3]  = -3;  RLW[4]  = -1;  RLW[5]  =  3;
  RLW[6]  = -1;  RLW[7]  = 2;  RLW[8]  =  1;  RLW[9]  = -2;  RLW[10] = -2;
  RLW[11] = -3;  RLW[12] = 1;  RLW[13] =  3;  RLW[14] = -1;  RLW[15] = -1;
  RLW[16] =  2;  RLW[17] = 2;  RLW[18] = -1;  RLW[19] = -2;  RLW[20] =  1;
  RLW[21] =  2;  RLW[22] = 2;  RLW[23] = -1;  RLW[24] = -2;  RLW[25] =  1;
  RL->len = 25;
  RL->exp = 1;
  RL->next = NULL;

  RL = (RL->next = al1_newelt());
  R->len++;
  RLW = (int*)malloc((25+1)*sizeof(int));
  RLW[0]  = 0;
  RLW[1]  =  2;  RLW[2]  = 2;  RLW[3]  = -1;  RLW[4]  = -2;  RLW[5]  =  1;
  RLW[6]  = -2;  RLW[7]  = 3;  RLW[8]  =  2;  RLW[9]  = -3;  RLW[10] = -3;
  RLW[11] = -1;  RLW[12] = 2;  RLW[13] =  1;  RLW[14] = -2;  RLW[15] = -2;
  RLW[16] =  3;  RLW[17] = 3;  RLW[18] = -2;  RLW[19] = -3;  RLW[20] =  2;
  RLW[21] =  3;  RLW[22] = 3;  RLW[23] = -2;  RLW[24] = -3;  RLW[25] =  2;
  RL->len = 25;
  RL->exp = 1;
  RL->next = NULL;

  subgrpname = (char *)malloc(8+1);
  strcpy(subgrpname, "Identity");

  genlst = al1_newwl();

  genlst->first = genlst->last = al1_newelt();
  genlst->len++;
  genlst->last->word = (int*)malloc((1+1)*sizeof(int));
  genlst->last->word[0] = 0;
  genlst->last->word[1] = 3;
  genlst->last->len = 1;
  genlst->last->exp = 1;
  genlst->last->next = NULL;
  }

void munge_rel(void)
  {
  Wlelt *p[3], *t;
  int i,j,k,l, w[26];

  /* swap a pair of relators (numbered 0, 1 & 2) */

  p[0] = R->first;
  p[1] = p[0]->next;
  p[2] = p[1]->next;

  i = rand()%3;
  while ((j = rand()%3) == i)
    { ; }

  t = p[i];
  p[i] = p[j];
  p[j] = t;

  R->first   = p[0];
  p[0]->next = p[1];
  p[1]->next = p[2];
  p[2]->next = NULL;
  R->last    = p[2];

  /* invert each of the relators (maybe) */

  for (i = 0; i < 3; i++)
    {
    if (rand()%2 == 1)
      {
      for (j = 1; j <= 12; j++)
        {
        k                =  p[i]->word[j];
        p[i]->word[j]    = -p[i]->word[26-j];
        p[i]->word[26-j] = -k;
        }
      p[i]->word[13] = -p[i]->word[13];
      }
    }
  
  /* cycle each of the relators (maybe by 0 positions) */

  for (i = 0; i < 3; i++)
    {
    for (j = 1; j <= 25; j++)
      { w[j] = p[i]->word[j]; }

    k = rand()%25;
    for (j = 1; j <= 25; j++)
      { 
      if ((l = j+k) > 25)
        { l -= 25; }
      p[i]->word[j] = w[l];
      }
    }
  }

void munge_gen(void)
  {
  int i;

  if ((i = rand()%6-2) == 0)
    { i = -3; }

  genlst->last->word[1] = i;
  }

int main(void)
  {
  int i, rslt;

  al1_init();
  srand((unsigned int)time(NULL)); 

  fprintf(fop, "\n%s (Level 1): example programme \"b\"\n", ACE_VER);
  fprintf(fop, "Start time: %s", al0_date());
  fprintf(fop, "====================\n");

  workspace = 60;
  workmult  = MEGA;

  costable  = 
    (int *)malloc((long)workspace*(long)workmult*(long)sizeof(int));

  cfactor1 = 1;
  comppc   = 100;
  dedmode  = 4;
  dedsiz1  = 1000;
  mendel   = FALSE;
  pdefn    = 0;
  rfactor1 = 5;
  rfill    = TRUE;
  nrinsgp1 = 0;

  msgctrl = TRUE;
  msghol  = FALSE;
  msgincr = 5000000;

  bld_pres();

  i = 0;
  fprintf(fop, "**** Run %d ****\n", i);
  al1_rslt(al1_start(0));

  msgctrl = FALSE;

  for (i = 1; i <= 4096; i++)
    {
    munge_gen();
    munge_rel();

    if ((rslt = al1_start(0)) > 0)
      { 
      fprintf(fop, "** Run %d ...\n", i);
      al1_prtdetails(FALSE);
      al1_rslt(rslt);
      /*STATDUMP;*/
      }
    }

  fprintf(fop, "====================\n");
  fprintf(fop, "Stop time: %s\n", al0_date());

  return(0);
  }

