
/**************************************************************************

        ex1c.c
        Colin Ramsay
        6 Feb 99

        ADAPTIVE COSET ENUMERATOR, Version 3.000

        Copyright 1999 
        Centre for Discrete Mathematics and Computing,
        Department of Mathematics and 
        Department of Computer Science & Electrical Engineering,
        The University of Queensland, QLD 4072.
	(http://www.csee.uq.edu.au/~havas/cdmc.html)

See "~cram/grp/triv/...".  Our base-line presentation is:
	gr: r,s,t;
	rel: (r^t R^2)^(t^s T^2)=(r^t R^2)^2,
	     (s^r S^2)^(r^t R^2)=(s^r S^2)^2,
	     (t^s T^2)^(s^r S^2)=(t^s T^2)^2;
We preserve the ordering of the relators, but consider all inverses &
cyclic shifts (ie, (2x25)^3 = 125000 presentations).  We are interested in
the variability of the t (total coset) count.  To keep the amount of work
(ie, time per run) under control, we enumerate over 2-generator subgroups.
Some quick tests indicate that the precise form of these (ie, "r,s" or
"r,S" or "s,R" or ...) has little effect, so we'll limit ourselves to the
generators "r,s", "r,t" & "s,t" (? could we argue that we need test only
one of these).  We'll run in "sims:3" mode (which seems to be always close
to best).  We give a `generous' amount of memory, since we want to get some
idea of the variability.  We enforce a hard limit on t by disallowing 
compaction.

**************************************************************************/

#include "al1.h"

#include <sys/types.h>
#include <time.h>
#include <string.h>

#define R   rellst
#define RL  rellst->last
#define RLW rellst->last->word

#define G   genlst
#define GL  genlst->last
#define GLW genlst->last->word

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

  subgrpname = (char *)malloc(7+1);
  strcpy(subgrpname, "Trivial");

  G = al1_newwl();

  G->first = GL = al1_newelt();
  G->len++;
  GLW = (int*)malloc((1+1)*sizeof(int));
  GLW[0] = 0;
  GLW[1] = 1;
  GL->len = 1;
  GL->exp = 1;
  GL->next = NULL;

  GL = (GL->next = al1_newelt());
  G->len++;
  GLW = (int*)malloc((1+1)*sizeof(int));
  GLW[0] = 0;
  GLW[1] = 2;
  GL->len = 1;
  GL->exp = 1;
  GL->next = NULL;
  }

void cyc_rel(int rel)
  {
  Wlelt *p;
  int j,k;

  if (rel == 0)
    { p = R->first; }
  else if (rel == 1)
    { p = R->first->next; }
  else
    { p = R->last; }

  k = p->word[1];
  for (j = 1; j <= 24; j++)
    { p->word[j] = p->word[j+1]; }
  p->word[25] = k;
  }

void inv_rel(int rel)
  {
  Wlelt *p;
  int j,k;

  if (rel == 0)
    { p = R->first; }
  else if (rel == 1)
    { p = R->first->next; }
  else
    { p = R->last; }

  for (j = 1; j <= 12; j++)
    {
    k             =  p->word[j];
    p->word[j]    = -p->word[26-j];
    p->word[26-j] = -k;
    }
  p->word[13] = -p->word[13];
  }

int main(void)
  {
  int i, aa,bb,cc, rslt;

  al1_init(); 

  fprintf(fop, "\n%s (Level 1): example programme \"c\"\n", ACE_VER);
  fprintf(fop, "Start time: %s", al0_date());
  fprintf(fop, "====================\n");

  workspace = 60;		/* max:9999998 */
  workmult  = MEGA;

  costable  = 
    (int *)malloc((long)workspace*(long)workmult*(long)sizeof(int));

  cfactor1 = 0;
  comppc   = 100;
  dedmode  = 4;
  dedsiz1  = 1000;
  mendel   = FALSE;
  rfactor1 = -1000;
  rfill    = TRUE;

  /* Initial run(s).  Full messaging, to check things. */

  msgctrl = TRUE;
  msghol  = FALSE;
  msgincr = 5000000;

  bld_pres();
  i = 0;
  fprintf(fop, "**** Run %d(r,s) ****\n", i);
  al1_rslt(al1_start(0));

  G->first->word[1] = 1;
  GLW[1] = 3;
  fprintf(fop, "**** Run %d(r,t) ****\n", i);
  al1_rslt(al1_start(0));

  G->first->word[1] = 2;
  GLW[1] = 3;
  fprintf(fop, "**** Run %d(s,t) ****\n", i);
  al1_rslt(al1_start(0));

  /* Cycled/inverted runs, with no messaging; includes a run #0. */

  msgctrl = FALSE;

  for (aa = 1; aa <= 50; aa++)
    {
    cyc_rel(0);

    for (bb = 1; bb <= 50; bb++)
      {
      cyc_rel(1);

      for (cc = 1; cc <= 50; cc++)
        {
        cyc_rel(2);

        i++;

        G->first->word[1] = 1;
        GLW[1] = 2;
        if ((rslt = al1_start(0)) > 0)
          { 
          fprintf(fop, "** Run %d(r,s) ...\n", i);
          al1_prtdetails(FALSE);
          al1_rslt(rslt);
          }

        G->first->word[1] = 1;
        GLW[1] = 3;
        if ((rslt = al1_start(0)) > 0)
          { 
          fprintf(fop, "** Run %d(r,t) ...\n", i);
          al1_prtdetails(FALSE);
          al1_rslt(rslt);
          }

        G->first->word[1] = 2;
        GLW[1] = 3;
        if ((rslt = al1_start(0)) > 0)
          { 
          fprintf(fop, "** Run %d(s,t) ...\n", i);
          al1_prtdetails(FALSE);
          al1_rslt(rslt);
          }

        if (cc == 25 || cc == 50)
          { inv_rel(2); }
        }

      if (bb == 25 || bb == 50)
        { inv_rel(1); }
      }

    if (aa == 25 || aa == 50)
      { inv_rel(0); }
    }

  fprintf(fop, "====================\n");
  fprintf(fop, "Stop time: %s\n", al0_date());

  return(0);
  }

