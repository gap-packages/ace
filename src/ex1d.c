
/*******************************************************************************

  ex1d.c
  6 Feb 99, 1 May 14
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


The group used is a well-known example from an infinite series of 'hard' pres'ns
of the trivial group: see "Proving a group trivial made easy: a case study in 
coset enumeration", by Havas & Ramsay (Bull. of the AMS).  Our base-line 
presentation is:
	gr: r,s,t;
	rel: (r^t R^2)^(t^s T^2)=(r^t R^2)^2,
	     (s^r S^2)^(r^t R^2)=(s^r S^2)^2,
	     (t^s T^2)^(s^r S^2)=(t^s T^2)^2;

We are interested in finding the 'easiest' equivalent presentation.  We do this 
by looking for easy enumerations over a 1-generator subgroup; we know that this 
can be done with a total of less than 10M cosets.  We preserve the ordering of 
the relators, but consider all inverses & cyclic shifts (ie, 2^3 x 25^3 = 125000
presentations) and all the subgroups <r>, <s> & <t>.  We'll run in "sims:3" mode
(which seems to be always close to best).  We enforce a hard limit on totcos by 
disallowing compaction.

notes:
- This may take a little time to complete.  On my test machine, it did ~4 runs
  per min (1 run = 3 x enum (over <r>, <s> & <t>).
- Might be better to do some number of random runs, said number being selected
  by an argument.
- This programme is ripe for parallelisation.

*******************************************************************************/

#include "al1.h"

#include <time.h>
#include <string.h>

/*******************************************************************************
Stuff from level 2 or elsewhere ...
*******************************************************************************/

SInt mode;                         /* cf. the "mode" argument to al1_start() */

static char *datetime(void)
  {
  time_t t = time(NULL);
  return ctime(&t);
  }

/*******************************************************************************
Useful aliases ...
*******************************************************************************/

#define R   rellst
#define RL  rellst->last
#define RLW rellst->last->word

#define G   genlst
#define GL  genlst->last
#define GLW genlst->last->word

/*******************************************************************************
Build the presentation and set the subgroup gen'r to "r".
*******************************************************************************/

void bld_pres(void)
  {
  int i;

  grpname = (char *)malloc(7+1);
  strcpy(grpname, "Trivial");

  ndgen = 3;
  galpha = TRUE;

  for (i = 0; i < 28; i++) { algen[i] = '\0'; }
  algen[1] = 'r';
  algen[2] = 's';
  algen[3] = 't';

  for (i = 0; i < 27; i++) { genal[i] = 0; }
  genal[1] = 1;
  genal[2] = 2;
  genal[3] = 3;

  R = al1_newwl();

  R->first = RL = al1_newelt();
  R->len++;
  RLW = (SInt*)malloc((25+1)*sizeof(SInt));
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
  RLW = (SInt*)malloc((25+1)*sizeof(SInt));
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
  RLW = (SInt*)malloc((25+1)*sizeof(SInt));
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
  GLW = (SInt*)malloc((1+1)*sizeof(SInt));
  GLW[0] = 0;
  GLW[1] = 1;
  GL->len = 1;
  GL->exp = 1;
  GL->next = NULL;
  }

void cyc_rel(int rel)
  {
  Wlelt *p;
  int j;
  SInt k;

  if      (rel == 0) { p = R->first; }
  else if (rel == 1) { p = R->first->next; }
  else               { p = R->last; }

  k = p->word[1];
  for (j = 1; j <= 24; j++) { p->word[j] = p->word[j+1]; }
  p->word[25] = k;
  }

void inv_rel(int rel)
  {
  Wlelt *p;
  int j;
  SInt k;

  if      (rel == 0) { p = R->first; }
  else if (rel == 1) { p = R->first->next; }
  else               { p = R->last; }

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
  int i, aa,bb,cc, b;
  Coset rslt;

  printf("\n");
  printf("%s (Level 1): example programme \"d\"\n", ACEVER);
  printf("Start time: %s", datetime());
  for (b = 0; b < 5; b++) { printf("---------------"); }
  printf("\n");

  al1_init(); 

  workspace = 60;                      /* ie, max:9999998 */
  workmult  = MEGA;

  costable  = 
       (Entry*)malloc((size_t)workspace*(size_t)workmult*(size_t)sizeof(Entry));

  cfactor1 = 0;
  comppc   = 100;                      /* disallow compaction */
  dedmode  = 4;
  dedsiz1  = 1000;
  mendel   = FALSE;
  rfactor1 = -1000;
  rfill    = TRUE;

  /* Initial run(s).  Full messaging, to check things. */

  msgctrl = TRUE;
  msgincr = 5000000;

  bld_pres();
  i = 0;
  mode = 0;

  printf("\n");
  printf("** Run %d, <r> ...\n", i);
  printf("\n");
  al1_rslt(al1_start(mode));

  G->first->word[1] = 2;

  printf("\n");
  printf("** Run %d, <s> ...\n", i);
  printf("\n");
  al1_rslt(al1_start(mode));

  G->first->word[1] = 3;

  printf("\n");
  printf("** Run %d, <t> ...\n", i);
  printf("\n");
  al1_rslt(al1_start(mode));

  /* Cycled/inverted runs, with no messaging; one run matches run #0.   TBA ...
  this does 125000 runs, but does it do each possibility *exactly* once? */

  msgctrl = FALSE;
  printf("\n");

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
        if (i%25 == 1) { printf("** Run %d ...\n", i); }

        G->first->word[1] = 1;
        if ((rslt = al1_start(mode)) > 0)
          { 
          printf("** Run %d, <r> ...\n", i);
          al1_prtdetails(3);
          al1_prtdetails(5);
          al1_rslt(rslt);
          }

        G->first->word[1] = 2;
        if ((rslt = al1_start(mode)) > 0)
          { 
          printf("** Run %d, <s> ...\n", i);
          al1_prtdetails(3);
          al1_prtdetails(5);
          al1_rslt(rslt);
          }

        G->first->word[1] = 3;
        if ((rslt = al1_start(mode)) > 0)
          { 
          printf("** Run %d, <t> ...\n", i);
          al1_prtdetails(3);
          al1_prtdetails(5);
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

  printf ("\n");
  for (b = 0; b < 5; b++) { printf("---------------"); }
  printf("\n");
  printf("Stop time: %s", datetime());
  printf ("\n");

  exit(EXIT_SUCCESS);
  }

