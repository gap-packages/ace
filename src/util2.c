
/*****************************************************************************

  util2.c
  27 Oct 03, 19 Apr 14
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


Various utilities for the "demonstration interactive interface"; ie, Level 2.

*****************************************************************************/

#include "al2.h"

#include <time.h>

/*****************************************************************************
  void al2_init(void)

One-off initialisation of the Level 2 stuff, and all lower levels.  Note that 
there is no need to initialise, for example, intarr[].  lresult is init'd to
`invalid mode' (just in case).  Note the seeding of the random num gen'r.  We
need some sort of not-repeatability across a succession of runs, so we use the
current time; we don't need crypto style randomness, statistical is way good
enough.
*****************************************************************************/

void al2_init(void)
  {
  al1_init();

  okstart = okcont   = okredo = FALSE;
  tabinfo = tabindex = FALSE;
  lresult = -8192;

  echo   = FALSE;
  skipnl = TRUE;

  currip = currkey[0] = currname[0] = '\0';

  currword = NULL;
  currsiz  = currexp = 0;

  intcnt = 0;

  srand((unsigned int)time(NULL));
  }

/*******************************************************************************
  void al2_opt(void)

Pretty-prints some build specific info ...

The __DATE__ & __TIME__ macros from the compiler give when *this* file was 
compiled.  This may not be when the executable was built (if, eg, seperate *.o
files were generated).  Sizeof() returns a size_t type, which may not be int (it
may be long or long long).  Hence the casts to prevent some compilers spitting
out a warning.  We pick up CLK32 or CLK64 from the compiler's command line, if
we can.
*******************************************************************************/

void al2_opt(void)
  {
  printf("%s executable built: %s on %s\n", ACEVER, __TIME__, __DATE__);

  printf("BInt, SInt: %d, %d    ", (int)sizeof(BInt), (int)sizeof(SInt));
  printf("Coset, Entry: %d, %d", (int)sizeof(Coset), (int)sizeof(Entry));

#if defined (CLK32)
  printf("    CLK: 32");
#elif defined (CLK64)
  printf("    CLK: 64");
#else
  printf("    CLK: unknown");
#endif

  printf("\n");
  }

/*****************************************************************************
  void al2_help(void)

TBA ... this is a bit overwhelming, split into sections?
*****************************************************************************/

void al2_help(void)
  {
  printf("  -- Level 2 Help ------------------------------------\n");
  printf("add gen[erators] / sg : <word list> ;\n");
  printf("add rel[ators] / rl : <relation list> ;\n");
  printf("aep : 1..7 ;\n");
  printf("as[is] : [0/1] ;\n");
  printf("beg[in] / end / start ;\n");
  printf("bye / exit / q[uit] ;\n");
  printf("cc / coset coinc[idence] : int ;\n");
  printf("c[factor] / ct[ factor] : [int] ;\n");
  printf("check / redo ;\n");
  printf("com[paction] : [0..100] ;\n");
  printf("cont[inue] ;\n");
  printf("cy[cles] ;\n");
  printf("ded mo[de] / dmod[e] : [0..4] ;\n");
  printf("ded si[ze] / dsiz[e] : [0/1..] ;\n");
  printf("def[ault] ;\n");
  printf("del gen[erators] / ds : <int list> ;\n");
  printf("del rel[ators] / dr : <int list> ;\n");
  printf("easy ;\n");
  printf("echo : [0/1] ;\n");
  printf("enum[eration] / group name : <string> ;\n");
  printf("fel[sch] : [0/1] ;\n");
  printf("f[factor] / fi[ll factor] : [0/1..] ;\n");
  printf("gen[erators] / subgroup gen[erators] : <word list> ;\n");
  printf("gr[oup generators]: [<letter list> / int] ;\n");
  printf("group relators / rel[ators] : <relation list> ;\n");
  printf("hard ;\n");
  printf("h[elp] ;\n");
  printf("hlt ;\n");
  printf("look[ahead] : [0/1..4] ;\n");
  printf("loop[ limit] : [0/1..] ;\n");
  printf("max[ cosets] : [0/2..] ;\n");
  printf("mend[elsohn] : [0/1] ;\n");
  printf("mess[ages] / mon[itor] : [0/+int] ;\n");
  printf("mo[de] ;\n");
  printf("nc / normal[ closure] : [0/1] ;\n");
  printf("no[ relators in subgroup] : [-1/0/1..] ;\n");
  printf("oo / order[ option] : int ;\n");
  printf("opt[ions] ;\n");
  printf("path[ compression] : [0/1] ;\n");
  printf("pd[efinitions] : [0/1] ;\n");
  printf("pd si[ze] / psiz[e] : [0/2/4/8/...] ;\n");
  printf("print det[ails] / sr : [int] ;\n");
  printf("pr[int table] : [[-]int[,[-]int]] ;\n");
  printf("pure c[t] ;\n");
  printf("pure r[t] ;\n");
  printf("rc / random coinc[idences]: int[,int] ;\n");
  printf("rec[over] / contig[uous] ;\n");
  printf("rep : 1..7[,int] ;\n");
  printf("r[factor] / rt[ factor] : [int] ;\n");
  printf("row[ filling] : [0/1] ;\n");
  printf("sc / stabil[ising cosets] : int ;\n");
  printf("sims : 1/3/5/7/9 ;\n");
  printf("st[andard table] ;\n");
  printf("style ;\n");
  printf("subg[roup name] : <string> ;\n");
  printf("sys[tem] : <string> ;\n");
  printf("text : <string> ;\n");
  printf("tw / trace[ word] : int,<word> ;\n");
  printf("wo[rkspace] : [int[k/m/g]] ;\n");
  printf("# ... <newline> - a comment (ignored)\n");
  printf("  ----------------------------------------------------\n");
  }

/*****************************************************************************
  void al2_nextip(void)

Primes currip with the next character from stdin, if we're not at the end-of-
file.  Echoes the character if echo is on.
*****************************************************************************/

void al2_nextip(void)
  {
  if (currip != EOF) 
    { 
    currip = getchar(); 
    if (echo && currip != EOF)  { putchar(currip); }
    }
  }

/*****************************************************************************
  void al2_skipws(void)

Skip all whitespace characters (incl. comments).
*****************************************************************************/

void al2_skipws(void)
  {
  Logic comment = (currip == '#');

  while ( currip == ' ' || currip == '\t' || comment 
          || (skipnl && (currip == '\n' || currip == '\r')) ) 
    {
    al2_nextip();
    comment = ( currip == '#' || 
            (comment && currip != '\n' && currip != '\r' && currip != EOF) );
    }
  }

/*****************************************************************************
  void al2_nextnw(void)

Skip to the next non-whitespace character.
*****************************************************************************/

void al2_nextnw(void)
  { al2_nextip();  al2_skipws(); }

