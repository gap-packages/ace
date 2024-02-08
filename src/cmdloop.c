
/*******************************************************************************

  cmdloop.c
  1 Nov 03, 23 May 14
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


Dispatcher code for stand-alone ACE.  We try to ensure that we only change 
things in response to a command if the entire command is ok.  This means that 
the state is always consistent, and we can usually just continue; the loop is
written as a set of "if() ..." statements, each of which (apart from the quit
cmd) contains a continue.  The original was a single "if .. else if ..."
statement, which gave some compilers indigestion.

On error, we exit via a jump to the main() routine and a restart.  The 
_continue() routine is intended for cases where an `error' does not affect the
ability to continue, while _restart() is intended for errors which (may) mean 
that continuing is not possible, so we have to (re)start an enumeration.  I'm 
not sure that I'm always careful in calling the `right' one; we may have to 
tinker with this in the light of experience.

*******************************************************************************/

#include "al2.h"

#include <ctype.h>
#include <string.h>

/*****************************************************************************
  void al2_cmdloop(void)
*****************************************************************************/

void al2_cmdloop(void)
  {
  BInt i,j,k;  Wlist *p;  Logic li, lj;

  while (TRUE)
    {
    /* Flush o/p before processing next command. */

    fflush(stdout);

    /* Do the necessary for the next command (or quit if end-of-file).  The 
    next command may follow on the same line, or we may have to skip over a 
    '\n' to the next line, or skip over a comment & newline.  '\n' is usually 
    a cmd terminator, but is WS for multi-line cmds. */

    al2_nextnw();  skipnl = TRUE;
    al2_skipws();  skipnl = FALSE;

    if (currip == EOF)  { break; }

    /* Read a string (ie, the cmd keyword) into currkey, plow through the if's
    until the first match, do it & then skip to the end of the while(). */
 
    al2_readkey();

    /* augment list of subgroup generators ... */
    if (al2_match("add gen[erators]") || al2_match("sg"))
      {
      if (ndgen < 1)  { al2_continue("there are no generators as yet"); }

      skipnl = TRUE;  al2_skipws();  p = al2_rdwl();  al2_endcmd();

      if (genlst == NULL)  { genlst = p; }
      else                 { al1_concatwl(genlst,p); }

      nsgpg = genlst->len;

      okcont  = FALSE;
      tabinfo = tabindex = FALSE;

      continue;
      }

    /* augment list of relators ... */
    if (al2_match("add rel[ators]") || al2_match("rl"))
      {
      if (ndgen < 1)  { al2_continue("there are no generators as yet"); }

      skipnl = TRUE;  al2_skipws();  p = al2_rdrl();  al2_endcmd();

      if (rellst == NULL)  { rellst = p; }
      else                 { al1_concatwl(rellst,p); }

      ndrel = rellst->len;

      okcont   = FALSE;
      tabindex = FALSE;

      continue;
      }

    /* all equivalent presentations ... */
    if (al2_match("aep"))
      {
      al2_readia();  al2_endcmd();

      if (intcnt != 1)  { al2_continue("bad number of parameters"); }
      if (intarr[0] < 1 || intarr[0] > 7)
        { al2_continue("invalid argument"); }

      if (!okstart)
        { al2_continue("can't start (no generators/workspace)"); }
      if (rellst == NULL || rellst->len == 0)
        { al2_continue("can't start (no relators)"); }

      al2_aep(intarr[0]);

      continue;
      }

    /* TBA: What to do with asis in continue/redo?  It's (current) value in a
    printout may not match that actually used at the start of a run, when the 
    involutary generators are picked up & the columns allocated (these 
    settings are frozen until the next start/begin/end). */

    if (al2_match("as[is]")) 
      { 
      al2_readia();  al2_endcmd();

      if ( (intcnt > 0 && (intarr[0] < 0 || intarr[0] > 1)) || intcnt > 1 )
        { al2_continue("bad parameter"); }
      else if (intcnt == 0)
        { printf("asis = %s\n", asis ? "true" : "false"); }
      else
        { asis = (intarr[0] == 1); }

      continue;
      }

    if (al2_match("beg[in]") || al2_match("end") || al2_match("start"))
      {
      al2_endcmd();

      if (!okstart)  { al2_continue("can't start (no generators?)"); }

      al1_rslt( lresult = al1_start(0) );

      /* If something `sensible' happened, then it'll be ok to continue or 
      redo this run.  If not, then we make sure that we must begin a new run. 
      Note that here (& in continue/redo) we play it safe by enforcing a new 
      run, even if there may be no need to.  Note that the SG phase is 1st in 
      start mode, so should *always* be done; failure to complete the SG phase
      is regarded as an error. */

      if (lresult > 0 && sgdone)		/* finite index */
        {
        okcont  = okredo   = TRUE;
        tabinfo = tabindex = TRUE;
        }
      else if (lresult >= -259 && sgdone)	/* overflow/limit */
        { 
        okcont   = okredo = TRUE;
        tabinfo  = TRUE;
        tabindex = FALSE;
        }
      else					/* SG overflow/`error' */
        {
        okcont  = okredo   = FALSE;
        tabinfo = tabindex = FALSE;
        }

      continue;
      }

    /* done - break out of the while() ... */
    if (al2_match("bye") || al2_match("exit") || al2_match("q[uit]"))
      { al2_endcmd();  break; }

    /* force coset coinc to #1 (ie, subgroup) ... */
    if (al2_match("cc") || al2_match("coset coinc[idence]"))
      {
      al2_readia();  al2_endcmd();

      if (intcnt != 1)  { al2_continue("bad number of parameters"); }
      if (!tabinfo)     { al2_continue("there is no table information"); }
      if (intarr[0] < 2 || intarr[0] >= nextdf || COL1(intarr[0]) < 0)
        { al2_continue("invalid/redundant coset number"); }

      al2_cc(intarr[0]);

      continue;
      }

    /* CT (ie, Felsch) coset defn factor ... */
    if (al2_match("c[factor]") || al2_match("ct[ factor]"))
      {
      al2_readia();  al2_endcmd();

      if      (intcnt > 1)   { al2_continue("bad parameter"); }
      else if (intcnt == 0)  { printf("ct factor = %"PS"\n", cfactor1); }
      else                   { cfactor1 = intarr[0]; }

      continue;
      }

    /* see comments for "begin" ... */
    if (al2_match("check") || al2_match("redo"))
      {
      al2_endcmd();

      if (!okredo)  { al2_continue("can't redo (different presentation?)"); }

      al1_rslt( lresult = al1_start(2) );

      if (lresult > 0 && sgdone)
        { okcont = TRUE;  tabinfo = tabindex = TRUE; }
      else if (lresult >= -259 && sgdone)
        { okcont = TRUE;  tabinfo = TRUE;  tabindex = FALSE; }
      else
        { okcont = FALSE;  tabinfo = tabindex = FALSE; }

      if (lresult < -260)  { okredo = FALSE; }

      continue;
      }

    /* set % empty threshold for CT compaction ... */
    if (al2_match("com[paction]"))
      {
      al2_readia();  al2_endcmd();

      if ( (intcnt > 0 && (intarr[0] < 0 || intarr[0] > 100)) || intcnt > 1 )
        { al2_continue("bad parameter"); }
      else if (intcnt == 0)
        { printf("compaction = %"PS"\n", comppc); }
      else
        { comppc = intarr[0]; }

      continue;
      }

    /* see comments for "begin" ... */
    if (al2_match("con[tinue]"))
      {
      al2_endcmd();

      if (!okcont)  
        { al2_continue("can't continue (altered presentation?)"); }

      al1_rslt( lresult = al1_start(1) );

      if (lresult > 0 && sgdone)
        { tabinfo = tabindex = TRUE; }
      else if (lresult >= -259 && sgdone)
        { tabinfo = TRUE;  tabindex = FALSE; }
      else
        { okcont = FALSE;  tabinfo = tabindex = FALSE; }

      continue;
      }

    /* print CT as permutations ... */
    if (al2_match("cy[cles]"))
      {
      al2_endcmd();

      if (!tabindex)  { al2_continue("there is no completed table"); }

      begintime = al0_clock();  li = al0_compact();  endtime = al0_clock();
      if (li)  { printf("CO"); }
      else     { printf("co"); }
      printf(": a=%"PC" r=%"PC" h=%"PC" n=%"PC"; c=+%4.2f\n", 
                   nalive, knr, knh, nextdf, al0_diff(begintime,endtime));

      al2_cycles();

      continue;
      }

    /* set deduction handling mode ... */
    if (al2_match("ded mo[de]") || al2_match("dmod[e]"))
      {
      al2_readia();  al2_endcmd();

      if (intcnt == 0)  { printf("deduction mode = %"PS"\n", dedmode); }
      else if (intcnt == 1)
        {
        if (intarr[0] < 0 || intarr[0] > 4)
          { al2_continue("bad mode parameter"); }
        dedmode = intarr[0];
        }
      else  { al2_continue("bad parameter count"); }

      continue;
      }

    /* set (initial) dedn stack size ... */
    if (al2_match("ded si[ze]") || al2_match("dsiz[e]"))
      {
      al2_readia();  al2_endcmd();

      if ( (intcnt > 0 && intarr[0] < 0) || intcnt > 1 )
                             { al2_continue("bad parameter"); }
      else if (intcnt == 0)  { printf("deduction stack = %"PS"\n", dedsiz1); }
      else                   { dedsiz1 = intarr[0]; }

      continue;
      }

    /* default mode (best choice?), matches Lev 0/1 init()'s + main() ... */
    if (al2_match("def[ault]"))
      {
      al2_endcmd();

      cfactor1 =     0;  comppc =  10;  dedmode  =     4;  dedsiz1  = 1000;
      ffactor1 =     0;  lahead =   0;  mendel   = FALSE;  nrinsgp1 =   -1;
      pdefn    =  TRUE;  pdsiz1 = 256;  rfactor1 =     0;  rfill    = TRUE;
      pcomp    = FALSE;

      continue;
      }

    /* delete (numbered) subgrp gen's from list ... */
    if (al2_match("del gen[erators]") || al2_match("ds"))
      {
      al2_readia();  al2_endcmd();

      if (intcnt < 1 || genlst == NULL || genlst->len < 1)
        { al2_continue("empty argument list / generator list"); }
      al2_dw(genlst);
      nsgpg = genlst->len;

      okcont  = okredo   = FALSE;
      tabinfo = tabindex = FALSE;

      continue;
      }

    /* delete (numbered) relators from list ... */
    if (al2_match("del rel[ators]") || al2_match("dr"))
      {
      al2_readia();  al2_endcmd();
 
      if (intcnt < 1 || rellst == NULL || rellst->len < 1)
        { al2_continue("empty argument list / relator list"); }
      al2_dw(rellst);
      ndrel = rellst->len;

      okcont  = okredo   = FALSE;
      tabinfo = tabindex = FALSE;

      continue;
      }

    /* pure Rt mode + rfill ... */
    if (al2_match("easy"))
      {
      al2_endcmd();

      cfactor1 =     0;  comppc = 100;  dedmode  =     0;  dedsiz1  = 1000;
      ffactor1 =     1;  lahead =   0;  mendel   = FALSE;  nrinsgp1 =    0;
      pdefn    = FALSE;  pdsiz1 = 256;  rfactor1 =  1000;  rfill    = TRUE;
      pcomp    = FALSE;

      continue;
      }

    /* turn cmd echoing on/off ... */
    if (al2_match("echo")) 
      { 
      al2_readia();  al2_endcmd();

      if ( (intcnt > 0 && (intarr[0] < 0 || intarr[0] > 1)) || intcnt > 1 )
        { al2_continue("bad parameter"); }
      else if (intcnt == 0)
        { printf("echo = %s\n", echo ? "true" : "false"); }
      else
        { echo = (intarr[0] == 1); }

      continue;
      }

    /* give grp non-default (ie, "G") name; "" is ok ... */
    if (al2_match("enum[eration]") || al2_match("group name"))
      {
      al2_readname();  al2_endcmd();

      if (grpname != NULL)  { free(grpname); }
      if (( grpname = (char*)malloc(strlen(currname)+1) ) == NULL)
        { al2_continue("out of memory in grpname copy"); }
      strcpy(grpname, currname);

      continue;
      }

    /* Felsch (pure Ct + comppc) & preferred defns / rel's in subgrp ... */
    if (al2_match("fel[sch]"))
      {
      al2_readia();  al2_endcmd();

      if ( (intcnt > 0 && (intarr[0] < 0 || intarr[0] > 1)) || intcnt > 1 )
        { al2_continue("bad parameter"); }
      
      if (intcnt == 1 && intarr[0] == 1)		/* enhanced */
        { ffactor1 = 0;  nrinsgp1 = -1;  pdefn = TRUE; }
      else						/* basic */
        { ffactor1 = 1;  nrinsgp1 = 0;  pdefn = FALSE; }

      cfactor1 =  1000;  comppc =    10;  dedmode =   4;  dedsiz1  = 1000;
      lahead   =     0;  mendel = FALSE;  pdsiz1  = 256;  rfactor1 =    0;
      rfill    = FALSE;  pcomp  = FALSE;

      continue;
      }

    /* Set fill-factor for preferred defns. If 0, Level 1 will set ffactor to 
    a `sensible' default (eg, 5(ncol+2)/4). */
    if (al2_match("f[factor]") || al2_match("fi[ll factor]"))
      {
      al2_readia();  al2_endcmd();

      if ( (intcnt > 0 && intarr[0] < 0) || intcnt > 1 )
                             { al2_continue("bad parameter"); }
      else if (intcnt == 0)  { printf("fill factor = %"PS"\n", ffactor1); }
      else                   { ffactor1 = intarr[0]; }

      continue;
      }

    /* subgroup generator list ... */
    if (al2_match("gen[erators]") || al2_match("subgroup gen[erators]"))
      {
      if (ndgen < 1)  { al2_continue("there are no generators as yet"); }

      skipnl = TRUE;  al2_skipws();  p = al2_rdwl();  al2_endcmd();

      if (genlst != NULL)  { al1_emptywl(genlst);  free(genlst); }
      genlst = p;
      nsgpg  = p->len;

      okcont  = okredo   = FALSE;
      tabinfo = tabindex = FALSE;

      continue;
      }

    /* grp gen's, a letter list or a number ... */
    if (al2_match("gr[oup generators]"))
      {
      if (isdigit(currip) || currip == '+' || currip == '-')
        {
        i = al2_readint();  al2_endcmd();
        if (i < 1)  { al2_continue("bad parameter"); }
        ndgen  = i;
        galpha = FALSE;
        li = TRUE;
        }
      else if (isalpha(currip))
        {
        i = al2_readgen();  al2_endcmd();
        ndgen  = i;
        galpha = TRUE;
        for (j = 1; j <= ndgen; j++)  { algen[j] = currname[j]; }
        algen[ndgen+1] = '\0';
        for (j = 1; j <= 26; j++)     { genal[j] = 0; }
        for (j = 1; j <= ndgen; j++)  { genal[algen[j]-'a'+1] = j; }
        li = TRUE;
        }
      else
        {
        al2_endcmd();

        printf("group generators = ");
        if      (ndgen < 1)  { printf("none\n"); }
        else if (galpha)     { printf("%s\n", &algen[1]); }
        else                 { printf("1..%"PS"\n", ndgen); }

        li = FALSE;
        }

      /* If grp gens changed, the relator & grp generator lists are invalid, 
      there is no CT info, and only starting a new enum'n is ok (maybe). */

      if (li)
        {
        okstart = (costable != NULL);
        okcont  = okredo   = FALSE;
        tabinfo = tabindex = FALSE;

        if (rellst != NULL)  { al1_emptywl(rellst);  free(rellst); }
        rellst = NULL;  ndrel = 0;

        if (genlst != NULL)  { al1_emptywl(genlst);  free(genlst); }
        genlst = NULL;  nsgpg = 0;
        }

      continue;
      }

    /* relator list ... */
    if (al2_match("group relators") || al2_match("rel[ators]"))
      {
      if (ndgen < 1)  { al2_continue("there are no generators as yet"); }

      skipnl = TRUE;  al2_skipws();  p = al2_rdrl();  al2_endcmd();

      if (rellst != NULL)  { al1_emptywl(rellst);  free(rellst); }
      rellst = p;
      ndrel  = p->len;

      okcont  = okredo   = FALSE;
      tabinfo = tabindex = FALSE;

      continue;
      }

    /* mixed Ct/Rt mode, with all the (appropriate) trimmings ... */
    if (al2_match("hard"))
      {
      al2_endcmd();

      cfactor1 =  1000;  comppc =  10;  dedmode  =     4;  dedsiz1  = 1000;
      ffactor1 =     0;  lahead =   0;  mendel   = FALSE;  nrinsgp1 =   -1;
      pdefn    =  TRUE;  pdsiz1 = 256;  rfactor1 =     1;  rfill    = TRUE;
      pcomp    = FALSE;

      continue;
      }

    /* splurge all the cmds ... */
    if (al2_match("h[elp]"))
      { al2_endcmd();  al2_help();  continue; }

    /* Rt style defns, with compaction/lookahead/fill ... */
    if (al2_match("hlt"))
      {
      al2_endcmd();

      cfactor1 =     0;  comppc =  10;  dedmode  =     0;  dedsiz1  = 1000;
      ffactor1 =     1;  lahead =   1;  mendel   = FALSE;  nrinsgp1 =    0;
      pdefn    = FALSE;  pdsiz1 = 256;  rfactor1 =  1000;  rfill    = TRUE;
      pcomp    = FALSE;

      continue;
      }

    /* lookahead mode ... */
    if (al2_match("look[ahead]"))
      {
      al2_readia();  al2_endcmd();

      if ( (intcnt > 0 && (intarr[0] < 0 || intarr[0] > 4)) || intcnt > 1 )
                             { al2_continue("bad parameter"); }
      else if (intcnt == 0)  { printf("lookahead = %"PS"\n", lahead); }
      else                   { lahead = intarr[0]; }

      continue;
      }

    /* limit times round enumerator's main loop ... */
    if (al2_match("loop[ limit]"))
      {
      al2_readia();  al2_endcmd();

      if ( (intcnt > 0 && intarr[0] < 0) || intcnt > 1 )
                             { al2_continue("bad parameter"); }
      else if (intcnt == 0)  { printf("loop limit = %"PS"\n", llimit); }
      else                   { llimit = intarr[0]; }

      continue;
      }

    /* limit number of cosets allowed ... */
    if (al2_match("max[ cosets]"))
      {
      al2_readia();  al2_endcmd();

      if ( (intcnt > 0 && (intarr[0] < 0 || intarr[0] == 1)) || intcnt > 1 )
                             { al2_continue("bad parameter"); }
      else if (intcnt == 0)  { printf("max cosets = %"PC"\n", maxrow1); }
      else                   { maxrow1 = intarr[0]; }

      continue;
      }

    /* setup progress messages ... */
    if (al2_match("mess[ages]") || al2_match("mon[itor]"))
      {
      al2_readia();  al2_endcmd();

      if (intcnt > 1)  { al2_continue("too many parameters"); }
      else if (intcnt == 0)
        {
        if (msgctrl)  { printf("messages = %"PS"\n", msgincr); }
        else          { printf("messages = off\n"); }   
        }
      else if (intarr[0] == 0)  { msgctrl = FALSE;  msgincr = 0; }
      else if (intarr[0] > 0)   { msgctrl = TRUE;   msgincr = intarr[0]; }
      else    { al2_continue("parameter must be non-negative"); }

      continue;
      }

    /* setup Mendelsohn flag ... */
    if (al2_match("mend[elsohn]")) 
      { 
      al2_readia();  al2_endcmd();

      if ( (intcnt > 0 && (intarr[0] < 0 || intarr[0] > 1)) || intcnt > 1 )
        { al2_continue("bad parameter"); }
      else if (intcnt == 0)
        { printf("mendelsohn = %s\n", mendel ? "true" : "false"); }
      else
        { mendel = (intarr[0] == 1); }

      continue;
      }

    /* what modes are allowable ... */
    if (al2_match("mo[de]"))
      {
      al2_endcmd();

      if (okstart)  { printf("start = yes,"); }
      else          { printf("start = no,"); }
      if (okcont)   { printf(" continue = yes,"); }
      else          { printf(" continue = no,"); }
      if (okredo)   { printf(" redo = yes\n"); }
      else          { printf(" redo = no\n"); }

      continue;
      }

    /* which grp gen (dont) normalise which subgrp gen ... */
    if (al2_match("nc") || al2_match("normal[ closure]"))
      {
      al2_readia();  al2_endcmd();

      if ( (intcnt > 0 && (intarr[0] < 0 || intarr[0] > 1)) || intcnt > 1 )
                      { al2_continue("bad parameter"); }
      if (!tabinfo)   { al2_continue("there is no table information"); }
      if (nsgpg < 1)  { al2_continue("there are no subgrp generators"); }

      begintime = al0_clock();  li = al0_compact();  endtime = al0_clock();
      if (li)  { printf("CO"); }
      else     { printf("co"); }
      printf(": a=%"PC" r=%"PC" h=%"PC" n=%"PC"; c=+%4.2f\n", 
                   nalive, knr, knh, nextdf, al0_diff(begintime,endtime));

      if (intcnt == 0)  { al2_normcl(FALSE); }
      else              { al2_normcl(intarr[0] == 1); }

      continue;
      }

    /* which rel's to scan at coset #1 ... */
    if (al2_match("no[ relators in subgroup]"))
      {
      al2_readia();  al2_endcmd();

      if ( (intcnt > 0 && intarr[0] < -1) || intcnt > 1 )
                            { al2_continue("bad parameter"); }
      else if (intcnt == 0) { printf("no rels in subgr = %"PS"\n", nrinsgp1); }
      else                  { nrinsgp1 = intarr[0]; }

      continue;
      }

    /* find coset(s) of requested order ... */
    if (al2_match("oo") || al2_match("order[ option]"))
      {
      al2_readia();  al2_endcmd();

      if (intcnt != 1)  { al2_continue("missing / too many arguments"); }
      if (!tabinfo)     { al2_continue("no information in table"); }

      al2_oo(intarr[0]);

      continue;
      }

    /* dump build info ... */
    if (al2_match("opt[ions]"))
      { al2_endcmd();  al2_opt();  continue; }

    /* coinc path compression ... */
    if (al2_match("path[ compression]")) 
      { 
      al2_readia();  al2_endcmd();

      if ( (intcnt > 0 && (intarr[0] < 0 || intarr[0] > 1)) || intcnt > 1 )
        { al2_continue("bad parameter"); }
      else if (intcnt == 0)
        { printf("path compression = %s\n", pcomp ? "on" : "off"); }
      else
        { pcomp = (intarr[0] == 1); }

      continue;
      }

    /* preferred defn on/off ... */
    if (al2_match("pd[efinitions]"))
      {
      al2_readia();  al2_endcmd();

      if ( (intcnt > 0 && (intarr[0] < 0 || intarr[0] > 1)) || intcnt > 1 )
                     { al2_continue("bad parameter"); }
      else if (intcnt == 0)  
                     { printf("pref defn = %s\n", pdefn ? "true" : "false"); }
      else           { pdefn = (intarr[0] == 1); }

      continue;
      }

    /* pref defn queue size (power of 2, 0 = default) ... */
    if (al2_match("pd si[ze]") || al2_match("psiz[e]"))
      {
      al2_readia();  al2_endcmd();

      if ( (intcnt > 0 && intarr[0] < 0) || intcnt > 1 )
                                 { al2_continue("bad parameter"); }
      else if (intcnt == 0)      { printf("pref defn list = %"PS"\n",pdsiz1); }
      else if (intarr[0] == 0)   { pdsiz1 = intarr[0]; }
      else if (intarr[0]%2 == 1) { al2_continue("bad parameter"); }
      else				
        {					/* even & >= 2 */
        i = intarr[0];
        while (i%2 == 0)  { i /= 2; }
        if (i == 1)  { pdsiz1 = intarr[0]; }
        else         { al2_continue("bad parameter"); }
        }

      continue;
      }

    /* dump enum'n & parameters ... */
    if (al2_match("print det[ails]") || al2_match("sr"))
      {
      al2_readia();  al2_endcmd();

      if ( (intcnt > 0 && (intarr[0] < 0 || intarr[0] > 5)) || intcnt > 1 )
                             { al2_continue("bad parameter"); }
      else if (intcnt == 0)  { al1_prtdetails(0); }
      else                   { al1_prtdetails(intarr[0]); }

      continue;
      }

    /* No parameters means all the table, one parameter "x" means (1,x), two 
    params "x,y" means (x,y).  Negative 1st param means include order/rep, 
    else don't.  Negative 2nd param means include coinc, else don't.  Note 
    that CT with coincs may be confusing.  Begin & end values are silently
    adjusted to fit the CT size, so some sloppyness is allowed. */

    if (al2_match("pr[int table]"))
      {
      al2_readia();  al2_endcmd();

      if (!tabinfo)  { al2_continue("no information in table"); }

      li = lj = FALSE;
      if (intcnt > 0 && intarr[0] < 0)  
        { intarr[0] = -intarr[0];  lj = TRUE; }
      if (intcnt > 1 && intarr[1] < 0)  
        { intarr[1] = -intarr[1];  li = TRUE; }

      switch (intcnt)
        {
        case 0:  intarr[0] = 1;  intarr[1] = nextdf-1;   break;
        case 1:  intarr[1] = intarr[0];  intarr[0] = 1;  break;
        case 2:                                          break;
        default:  al2_continue("too many parameters");   break;
        }

      if (intarr[0] < 1)  { intarr[0] = 1; }
      if (intarr[1] < 1)  { intarr[1] = 1; }

      if (intarr[0] >= nextdf)  { intarr[0] = nextdf-1; }
      if (intarr[1] >= nextdf)  { intarr[1] = nextdf-1; }

      if (intarr[0] > intarr[1])  { al2_continue("bad range"); }

      al1_prtct(intarr[0], intarr[1], li, lj);

      continue;
      }

    /* basic Felsch; Ct defns & full dedn processing ... */
    if (al2_match("pure c[t]"))
      {
      al2_endcmd();

      cfactor1 =  1000;  comppc = 100;  dedmode  =     4;  dedsiz1  =  1000;
      ffactor1 =     1;  lahead =   0;  mendel   = FALSE;  nrinsgp1 =     0;
      pdefn    = FALSE;  pdsiz1 = 256;  rfactor1 =     0;  rfill    = FALSE;
      pcomp    = FALSE;

      continue;
      }

    /* basic HLT; Rt defns & no lookahead/fill ... */
    if (al2_match("pure r[t]"))
      {
      al2_endcmd();

      cfactor1 =     0;  comppc = 100;  dedmode  =     0;  dedsiz1  =  1000;
      ffactor1 =     1;  lahead =   0;  mendel   = FALSE;  nrinsgp1 =     0;
      pdefn    = FALSE;  pdsiz1 = 256;  rfactor1 =  1000;  rfill    = FALSE;
      pcomp    = FALSE;

      continue;
      }

    /* Try to find nontrivial subgroup with nominated index by augmenting the
    subgroup (gen'rs) with randomly chosen coset (rep'ves).  This is a 
    `dangerous' operation, since it can go wrong, or `corrupt' the status, in 
    so many ways.  We try to minimise problems by being very strict as to when
    we allow it to be called; how much of this is necessary is moot. */

    if (al2_match("rc") || al2_match("random coinc[idences]"))
      {
      al2_readia();  al2_endcmd();

      if (intcnt < 1 || intcnt > 2)
        { al2_continue("bad number of parameters"); }
      if (intarr[0] < 0)
        { al2_continue("invalid first argument"); }
      if (intcnt == 2 && intarr[1] < 1)
        { al2_continue("invalid second argument"); }

      if (!tabinfo)  { al2_continue("there is no table information"); }
      if (!okredo)   { al2_continue("can't redo (different presentation?)"); }

      if (lresult == 1)
        { al2_continue("trivial finite index already exists"); }

      if (intarr[0] == 0)
        {
        if (lresult > 0)
          { al2_continue("non-trivial finite index already present"); }
        }
      else
        {
        if (lresult > 0 && lresult < intarr[0])
          { al2_continue("finite index already < argument"); }
        if (lresult > 0 && lresult%intarr[0] == 0)
          { al2_continue("finite index already multiple of argument"); }
        }

      if (intarr[0] >= nalive)
        { al2_continue("not enough active cosets available"); }

      if (intcnt == 1)  { al2_rc(intarr[0],8); }
      else              { al2_rc(intarr[0],intarr[1]); }

      continue;
      }

    /* NB: compacting the table can cause disded to become true. */

    if (al2_match("rec[over]") || al2_match("contig[uous]"))
      {
      al2_endcmd();
      if (!tabinfo)  { al2_continue("there is no table information"); }

      begintime = al0_clock();  li = al0_compact();  endtime = al0_clock();
      if (li)  { printf("CO"); }
      else     { printf("co"); }
      printf(": a=%"PC" r=%"PC" h=%"PC" n=%"PC"; c=+%4.2f\n", 
                        nalive, knr, knh, nextdf, al0_diff(begintime,endtime));

      continue;
      }

    /* random equivalent presentations ... */
    if (al2_match("rep"))
      {
      al2_readia();  al2_endcmd();

      if (intcnt < 1 || intcnt > 2)
        { al2_continue("bad number of parameters"); }
      if (intarr[0] < 1 || intarr[0] > 7)
        { al2_continue("invalid first argument"); }
      if (intcnt == 2 && intarr[1] < 1)
        { al2_continue("invalid second argument"); }

      if (!okstart)
        { al2_continue("can't start (no generators/workspace)"); }
      if (rellst == NULL || rellst->len == 0)
        { al2_continue("can't start (no relators)"); }

      if (intcnt == 1)  { al2_rep(intarr[0], 8); }
      else              { al2_rep(intarr[0], intarr[1]); }

      continue;
      }

    /* RT (ie, HLT) coset defn factor ... */
    if (al2_match("r[factor]") || al2_match("rt[ factor]"))
      {
      al2_readia();  al2_endcmd();

      if (intcnt > 1)        { al2_continue("bad parameter"); }
      else if (intcnt == 0)  { printf("rt factor = %"PS"\n", rfactor1); }
      else                   { rfactor1 = intarr[0]; }

      continue;
      }

    /* Rt defn row filling ... */
    if (al2_match("row[ filling]")) 
      { 
      al2_readia();  al2_endcmd();

      if ( (intcnt > 0 && (intarr[0] < 0 || intarr[0] > 1)) || intcnt > 1 )
        { al2_continue("bad parameter"); }
      else if (intcnt == 0)
        { printf("row fill = %s\n", rfill ? "on" : "off"); }
      else
        { rfill = (intarr[0] == 1); }

      continue;
      }

    /* find cosets which stabilise subgroup ... */
    if (al2_match("sc") || al2_match("stabil[ising cosets]"))
      {
      al2_readia();  al2_endcmd();

      if (intcnt != 1)  { al2_continue("missing / too many arguments"); }
      if (!tabinfo)     { al2_continue("no information in table"); }

      al2_sc(intarr[0]);

      continue;
      }

    /* We emulate, as best we can, the odd-numbered enumeration strategies
    given in Table 5.5.1 (p. 245) of C.C. Sims' book.  The even-numbered ones 
    involve `standardise-as-you-go', which we don't do; however we can
    standardise the table once we're done, or we can pause an enumeration at 
    any time, standardise, and then continue.  (This last is not as daft as it
    seems and does, in fact, sometimes prove beneficial.)  The strategies are:
    1) HLT, no save; 3) HLT, save; 5) CHLT, no save; 7) CHLT, save; 9) Felsch 
    (save).  We already have 1/5/9, but 3/7 are new.  #3 is handy. */

    if (al2_match("sims"))
      {
      al2_readia();  al2_endcmd();

      if ( intcnt != 1 || intarr[0] < 1 || intarr[0] > 9 || intarr[0]%2 == 0 )
        { al2_continue("bad parameter"); }

      switch(intarr[0])
        {
        case 1:					/* cf. "pure r" + row-fill */
          cfactor1 =    0;  dedmode =    0;  mendel = FALSE;
          rfactor1 = 1000;  rfill   = TRUE;  break;
        case 3:
          cfactor1 =     0;  dedmode =    4;  mendel = FALSE;
          rfactor1 = -1000;  rfill   = TRUE;  break;
        case 5:					/* #1 + Mendelsohn */
          cfactor1 =    0;  dedmode =    0;  mendel = TRUE;  
          rfactor1 = 1000;  rfill   = TRUE;  break;
        case 7:
          cfactor1 =     0;  dedmode =    4;  mendel = TRUE;
          rfactor1 = -1000;  rfill   = TRUE;  break;
        case 9:					/* cf. "pure c" / "Felsch" */
          cfactor1 = 1000;  dedmode =     4;  mendel = FALSE;
          rfactor1 =    0;  rfill   = FALSE;  break;
        }

      /* parameters common to all modes ... */

      comppc   = 10;		/* compaction always allowed */
      dedsiz1  = 1000;		/* default (starting) size */
      ffactor1 = 1;		/* fill-factor not active */
      lahead   = 0;		/* never lookahead */
      nrinsgp1 = 0;		/* no (active) RS phase */
      pdefn    = FALSE;		/* no preferred defns ... */
      pdsiz1   = 256;
      pcomp    = FALSE;		/* dont compress coinc paths */

      continue;
      }

    /* standardise (& compact) the CT ... */
    if (al2_match("st[andard table]"))
      {
      al2_endcmd();

      if (!tabinfo)  { al2_continue("no information in table"); }

      begintime = al0_clock();
      li = al0_compact();  lj = al0_stdct();
      endtime = al0_clock();

      if (li)  { printf("CO"); }
      else     { printf("co"); }
      if (lj)  { printf("/ST"); }
      else     { printf("/st"); }
      printf(": a=%"PC" r=%"PC" h=%"PC" n=%"PC"; c=+%4.2f\n", 
                        nalive, knr, knh, nextdf, al0_diff(begintime,endtime));

      continue;
      }

    /* what style implied by Ct/Rt params ... */
    if (al2_match("style"))
      {
      al2_endcmd();

      if (rfactor1 < 0)
        {
        if      (cfactor1 < 0)   { printf("style = R/C\n"); }
        else if (cfactor1 == 0)  { printf("style = R*\n"); }
        else                     { printf("style = Cr\n"); }
        }
      else if (rfactor1 == 0)
        {
        if      (cfactor1 < 0)   { printf("style = C* (aka C-style)\n"); }
        else if (cfactor1 == 0)  { printf("style = R/C (default R & C)\n"); }
        else                     { printf("style = C\n"); }
        }
      else
        {
        if      (cfactor1 < 0)   { printf("style = Rc\n"); }
        else if (cfactor1 == 0)  { printf("style = R\n"); }
        else                     { printf("style = CR\n"); }
        }

      continue;
      }

    /* give subgrp non-default (ie, "H") name; "" is ok ... */
    if (al2_match("subg[roup name]"))
      {
      al2_readname();  al2_endcmd();

      if (subgrpname != NULL)  { free(subgrpname); }
      if (( subgrpname = (char*)malloc(strlen(currname)+1) ) == NULL)
        { al2_continue("out of memory in subgrpname copy"); }
      strcpy(subgrpname, currname);

      continue;
      }

    /* Allows access to the system; ie, fires up a shell & passes it the (non-
    empty) argument.  Use with caution, of course.  The argument must consist 
    of one line of printable characters (plus '\t'), excluding ';'.  Trailing 
    WS is removed.  We do *no* error checking on the call, or its result. */
 
    if (al2_match("sys[tem]"))
      {
      al2_readname();  al2_endcmd();

      if (strlen(currname) == 0)  { al2_continue("empty argument"); }
      else                        { system(currname); }

      continue;
      }

    /* print out some text ... */
    if (al2_match("text"))
      {
      al2_readname();  al2_endcmd();  printf("%s\n", currname);
      continue;
      }

    /* The trace word command takes as arguments a coset number & a word.
    Unlike ACE2, we do not allow a multi-line word. */

    if (al2_match("tw") || al2_match("trace[ word]"))
      {
      i = al2_readint();
      if (currip != ',')  { al2_continue("missing argument"); }
      al2_nextnw();
      if (( j = al2_pwrd(0) ) == 0)  { al2_continue("empty argument"); }
      al2_endcmd();

      if (!tabinfo)  { al2_continue("table dne or has no information"); }
      if (i < 1 || i >= nextdf || COL1(i) < 0)
                     { al2_continue("invalid/redundant coset number"); }

      /* copy currword (gen'r nos) to currrep (col nos) ... */
      repsiz = 0;
      for (k = 0; k < j; k++)
        { 
        if ( !al1_addrep( gencol[ndgen+currword[k]] ) )
          { al2_continue("unable to build coset rep've"); }
        }

      if ((k = al1_trrep(i)) == 0)  { printf("* Trace doesn't complete\n"); }
      else                          { printf("%"PB" * word = %"PB"\n", i, k); }

      continue;
      }

    /* Negative workspace sizes are errors, zero size selects DEFWORK, and
    values <1K (ie, KILO) are rounded up to 1K. */

    if (al2_match("wo[rkspace]"))
      {
      if ( !(isdigit(currip) || currip == '+' || currip == '-') )
        {
        al2_endcmd();		/* Raises error if currip not ';' or '\n'. */ 
        printf("workspace = %"PS" x %"PS"\n", workspace, workmult);
        }
      else
        {
        i = al2_readint();  j = al2_readmult();  al2_endcmd();

        if      (i < 0)  { al2_continue("argument must be non-negative"); }
        else if (i == 0)              { i = DEFWORK;  j = 1; }
        else if (j == 1 && i < KILO)  { i = KILO; }

        workspace = i;  workmult = j;

        /* The casts to size_t are to allow big allocations (hopefully). */

        if (costable != NULL)  { free(costable); }
        costable = (Entry*)malloc( (size_t)workspace * (size_t)workmult 
                                                    * (size_t)sizeof(Entry) );
        if (costable == NULL) 
          {
          okstart = okcont   = okredo = FALSE;
          tabinfo = tabindex = FALSE;
          al2_restart("unable to resize workspace (will try default)");
          }

        okstart = (ndgen > 0);			/* new table ... */
        okcont  = okredo   = FALSE;
        tabinfo = tabindex = FALSE;
        }

      continue;
      }

    /* A 'hidden' command to dump some of the Level 0/1/2 variables.  Modify
    this as required if you want other information. */

    if (al2_match("deb[ug]"))
      {
      printf("  -- Debug -------------------------------------------\n");
      printf("Lev0: ncol=%"PS" ndrel=%"PS" nsgpg=%"PS" disded=%d sgdone=%d\n", 
                                            ncol, ndrel, nsgpg, disded, sgdone);
      printf("Lev1: trellen=%"PS" tgenlen=%"PS"\n",  trellen, tgenlen);
      printf("Lev2: okstart=%d okcont=%d okredo=%d tabinfo=%d tabindex=%d "
        "lresult=%"PC"\n", okstart, okcont, okredo, tabinfo, tabindex, lresult);
      printf("  ----------------------------------------------------\n");
      continue;
      }

    /* ... no match; signal an error */

    al2_continue("there is no such keyword");
    }
  }

