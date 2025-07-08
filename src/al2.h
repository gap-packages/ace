
/*******************************************************************************

  al2.h
  1 Nov 03, 21 Apr 14
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


This is the header file for Level 2 of ACE; ie, a demonstration application in
the form of a stand-alone, fully-featured coset enumerator, with an interactive 
interface (although it's more usually driven in batch mode, by an input script).

*******************************************************************************/

#include "al1.h"

/*******************************************************************************
To ensure that any index reported by the enumerator is correct, we must take 
care that we do not call al0_enum() in an invalid mode.  If the okstart 
(resp. okcont, okredo) flag is set, then it is permissible to call Level 1/0 in
start (resp. continue, redo) mode; although other things may have to be checked 
as well.  The actions of the enumerator are monitored (mainly in cmdloop.c and
postproc.c), and will set/clear the appropriate flags.  All three flags are 
initialised FALSE (remember, paranoia prevents problems).
*******************************************************************************/

extern Logic okstart, okcont, okredo;

/*****************************************************************************
In order that we do not do anything `silly' during postprocessing, we maintain
various status regarding the current state of the table.  lresult is the 
result of the last call to al1_start().  If tabindex is T, then we have a 
(valid) index.  If tabinfo is T, then the table contains valid information; in
particular, the SG phase has been successfully completed.
*****************************************************************************/

extern Logic tabinfo, tabindex;
extern Coset lresult;

/*****************************************************************************
echo defaults to FALSE, and should be left that way for interactive use.  If 
output is redirected to a file (because, say, the i/p is coming from a 
script), we might want to set this so that the commands are also logged
(although it can make a bit of a mess of the o/p).  If skipnl is set, then 
'\n' is treated as whitespace (eg, as part of a multiline relator list).  
currip is the current input character, currkey is the current command (ie, 
keyword), and currname is the current name (ie, string argument).  currword is
the word (group relator/subgroup generator) currently being processed, and 
currsiz is the size of the array allocated to currword (*not* the size of the 
stored word).  currexp is the (most recent) exponent explicitly entered for 
currword (for tracking involutions).

Note that currip is set by getchar(), which (in common with most `character'
handling under C/unix) uses type int.  So currip is left as type int, as are
some other variables/functions of this ilk.  currkey & currname can be type
char, since currip is checked before storing in these.

TBA ... note the magic numbers 64 & 128 here, they should be #define's
*****************************************************************************/

extern Logic echo, skipnl;
extern int   currip;
extern char  currkey[64], currname[128];
extern SInt *currword, currsiz, currexp;

/*****************************************************************************
Various parameters to Level 2 are lists of integers.  We store them & their 
number in these.  The numbers can be cosets, but can also be integers for 
other purposes, so we use the BInt type.
TBA ... note the magic number 32 here, it should be a #define
*****************************************************************************/

extern SInt intcnt;
extern BInt intarr[32];

/*****************************************************************************
External functions defined in util2.c
*****************************************************************************/

void  al2_init(void);
void  al2_opt(void);
void  al2_help(void);
void  al2_nextip(void);
void  al2_skipws(void);
void  al2_nextnw(void);

/*****************************************************************************
External functions defined in parser.c
*****************************************************************************/

void   al2_readkey(void);
void   al2_readname(void);
SInt   al2_readmult(void);
SInt   al2_readgen(void);
Logic  al2_match(char*);
void   al2_endcmd(void);
BInt   al2_readint(void);
void   al2_readia(void);
SInt   al2_pwrd(SInt);
Wlist *al2_rdwl(void);
Wlist *al2_rdrl(void);

/*****************************************************************************
External functions defined in cmdloop.c
*****************************************************************************/

void al2_cmdloop(void);

/*****************************************************************************
External functions defined in postproc.c
*****************************************************************************/

void al2_oo(Coset);
void al2_sc(Coset);
void al2_cycles(void);
void al2_normcl(Logic);
void al2_cc(Coset);
void al2_rc(Coset,SInt);
void al2_dw(Wlist*);
void al2_rep(SInt, SInt);
void al2_aep(SInt);

/*****************************************************************************
External functions defined in ace.c
*****************************************************************************/

void  al2_continue(char*);
void  al2_restart(char*);
void  al2_abort(char*);

