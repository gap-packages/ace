
/*****************************************************************************

  al1.h
  1 Nov 03, 18 Apr 14
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


This is the header file for Level 1 of ACE; that is, a set of basic wrapper 
routines round the core enumerator.  This can also be thought of as a simple 
interface between application programmes (eg, ACE Level 2 or whatever) and the
clever bits.  It is not necessary to use this to drive Level 0, but it makes
things a good deal easier.

*****************************************************************************/

#include "al0.h"

/*******************************************************************************
This is an approximate limit on the length of output lines.  It's used by
al1_prtwl() and al2_cycles() (which is, of course, a level 2 routine) to start a
newline if the current one is already 'too long'.  The limit is 'soft', in that 
we throw a new line if we can, but don't, for example, split individual relators
across lines.  The LLL limit is not checked in other places, and wider lines are
quite common.  Very wide lines are possible if, for example, tables with coset 
representatives are printed for high-numbered cosets.

TBA ... LLL should really be increased (given the wide screens now available),
and it should be more widely 'enforced'.
*******************************************************************************/

#define LLL 75

/*******************************************************************************
The memory for the coset table is currently allocated as one contiguous block.
This is done by the user; Level 1 expects to be `handed' the workspace for the
table, pointed to by costable.  DEFWORK should be used as the default number 
of words in toto (ie, number of Entry's in costable[]).  The workspace size is 
indicated via a size/multiplier combination (workspace/workmult, note that these
are SInt's).  The usual K/M/G multipliers are used (as powers of 10, *not* 
binary).  tabsiz indicates the maximum number of rows which can (safely) be 
fitted into the allocated space; it depends on ncol.  If tabsiz works out to 
less than 2, the al1_start() function will complain.  Note that tabsiz is a 
BInt, so could be larger than a Coset; however, the number of actual rows which 
can be used is limited by the size of Coset (& the maxrow value).
*******************************************************************************/

#define DEFWORK  1000000

#define KILO  1000
#define MEGA  1000000
#define GIGA  1000000000

extern SInt   workspace, workmult;
extern Entry *costable;
extern BInt   tabsiz;

/*****************************************************************************
The group relations and subgroup generators are stored as linked lists.  Each 
item on the list consists of an array of generators (ie, the word), along with
its total length, its exponent & whether or not it was enter as x^2 (ie, as an
involn).  In the words, -ve numbers represent inverses.  The word starts at 
word[1].  Each list has a header containing the list's length and head/tail 
pointers.
*****************************************************************************/

typedef struct Wlelt
  {
  SInt *word;			/* array of generators */
  SInt len, exp;		/* total length, and exponent */
  Logic invol;			/* ?entered as an involution */
  struct Wlelt *next;		/* next in list */
  }
Wlelt;				/* word list element */

typedef struct
  {
  SInt len;			/* list length */
  Wlelt *first, *last;		/* head & tail of list */
  }
Wlist;				/* word list */

/*****************************************************************************
We find coset rep's by backtracing the table.  currrep is the currently active
rep've, repsiz is its size & repsp is the space allocated to currrep.  Note 
that the representative is in terms of column numbers.
*****************************************************************************/

extern SInt *currrep, repsiz, repsp;

/*****************************************************************************
Logic control variable for current enumeration.  TRUE, use presentation as 
given.  FALSE, reduce (freely/cyclically relators & freely generators) and
reorder relations/generators.
*****************************************************************************/

extern Logic asis;

/*****************************************************************************
  Group stuff ...
grpname  enumeration (ie, group) name
rellst   the group's relator list
trellen  total relator list length
ndgen    number of group generators
geninv   are generators involutions?
gencol   translates +/- gen'r nos to columns
colgen   translates col nos to +/- gen'r nos
galpha   T if the generators are letters
algen    translate generator number (1..ndgen, in its order of entry) to its 
         letter (ie, 'a'...'z').  A printable string, hence 1+26+1=28 posns.
genal    translate generator letter (where a=1, etc) to its order of entry 
         (ie, number)
*****************************************************************************/

extern char  *grpname;
extern Wlist *rellst;
extern SInt   trellen;
extern SInt   ndgen;
extern Logic *geninv;
extern SInt  *gencol, *colgen;
extern Logic  galpha;
extern char   algen[28];
extern SInt   genal[27];

/*****************************************************************************
  Subgroup stuff ...
subgrpname  subgroup name
genlst      the subgroup's renerator list
tgenlen     total list length
*****************************************************************************/

extern char  *subgrpname;
extern Wlist *genlst;
extern SInt   tgenlen;

/*******************************************************************************
Many of the Level 0 parameters can be set directly.  However, some of them have 
slightly different meanings at Level 1 (eg, a special value can be used to 
indicate a `default'), or can affect a continuing enumeration.  All of the 
following variables are `aliases' for Level 0 parameters, and it is up to the 
al1_start() function to decide when & how they should be transferred to their 
Level 0 namesakes.
*******************************************************************************/

extern SInt  rfactor1, cfactor1;
extern SInt  pdsiz1, dedsiz1;
extern Coset maxrow1;
extern SInt  ffactor1, nrinsgp1;

/*****************************************************************************
Externally visible functions defined in util1.c
*****************************************************************************/

void   al1_init(void);
void   al1_prtdetails(SInt);
void   al1_rslt(Coset);
Wlist *al1_newwl(void);
Wlelt *al1_newelt(void);
void   al1_addwl(Wlist*, Wlelt*);
void   al1_concatwl(Wlist*, Wlist*);
void   al1_emptywl(Wlist*);
void   al1_prtwl(Wlist*, SInt);
Logic  al1_addrep(SInt);
Logic  al1_bldrep(Coset);
Coset  al1_trrep(Coset);
Coset  al1_ordrep(void);
void   al1_prtct(Coset, Coset, Logic, Logic);

/*****************************************************************************
Externally visible functions defined in control.c
*****************************************************************************/

Coset al1_start(SInt);

