
/*****************************************************************************

  al0.h
  1 Nov 03, 30 Apr 14
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


This is the header file for Level 0 of ACE; that is, the core enumerator
routines.  This file (and maybe util0.c) is all that should need changed to
move to a different `platform' (joke).

*****************************************************************************/

#define ACEVER  "ACE 4.100"

/*******************************************************************************
Stdio.h, stdlib.h & inttypes.h will be included in all the source files, since 
all of them ultimately include this file.
*******************************************************************************/

#include <stdio.h>
#include <stdlib.h>

#include <inttypes.h>

/*******************************************************************************
We try, as far as possible, to stick with generic, standard C99.  We also assume
that we are in the C locale, and that we're using the ASCII character set.  The 
usual target is a Unix box of some sort, but that's not hard-wired in any way.  
The demo stand-alone applic'n (ACE level 2) has a text-based I/O; if you want a 
GUI interface, write it yourself.  The Logic type is defined for clarity, to 
help make the code clearer.
*******************************************************************************/

typedef int    Logic;
#define TRUE   1
#define FALSE  0

/*******************************************************************************
The coset table (CT) is treated as a 2D array, and its entries are signed 
integers representing coset numbers (signing allows coinc cosets to be handled
within the CT array).  This is just the way things are; alternatives exist, but 
I'm not going to describe them here. 

On machines with enough memory we can build CTs with more than 2G (ie, 2^31) 
rows, so 32 bit integers are not enough.  Modern machines & compilers support
64 bit integers.  This is fine, but is very wasteful of space in the CT.  So we 
pack the CT data as a bitfield (in a struct) in a specified number of bytes, and
accept whatever speed penalty this gives.  Note that CT memory = cosets * 
columns * bytes/coset, and that we can exceed the 32 bit addressing limit of 4G
(ie, 2^32) bytes with less than 2G cosets.

We define a `standard integer' type for a coset (type Coset) and a type for a 
`CT entry' (type Entry, a Coset or a packed bitfield which extends to a Coset), 
along with some macros to manipulate the CT and its entries.  We use the
features of inttypes.h (which is part of the C99 standard) to ensure that the 
code is as robust & generic as possible.  The intended compiler is the GNU gcc 
compiler, which is widely available, is C99 compliant, and supports the packed 
keyword.  

We also define types for (almost) all of the integers used throughout the code;
a 'standard integer' SInt and a 'big integer' BInt.  SInt is used for all 
general code, and should be at least 32 bits.  BInt is intended for pointer and
memory size related code, and must be at least as big as both SInt & Coset.  It
will typically be 64 bits on modern machines and 32 bits on older machines or
ones with less than 4G bytes of memory.

The Coset type must be at least 16 bits, since it has to contain the result of a
call to the level 0 enumerator and its level 1 wrapper, which can return errors 
in the ~-8k range.  It's probably a good idea to ensure that Entry's size is a
multiple of 8 bits (ie, it's byte aligned).  Obviously, its size can be at most
Coset's size, but it can, in principle, be as small as you wish.

In the macros for accessing to coset table, i is coset, j is generator (as 
column number).  colptr[j] stores the start address of the block of memory for 
column j.  CT(i,j), with j = 1...ncol, indicates the action of the associated 
generator or inverse generator of column j on coset i.  It contains the coset 
number if known, otherwise 0 (in column 1, -ve numbers indicate coincidences).  
Coset #1 is the subgroup/identity.  Note the special macros for cols 1 & 2 to 
maximise speed (and improve clarity), since these columns handle coincs & are 
heavily used.  Note that all these macros must be both l-values and r-values.

On any run of ACE, there will be some limit on how many cosets the CT can 
contain.  This will depend on the address/data memory model, how address/size 
arithmetic is performed, the size of Entry, the number of columns in the table, 
how much physical memory is available, the workspace size argument, rounding 
effects, and any guard bands.  Also, coset numbering starts from 1, so row 0 is 
not used.  An Entry in the table can count up to 2^(x-1)-1, where x is the 
number of bits allocated.  If the CT can contain more cosets than Entry can 
count, then an Entry value may become 'invalid' (it'll exceed its bitfield width
or rollover to a -ve value).  ACE cannot detect this possibility before it 
occurs, nor can it recover from it; the usual result is a "Segmentation fault".
So, all other things being equal, Entry (& thus Coset) should be large enough to
avoid this sort of problem.  However, if memory is limited and there are large 
numbers of group generators (ie, many table columns), then it may make sense to 
use 24 or even 16 bit Entry types.  This limits the number of cosets possible 
(to 8388607 or 32767 resp.), but it may allow the enumeration to fit in the 
available memory.

The "#ifdef ... #endif" wraps up the most 'reasonable' memory models and allows
one to picked by a compiler flag (eg, "gcc -DB8S4C8 ...").  There is no default,
and you must explicitly select your model.  Some test runs with, eg, test3/5/7
should help you make your choice.  Note that the use of packed structs minimises
the memory usage but incurs a time penalty (see test7.c).  If there's plenty of
memory then use the next size 'standard integer', since it'll likely be faster.
*******************************************************************************/

#if defined (B8S8C8)                   /* 64/64/64 bits */

typedef int64_t BInt;
#define PB PRId64
typedef int64_t SInt;
#define PS PRId64

typedef int64_t Coset;
#define PC PRId64
typedef Coset Entry;

#define CT(i,j)  (*(colptr[(j)] + (i)))
#define COL1(i)  (*(col1ptr + (i)))
#define COL2(i)  (*(col2ptr + (i)))

#elif defined (B8S4C8)                 /* 64/32/64 bits */

typedef int64_t BInt;
#define PB PRId64
typedef int32_t SInt;
#define PS PRId32

typedef int64_t Coset;
#define PC PRId64
typedef Coset Entry;

#define CT(i,j)  (*(colptr[(j)] + (i)))
#define COL1(i)  (*(col1ptr + (i)))
#define COL2(i)  (*(col2ptr + (i)))

#elif defined (B8S4C5)                 /* 64/32/40 bits */

typedef int64_t BInt;
#define PB PRId64
typedef int32_t SInt;
#define PS PRId32

typedef int64_t Coset;
#define PC PRId64 
struct dummy { Coset a:40; } __attribute__((gcc_struct,packed));
typedef struct dummy Entry; 

#define CT(i,j)  ((colptr[(j)] + (i))->a)
#define COL1(i)  ((col1ptr + (i))->a)
#define COL2(i)  ((col2ptr + (i))->a)

#elif defined (B8S4C4)                 /* 64/32/32 bits */

typedef int64_t BInt;
#define PB PRId64
typedef int32_t SInt;
#define PS PRId32

typedef int32_t Coset;
#define PC PRId32
typedef Coset Entry;

#define CT(i,j)  (*(colptr[(j)] + (i)))
#define COL1(i)  (*(col1ptr + (i)))
#define COL2(i)  (*(col2ptr + (i)))

#elif defined (B4S4C4)                 /* 32/32/32 bits */

typedef int32_t BInt;
#define PB PRId32
typedef int32_t SInt;
#define PS PRId32

typedef int32_t Coset;
#define PC PRId32
typedef Coset Entry;

#define CT(i,j)  (*(colptr[(j)] + (i)))
#define COL1(i)  (*(col1ptr + (i)))
#define COL2(i)  (*(col2ptr + (i)))

#elif defined (B4S4C3)                 /* 32/32/24 bits */

typedef int32_t BInt;
#define PB PRId32
typedef int32_t SInt;
#define PS PRId32

typedef int32_t Coset;
#define PC PRId32
struct dummy { Coset a:24; } __attribute__((gcc_struct,packed));
typedef struct dummy Entry; 

#define CT(i,j)  ((colptr[(j)] + (i))->a)
#define COL1(i)  ((col1ptr + (i))->a)
#define COL2(i)  ((col2ptr + (i))->a)

#else

#error You must define a BxSxCx memory model

#endif

/*******************************************************************************
begintime & endtime are the values of clock() (approx user time) at the start &
end of the current interval.  deltatime is the duration of the current interval,
while totaltime is the cumulative clock() time, for the current call.

The ETINT macro is used to end the current timing interval.  It updates the 
cumulative (user) time for this run & the time of the current interval (ie, 
since the last begintime).  It must be paired with a cmd to set the start of 
the next interval.  The new begintime is the old endtime (from ETINT), so our 
timings will include the time between the (start of the) ETINT and the 
subsequent setting of begintime into the next timing intrval.
*******************************************************************************/

extern double begintime, endtime;
extern double deltatime, totaltime;

#define ETINT  \
  endtime    = al0_clock();                 \
  deltatime  = al0_diff(begintime,endtime); \
  totaltime += deltatime;

/*****************************************************************************
Variables to control the (progress-based) messaging feature.  Such messages 
are enabled if msgctrl is set, and are printed every msgincr `actions'; where 
an action is a definition, a coincidence or a stacked deduction.  msgnext 
keeps track of how far away we are from the next message.  Values of 1 for 
msgctrl are ok, if you want to see everything that happens.  However, *lots* 
of output can be produced for such small values.
*****************************************************************************/

extern Logic msgctrl;
extern SInt  msgincr, msgnext;

/*****************************************************************************
Logic control variables for current enumeration:
mendel  If true, in R-style scan (& close) each relator at each cyclic 
        position for each coset, instead of just from 1st position
rfill   If true, fill rows after scanning
pcomp   If true, compress coinc paths
*****************************************************************************/

extern Logic mendel, rfill, pcomp;

/*****************************************************************************
The major user-settable parameters:
maxrow   Max number of cosets permitted.  May be less than the actual physical
         table size allocated, but not more.  Should be at least 2.
rfactor  R-style `blocking factor'
cfactor  C-style `blocking factor'
comppc   As new cosets are required, they are defined sequentially until the 
         table is exhausted, when compaction may be done.  Comppc sets the 
         percentage of dead cosets in the table before compaction is allowed.
nrinsgp  No. of relators `in' subgroup, for C-style enumerations.
lahead   If 0, don't do lookahead. If 1 (or 3), allow (cheap, R-style) 
         lookahead from current position (or over entire table).  If 2 (or 4),
         allow (expensive, C-style) lookahead over the entire table, a la ACE2
         (or from current position).  Note that, if mendel set, then cheap is 
         expensive.  Lookahead is one-level, ie, we don't look at consequences 
         of consequences or stack deductions.
*****************************************************************************/

extern Coset maxrow;
extern SInt  rfactor, cfactor;
extern SInt  comppc, nrinsgp, lahead;

/*****************************************************************************
Level 0 keeps track of the number of passes through the state-machine's main 
loop in lcount.  If llimit > 0, then the enumerator will exit after (at most) 
llimit passes.  You need to use this in conjunction with the machine's flow 
chart, else the results might surprise (annoy, frustrate) you.
*****************************************************************************/

extern SInt llimit, lcount;

/*****************************************************************************
The numbers of: current active cosets; maximum number of cosets active at any 
time; total number of cosets defined.
*****************************************************************************/

extern Coset nalive, maxcos;
extern BInt  totcos;

/*****************************************************************************
ctail (chead) is the tail (head) of the coincidence queue; we add at tail & 
remove at head.  During coincidence processing CT(high,2) (aka COL2(high)) is 
used to link the coincidence queue together.  CT(high,1) (aka COL1(high)) 
contains minus the equivalent (lower numbered) coset (the minus sign flags a 
`redundant' coset).  The queue is empty if chead = 0.  Primary coincidence are
always processed immediately, and processing continues until *all* secondary 
coincidences have been resolved.  We *may* discard deductions in coincidence 
processing, but never coincidences.  The only place where coincidences could 
be `discarded' is in table compaction; however, this is never called when the 
queue is non-empty, ie, during coincidence processing.
*****************************************************************************/

extern Coset chead, ctail;

/*******************************************************************************
If pdefn = T, then gaps of length 1 found during relator scans in C-style are 
preferentially filled (subject to the fill-factor, discussed below).  The gaps
are noted in the preferred definition list (or queue, the pdq).  Provided a live
such gap survives (and no coincidence occurs, which causes the pdq to be 
discarded) the next coset will be defined to fill the oldest gap of length 1.
 
On certain examples, eg, F(2,7), this can cause infinite looping unless CT 
filling is guaranteed.  This can be ensured by insisting that at least some 
constant proportion of the coset table is always kept filled & `tested'.  This
is done using ffactor.  Before defining a coset to fill a gap of length 1, the
enumerator checks whether ffactor*knh is at least nextdf and, if not, fills 
rows in standard order.  A good default value for ffactor (set by Level 1) is 
int((5(ncol+2))/4).  We'd `normally' expect that nextdf/knh ~= ncol+1 
(ignoring coincidences, which confuse things), so the default value of ffactor
`encourages' this ratio to grow a little.  Warning: using a ffactor with a 
large absolute value can cause infinite looping.  However, in general, a 
`large' positive value for ffactor works well.

Note: tests indicate that the effects of ffactor vary widely.  It is not clear
which value is a good general default or, indeed, whether any value is 
*always* `not too bad'.
*******************************************************************************/

extern Logic pdefn;
extern float ffactor;
 
/*****************************************************************************
The preferred definition queue (pdq) is implemented as a ring, dropping oldest
entries (see Havas, "Coset enumeration strategies", ISSAC'91).  It's size must
be at least 2.  The row/col arrays store the coset number/generator values.  
Entries are added at botpd and removed at toppd.  The list is empty if botpd =
toppd; so, in fact, the list can store only pdsiz-1 values.
*****************************************************************************/

extern SInt   pdsiz;
extern SInt  *pdqcol;
extern Coset *pdqrow;
extern SInt   toppd, botpd;

#define NEXTPD(i)  if (++i >= pdsiz)  { i = 0; }

/*****************************************************************************
The deduction list is organised as a stack.  Deductions may be discarded, and 
discards are flagged by disded, as they may impact the validity of a finite 
index.  (If deductions are not processed, under some circumstances the result 
may be a multiple of the actual index.)  We only `log' discards if we try to 
stack them & can't (ie, stack full) or if they're `potentially' meaningful 
(eg, if we *know* the table has collapsed, and index=1, then stacked 
deductions are *not* meaningful).  The stack is empty if topded=-1, and dedsiz
is the available stack space.

Note that if we define N.g = M, and thus M.G = N, we only stack N/g.  When we 
unstack, we test both N/g (picking up M) & M/G.  We ignore cosets that have 
become redundant, but we do nothing (too expensive) about duplicates (either 
direct or inverted); these will scan fast however, since `nothing' happens.

We test various stack-handling options by the dedmode parameter.  0 means do 
nothing (except discard individually if no space), 1 means purge redundancies 
off the top (on exiting _coinc()), 2 means compact out all redundancies (on 
exiting _coinc()), and 3 means throw away the entire stack if it overflows.  
Mode 4 is a fancy mode; every time the stack overflows we call a function to
`process' it, on the basis that we're prepared to work *very* hard not to 
throw anything away.  The particular function used is subject to review; 
currently, we expand the space available for the stack and move the old stack,
compressing it as it's moved by removing redundancies.  In practice this works
very well, and is the default dedn handling method; it means that we always 
process all deductions.  In the presence of (big) collapses, a judicious 
choice of dedsize & the use of Mode 0 will often be faster however (even 
allowing for the RA phase if any dedns are discarded).

Discussion: dedsiz is usually some `small' value, as the active stack is 
normally small and shrinks back to empty rapidly.  Since the enumerator is 
`clever' enough to `notice' dropped/unprocessed deductions & take appropriate 
action when checking a finite result, it makes sense in some circumstances to 
drop excessive deductions.  In particular, if we have a lot of coincidences in
al0_coinc, and thus a big stack (esp. one that doesn't shrink quickly), it is 
much faster to ignore these and tidy up at the end (since most of the stack is
redundant, duplicate or yields no info).  This is somewhat similar to the 
adaptive flag of ACE1/2.  (An alternative option would be to do a C-lookahead 
at the top of _cdefn() if ever dedns have been discarded, but this could be 
very expensive.)
*****************************************************************************/

extern SInt   dedsiz;
extern Coset *dedrow;
extern SInt  *dedcol, topded, dedmode;
extern Logic  disded;

/*******************************************************************************
Macro to save a deduction on the stack.  Note the function call in mode 4, if 
the stack overflows; this can be v. expensive if the stack overflows repeatedly 
(ie, a big collapse).  It's a question of which is faster; trying hard *not* to 
discard deductions, or discarding them & having to run a checking phase at the 
end of an enumeration.  In practice, _dedn() doubles the stack space at each 
call, so it's not actually called very often.  It can however chew up lots of
memory.
*******************************************************************************/

#define SAVED(cos,gen)    \
  if (topded >= dedsiz-1) \
    {                     \
    switch(dedmode)       \
      {                   \
      case 3:   disded = TRUE;  topded = -1;  break; \
      case 4:   al0_dedn(cos,gen);            break; \
      default:  disded = TRUE;                break; \
      } \
    }   \
  else  \
    { dedrow[++topded] = cos;  dedcol[topded] = gen; }

/*****************************************************************************
We note where generators occur in bases of relators, so that definitions can 
be applied at all essentially different positions (edp) in C-style definitions
or in C-style lookahead.  edpbeg[g] indexes array edp[], giving the first of 
the edps in all (noninvolutory) relators for that generator.  The edp array 
stores pairs: the index in array relators where this generator occurs; the
length of the relator.  edpend[g] indexes the last edp pair for generator g.  
If there are no such positions edpbeg[g] < 0.  Generators are in terms of 
column numbers, so noninvolutory generators have two sets of entries.  
Generators which are to be *treated* as involutions have only one column & one
set of entries.  The edp of a relator xx (or x^2, or XX, or X^2) where x is to
be treated as an involution is *not* stored, since it yields no information in
a C-style scan.
*****************************************************************************/

extern SInt *edp, *edpbeg, *edpend;

/*****************************************************************************
Group generators (aka coset table columns):
ncol is the number of columns in CT.  Involutions (usually) use only 1 column,
noninvolutary generators 2.  colptr is the array of pointers to CT columns,
while col1ptr/col2ptr are special pointers (aliases) for cols 1 & 2.  invcol
is a table mapping columns to their inverse columns, length ncol+1.  Note that
cols are numbered 1..ncol.
*****************************************************************************/

extern SInt    ncol;
extern Entry **colptr;
extern Entry  *col1ptr, *col2ptr;
extern SInt   *invcol;

/*****************************************************************************
Group relators: 
ndrel     Number of relators
relind    relind[i] is the start position of ith relator in array relators[]
relexp    relexp[i] is exponent (ith rel'r)
rellen    rellen[i] is total length (ith rel'r)
relators  The relators, fully expanded and duplicated for efficient scanning
*****************************************************************************/

extern SInt  ndrel;
extern SInt *relind, *relexp, *rellen;
extern SInt *relators;

/*****************************************************************************
Subgroup generators: 
nsgpg       Number of subgroup generators
subggen     All the subgroup generators
subgindex   Start index of each generator in subggen[]
subglength  Length of each generator
sgdone      have the subgroup generators been applied to coset #1 or not
*****************************************************************************/

extern SInt  nsgpg;
extern SInt *subggen, *subgindex, *subglength;
extern Logic sgdone;

/*****************************************************************************
knr is the coset at which an R-style scanning against relators is to commence;
all previous (active) cosets trace complete cycles at all relators.  If knr ==
nextdf *and* the table is hole-free, then a valid index has been obtained.  
knh is the coset at which a search for an undefined coset table entry is to 
begin; all previous cosets have all entries in their row defined.  If C-style
definitions are being (or will be) made, all previous cosets have all entries 
in their row defined, and all consequences traced or definitions stacked.  (In
fact, *all* non-zero entries in the table have been traced or stacked.)  If 
knh == nextdf *and* *all* definitions have had their consequences processed, 
then a valid index has been obtained.  Note that currently knh is only changed
in C-style, since it is important that the property referred to above is 
preserved.  This effectively overloads the meaning of knh; it should be 
replaced by separately maintained knh & knc variables.  This would make R-
style & C-style symmetric; much nicer.  It would also allow us to 
differentiate between the definition strategy, the scanning strategy, and the 
termination condition.

nextdf is the next sequentially available coset.  Normally 1 <= knr < nextdf 
and 1 <= knh < nextdf; the value of knr vis-a-vis knh is not fixed.  If knr /
knh hit nextdf, then we're done (modulo some other conditions).  Note that 1 
<= nalive < nextdf <= maxrow+1.  If nextdf = maxrow+1 and we want to define a 
new coset, then we've overflowed; we lookahead/compact/abort.
*****************************************************************************/

extern Coset knr, knh, nextdf;

/*****************************************************************************
Externally visible functions defined in enum.c.  Note that it is not strictly 
necessary to make _apply() visible across files, since it's only called from 
within enum.c, but we do anyway, since a Level 0 user might like to use it.
TBA ... any others like that?
*****************************************************************************/

Coset al0_apply(Coset, SInt*, SInt*, Logic, Logic);
Coset al0_enum(SInt, SInt); 

/*****************************************************************************
Externally visible functions defined in coinc.c
*****************************************************************************/

Coset al0_coinc(Coset, Coset, Logic);

/*****************************************************************************
Externally visible functions defined in util0.c
*****************************************************************************/

double al0_clock(void);
double al0_diff(double, double);
void   al0_init(void);
Logic  al0_compact(void);
Logic  al0_stdct(void);
void   al0_upknh(void);
void   al0_dedn(Coset, SInt); 
void   al0_rslt(Coset);

