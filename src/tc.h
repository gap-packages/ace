
/**************************************************************************

	tc.h
	Colin Ramsay
	17 Jun 98

	ADAPTIVE COSET ENUMERATOR, Version 2.000

	Copyright 1998 
	Centre for Discrete Mathematics and Computing,
	Department of Mathematics and 
	Department of Computer Science & Electrical Engineering,
	The University of Queensland, QLD 4072.

This is the header file for the coset enumerator.  Rewritten by Colin 
Ramsay, based on code by George Havas et al.

**************************************************************************/

	/******************************************************************
	Stdio.h and stdlib.h will be included in all the source files,
	since all of them include this file.
	******************************************************************/

#include <stdio.h>
#include <stdlib.h>

	/******************************************************************
	At some time - say when we want run ACE on a 64-bit machine - we
	will want to be careful as to the types we use.  For the moment we
	only typedef logical variables, and stick with standard C types for
	the rest.  The `default' environment is 32-bit Unix.
	******************************************************************/

typedef int Logic;
#define TRUE   1
#define FALSE  0

	/******************************************************************
	DITRACE((...)) expands into ditrace(...) if we want trace /
	debugging info.  Warning - will generate lots of o/p.
	******************************************************************/

#ifdef __DI
#  define DITRACE(ARGS)  ditrace ARGS 
#else
#  define DITRACE(ARGS)
#endif

#define MAX0(x,y)  (((x) > (y)) ? (x) : (y))
#define MIN0(x,y)  (((x) < (y)) ? (x) : (y))
#define SWAP(i,j)  { int swapp;  swapp=i;  i=j;  j=swapp; }

	/******************************************************************
	Macro for access to coset table.  i is coset, j is generator (as
	column number).  Colptr[j] stores the start address of the block of
	memory for column i.  CT(i,j), with j = 1...ncol, indicates the
	action of the associated generator (or inverse) of column j on 
	coset i.  It contains the coset number if known, otherwise 0 (-ve 
	numbers indicate coincidences, in column 1).  Coset 1 is the 
	subgroup. 

	Depending on the address/data memory model, how address arithemtic 
	is performed, the size of an int, and the number of columns, there 
	will be some limit on how many rows the table can have.
	******************************************************************/

#define CT(i,j)  (*(colptr[(j)] + (i)))

	/******************************************************************
	The memory for the coset table is currently allocated as one
	contiguous block.  DEFWORK is the default number of words (i.e., 
	entries) in toto.  The size can be (re)set via a size/multiplier
	combination (workspace/workmult).  The usual K/M/G multipliers are
	used, with a choice of meanings to suit computer scientists, or 
	engineers, or mathematicians.  The default (under pressure) is to
	use powers of 10.  Negative, or zero, workspace size selects 
	DEFWORK, while values <1K are rounded up to 1K.  While coset 
	numbers are limited to 2G, table sizes can exceed the 32-bit limit.
	******************************************************************/

#define DEFWORK  1000000

#ifdef BINARY
#  define KILO  1024
#  define MEGA  1048576
#  define GIGA  1073741824
#else
#  define KILO  1000
#  define MEGA  1000000
#  define GIGA  1000000000
#endif

#define LLL 75			/* Approx limit on output line length */

	/******************************************************************
	Some of ACE's parameters are stored in 3-int arrays; the SET3 macro
	is a convenient method for setting all 3 elements.  The SET2 macro 
	is for quickly updating the 2-char status string.
	******************************************************************/

#define SET3(A,x,y,z)  { A[0] = x;  A[1] = y;  A[2] = z; }
#define SET2(A,x,y)    { A[0] = x;  A[1] = y; }

	/******************************************************************
	Deduction stack macro (a stack is used for convenience).  The stack
	is stored in the unused positions at the top of CT(), and grows 
	down.  topded indexes the top of a stack of pairs 'row' & 'col' in 
	CT(topded, 1 & 2).  Note that deductions may be discarded if space
	is not available.  This is space-saving, but could lose lots!
	******************************************************************/

#define SAVEDED(row,col)   \
  if (topded <= nextdf)    \
    { disded++; }          \
  else                     \
    {                      \
    CT(--topded, 1) = row; \
    CT(topded, 2) = col;   \
    }

	/******************************************************************
	Various parameters to ACE are lists of integers.  This is the type
	used to process them in the parser.  The handling of this in the
	parser is a little bit silly - fix up some time.
	******************************************************************/

typedef struct 
  {
  int icount;			/* number of data items */
  int integers[64];		/* the data */
  }
Intarray;    

	/******************************************************************
	The group relations and subgroup generators are stored as linked
	lists.  Each item on the list consists of an array of generators 
	(i.e., the word), along with its length and exponent.  In the 
	words, -ve numbers represent inverses.  The word starts at
	wordgen[1].  Each list has a header containing the list's length 
	and head/tail pointers.
	******************************************************************/

typedef struct Wordlnode
  {
  int *wordgen;
  int len, exp;
  struct Wordlnode *next; 
  }
Wordlel;  			/* word list element */ 

typedef Wordlel *Wordlptr; 	/* word list pointer */

typedef struct
  {
  int length;
  Wordlptr first, last;
  } 
*Wordlhdr;			/* word list header */

	/******************************************************************
	The remainder of this header file lists the global variables 
	visible across all files.
	******************************************************************/

	/******************************************************************
	The (coset table) workspace - size (in words) & pointer thereto.
	******************************************************************/

extern int workspace, workmult, *cosettable;

extern FILE *fop, *fip;		/* All (normal) i/o goes through these */

extern char *grpname;		/* Enumeration (i.e., group) name */
extern char *subgrpname;	/* Subgroup name */

extern char status[3];		/* Enumeration status, 2 letter string */

extern double begintime;	/* Clock() at start of this interval */
extern double endtime;		/* Clock() at end of this interval */
extern double totaltime;	/* Cumulative clock() time */

	/******************************************************************
	Logic control variables for current enumeration.
	******************************************************************/

extern Logic asis;		/* TRUE: use presentation as given.  FALSE:
				reduce/reorder relations/generators. */
extern Logic adapt;		/* ? enumeration is adaptive */
extern Logic restart;		/* ? `resume' current enumeration */
extern Logic rtenum;		/* ? RT-style enumeration */
extern Logic mendel;		/* ? use cyclically DPs instead of EDPs */

	/******************************************************************
	Logic status variables for current enumeration.
	******************************************************************/

extern Logic index1;		/* ? index = 1.  Initially FALSE.  If in
			tc_coinc_cols12() it is detected that nalive has 
			fallen to 1 and all entries in coset 1 are defined,
			then index1 is set to TRUE and we know that the 
			subgroup has index 1. */
extern Logic overfl;		/* Space overflow flag.  Note that there 
			are two types of overflow:  we cannot define new 
			cosets as there are no more cosets logically 
			available (we reach `maxrow'); there are no more 
			cosets physically available (we reach the top of 
			deduction stack) and reusable space is smaller 
			than the compaction percentage. */
extern Logic indexc;		/* flag to signal CT complete */
extern Logic compct;		/* TRUE: coset table is known to have no 
				unused rows in it.  FALSE: otherwise. */
extern Logic mstflg;		/* TRUE: minimum spanning tree exists for 
				current coset table.  FALSE: otherwise. */
extern Logic ctflg;		/* TRUE: there is a CT.  FALSE: isn't. */

	/******************************************************************
	The `entry point' for an enumeration is tc_start().  This has a set
	of nested loops which loop over the actual enumerator tc_step2()
	for various (user settable) parameter combinations.  The loops are
	controlled by 3 element arrays.  These, and the parameters they
	control (if global), are as follows (same order as the loops):
	******************************************************************/

extern int maxpar[3], cosetlim, maxrow;	/* Max allowed rows in table. */
extern int rinsgpar[3], nrinsgp;	/* No. of relators in subgroup, for
					CT-style enumeration. */
extern int fipar[3], fillfactor;	/* If fillfactor is positive then 
					gaps of length 1 found during 
	relator scans are noted in the preferred definition list (pdl, an 
	overwriting ring data structure).  Provided a live such gap 
	survives (and no coincidence occurs, which causes the pdl to be 
	discarded) the next coset will be defined to fill the gap of 
	length 1 which is most recent (?).  On certain examples, e.g., 
	F(2,7), this can cause infinite looping unless CT filling is 
	guaranteed.  Such a guarantee can be ensured by insisting that at 
	least some constant proportion of the coset table is always kept 
	filled.  This is implemented via the parameter fillfactor.  Thus, 
	before defining a coset to fill a gap of length 1, the enumerator 
	checks whether fillfactor*knc is at least nalive and, if not, fills
	rows in standard order.  A default value of int((5(ncol+2))/4)) 
	for fillfactor is used, but the user may specify it.  If 
	fillfactor=1, preferred definitions are not made. Warning: using a 
	fillfactor with a large absolute value can cause infinite looping. 
	However, in general, a large positive value for fillfactor works 
					well. */
extern int ctpar[3], ctfactor;		/* See doco */
extern int rtpar[3], rtfactor;		/* ditto */

	/******************************************************************
	Various message control variables.  Msgctrl is set by the user to
	the `interval' between messages; if -ve, it means to dump lots more
	info.  Msgincr is the `actual (i.e., absolute) value of msgctrl.
	On integer multiples of msgincr (= abs(msgctrl)) cosets defined or
	deleted by the coincidence routine a message is  printed.  To avoid
	repeated divisions in defining or deleting loops, nextout is the 
	next nalive at which this message is to be printed.  Lastout is 
	used to avoid excessive printing if nalive oscillates around a 
	multiple of msgincr.  Lastout is the last value of nalive when the 
	message was printed.  Messages are not printed when nalive is the 
	same as lastout.  Note: a value of +/-1 for msgctrl is ok, however
	not all definitions/deletions will be `captured'.  Warning: small
	values can generate lots of output.
	******************************************************************/

extern int msgctrl, msgincr, lastout, nextout;

extern int prpar[3];			/* Coset table o/p parameters */

	/******************************************************************
	For CT style enumeration, we note where generators occur in bases 
	of relators, so that definitions can be applied at all essentially 
	different positions (edp).  Edpbeg[g] indexes array edp[], giving 
	the first of the edps in all (noninvolutory) relators for that 
	generator.  The edp array stores pairs: the index in array relators	
	where this generator occurs; the length of the relator.  Edpend[g] 
	indexes the last edp pair for generator g.  If there are no such 
	positions edpend[g] < edpbeg[g].
	******************************************************************/

extern int *edp, *edpbeg, *edpend;

	/******************************************************************
	The numbers of: current active cosets; maximum number of cosets 
	active at any time; total number of cosets defined.
	******************************************************************/

extern int nalive, maxcos, totcos;

	/******************************************************************
	Group generators (aka coset table columns): 
	******************************************************************/

extern int ngennum;		/* Number of numeric generators */
extern int ngenal;		/* No. of alphabetic generators */
extern int ndgen;		/* Number of group generators */
extern char algen[28];		/* The alphabetic generators; [0] and [27] 
				are '\0' */
extern int *geninv;		/* -1 indicates generator is involutory, 
				else 0. */
extern int ncol;		/* Number of columns in CT. Involutions 
				(usually) use only 1 column, noninvolutary
				generators 2. */
extern int **colptr;		/* Array of pointers to CT columns */
extern int *gencol;		/* Table mapping +/- generator numbers to 
			columns.  Gencol points to the middle of the 
			column translation table for each generator, so 
			that gencol[g] gives the column in CT of generator 
			g, and gencol[-g] gives the column of the inverse 
			of generator g.  This table is of length 2*ndgen+1.
				Gencol[0] is unused. */
extern int *colgen;		/* Table mapping columns to +/- generator
				numbers, length ncol. */
extern int *invcol;		/* Table mapping columns to their inverse 
				columns, length ncol. */

	/******************************************************************
	Group relators: 
	******************************************************************/

extern int ndrel;		/* Number of relators */
extern Wordlhdr relhdr;		/* Group relators list */

extern int *relind;		/* Relind[i] is the start position of ith 
				relator in array relators[]. */
extern int *relexp;   		/* Relexp[i] is exponent ?! */
extern int *rellen;   		/* Rellen[i] is real length of relator i */
extern int *relators; 		/* The group defining relators fully 
			expanded and duplicated for efficient scanning. */
extern int trellen;		/* Total length of relators */
extern int newrel;         	/* Number of new relators */
extern int newrelpar; 		/* ?Interaction newrel/newrelpar */

	/******************************************************************
	Subgroup generators: 
	******************************************************************/

extern int nsgpg;		/* Number of subgroup generators */
extern Wordlhdr sghdr;		/* Subgroup generators list */
extern int *subggen;        	/* Number of subgroup gens */
extern int *subgindex;
extern int *subglength;
extern int tsgenlen;		/* Total length of subg gens */
extern int newsg;		/* Number of new subgroup generators */
extern int newsgpar; 		/* ?Interaction newsg/newsgpar */

	/******************************************************************
	Knc is the coset at which a CTstyle search for an undefined coset 
	table entry is to begin; all previous cosets should have all 
	entries in their row defined.  Knr is the coset at which an RTstyle
	search for an undefined coset table entry is to begin; all previous
	cosets should trace complete cycles at all relators.  Nextdf is the
	next sequentially available coset, while it's not greater than the 
	top of the deduction stack.  Initially, as new cosets are required,
	they are defined sequentially until CT is exhausted, then 
	lookahead/compaction may be done.  Comppc sets the percentage of 
	dead cosets in the table before compaction is performed.
	******************************************************************/

extern int knc, knr, nextdf, comppc;

extern int mstlevel;		/* Levels in minimal spanning tree. */
extern int *mst;		/* The MST. (Change to 2 arrays?) */

	/******************************************************************
	Coset representative in index (i.e., column) terms.  This should
	not really be a global since there's only ever a single rep. All 
	usage is in postproc.c, except for initialisation in aceutils.c.
	******************************************************************/

extern int *crepx;

extern int topded;		/* Index of top of the deduction stack */
extern int disded;		/* Number of discarded deductions */

	/******************************************************************
	Various kludges, for debugging purposes
	******************************************************************/

#define DUMP(str) \
  fflush(fop); \
  fprintf(fop, "DUMP: %s\n", str); \
  fflush(fop);

#define DUMP1(str) \
  fflush(fop); \
  fprintf(fop, "DUMP3, %s: ", str); \
  fprintf(fop, "nalive=%d, knc=%d, knr=%d, nextdf=%d, topded=%d\n", \
          nalive, knc, knr, nextdf, topded); \
  fflush(fop);


